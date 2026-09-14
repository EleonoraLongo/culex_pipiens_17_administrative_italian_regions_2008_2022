# =============================================================================
# Harmonised dataset of Culex pipiens, Italy (2008–2022)
# =============================================================================
# Author     : Eleonora Longo
#              PhD Student in Agrifood and Environmental Science
#              Center of Agriculture, Food and Environment (C3A)
#              University of Trento
#              Via Edmund Mach, 1 - 38098 San Michele all'Adige (TN)
#              eleonora.longo@unitn.it
#
# Purpose    : Restricts the national database to Culex pipiens, 2008-2022,
#              and produces a spatio-temporally aggregated dataset (weekly
#              catch per ERA5-Land grid cell) for an integrated Species
#              Distribution Model.
#
# Reference  : Hijmans, R.J., et al. (2005). Very high resolution interpolated
#              climate surfaces for global land areas. Int. J. Climatol., 25:
#              1965-1978. (Methodological justification for the coastal
#              gap-fill applied once, upstream, in 1_2_reference_grid_final.R.)
#              Leys, C., et al. (2013). Detecting outliers: Do not use
#              standard deviation around the mean, use absolute deviation
#              around the median. J. Exp. Soc. Psychol., 49(4): 764-766.
#              (Rationale for the median/IQR summary retained here, and for
#              the MAD-based spatial-outlier check already performed
#              upstream at the national-merge stage.)
#
# Input      : ../main_db/db_samplings_clean.csv (Stage-2 output, READ ONLY)
#              ../other_data/reference_grid/italy/grid_cellid_italy.tif
#              ../other_data/reference_grid/italy/grid_provenance_italy.tif
#              (both built by 1_2_reference_grid_final.R -- run that first)
#              ../other_data/comuni.geojson
#
# Output     : ../main_db/db_aggregated.csv
#              ../main_db/qc_exclusions_log_aggregation_stage.csv (NEW,
#              exclusively owned by this script)
#              ../main_db/qc_summary_report_aggregation.csv (NEW,
#              reviewer-facing)
#
# Sections   :
#    0. Libraries & Settings
#    1. Data Loading & Harmonisation (species/period restriction, logged)
#    2. Data Quality Checks [Full Database]
#       2.1. Coordinate check & missing-coordinate imputation (defensive)
#       2.2. Harmonise factor levels & fix missing weeks (logged)
#    3. Spatio-Temporal Aggregation (grid-based cell assignment)
#    4. Diagnostics & Temporal Checks
#    5. Final Data Structure & Levels Check (hard gates)
#    6. QC Summary Report
#    6b. Visual checks
#    7. Save Final Output
# =============================================================================

rm(list = ls())

#### 0. LIBRARIES & SETTINGS ####
library(dplyr)
library(tidyverse)
library(terra)
library(sf)
library(stringr)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# STUDY_YEAR_MAX
STUDY_YEAR_MIN <- 2008
STUDY_YEAR_MAX <- 2022

# qc
qc_agg_log_path <- "../main_db/qc_exclusions_log_aggregation_stage.csv"
qc_agg_log <- list()  # accumulates one tibble per check below

GRID_CELLID_ITALY_FILE     <- "../other_data/reference_grid/italy/grid_cellid_italy.tif"
GRID_PROVENANCE_ITALY_FILE <- "../other_data/reference_grid/italy/grid_provenance_italy.tif"
stopifnot(
  "grid_cellid_italy.tif not found -- run 1_2_reference_grid_final.R first" =
    file.exists(GRID_CELLID_ITALY_FILE),
  "grid_provenance_italy.tif not found -- run 1_2_reference_grid_final.R first" =
    file.exists(GRID_PROVENANCE_ITALY_FILE)
)


#### 1. DATA LOADING & HARMONISATION ####
##### culex pipiens db #####
culex_raw <- read.csv("../main_db/db_samplings_clean.csv", sep = ",")
n_input <- nrow(culex_raw)

# Restrict to the target species.
off_target_species <- culex_raw %>%
  dplyr::filter(is.na(species) | species != "pipiens") %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "off_target_species",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = as.character(value),
    reason = sprintf("Species is '%s', not the target 'pipiens' -- out of scope for this dataset, not a data-quality issue.",
                     dplyr::coalesce(as.character(species), "NA"))
  )
qc_agg_log[["off_target_species"]] <- off_target_species

culex <- culex_raw %>% dplyr::filter(!is.na(species) & species == "pipiens")

# Restrict to the study period.
out_of_period <- culex %>%
  dplyr::filter(is.na(year) | year < STUDY_YEAR_MIN | year > STUDY_YEAR_MAX) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "year_out_of_study_period",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = as.character(year),
    reason = sprintf("Year %s falls outside the public-release study period (%d-%d).",
                     dplyr::coalesce(as.character(year), "NA"), STUDY_YEAR_MIN, STUDY_YEAR_MAX)
  )
qc_agg_log[["year_out_of_period"]] <- out_of_period

culex <- culex %>% dplyr::filter(!is.na(year) & year >= STUDY_YEAR_MIN & year <= STUDY_YEAR_MAX)

n_input
nrow(off_target_species)
nrow(out_of_period)
nrow(culex)


##### reference grid (Italy) #####
# Built once, upstream, by 1_2_reference_grid_final.R -- see the header
# note on why this script no longer loads the raw ERA5 .nc file at all.
cellid_italy_r     <- terra::rast(GRID_CELLID_ITALY_FILE)
provenance_italy_r <- terra::rast(GRID_PROVENANCE_ITALY_FILE)  # 2 layers: filled, fill_dist_m

##### italy boundaries #####
# Municipalities polygons (for the diagnostic maps only)
ita_muni <- sf::st_read("../other_data/comuni.geojson") %>%
  sf::st_make_valid()


#### 2. DATA QUALITY CHECKS: MISSING COORDINATES IMPUTATION ####
# Calculate municipality centroids
muni_coords <- ita_muni %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    lon_centroid = sf::st_coordinates(.)[,1],
    lat_centroid = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    Municipality_clean = stringr::str_to_upper(stringr::str_trim(name)),
    Province_clean = stringr::str_to_upper(stringr::str_trim(prov_name))
  ) %>%
  dplyr::select(Municipality_clean, Province_clean, lon_centroid, lat_centroid)

# Add muni centroids to culex (Fixing translation mismatches)
culex_updated <- culex %>%
  dplyr::mutate(
    Municipality_match = stringr::str_to_upper(stringr::str_trim(Municipality)),
    Province_match = stringr::str_to_upper(stringr::str_trim(Province))
  ) %>%
  dplyr::mutate(
    Province_match = dplyr::case_when(
      Province_match == "SYRACUSE" ~ "SIRACUSA",
      Province_match == "PESARO AND URBINO" ~ "PESARO E URBINO",
      Province_match == "MILAN" ~ "MILANO",
      Province_match == "MANTUA" ~ "MANTOVA",
      Province_match %in% c("MONZA AND BRIANZA", "MONZA E BRIANZA") ~ "MONZA E DELLA BRIANZA",
      Municipality_match == "SERDIANA" ~ "SUD SARDEGNA",
      Municipality_match == "LOTZORAI" ~ "NUORO",
      TRUE ~ Province_match
    ),
    Municipality_match = dplyr::case_when(
      Municipality_match == "ZAMBANA" ~ "TERRE D'ADIGE",
      Municipality_match == "BIGARELLO" ~ "SAN GIORGIO BIGARELLO",
      TRUE ~ Municipality_match
    )
  ) %>%
  dplyr::mutate(.row_id = dplyr::row_number()) %>%
  dplyr::left_join(muni_coords,
                   by = c("Municipality_match" = "Municipality_clean",
                          "Province_match" = "Province_clean")) %>%
  dplyr::mutate(
    used_stage3_centroid = is.na(latitude) & !is.na(lat_centroid),
    stage3_centroid_note = ifelse(used_stage3_centroid,
                                  "Coordinates were unexpectedly missing at the aggregation stage (should already be complete after Stage 2); derived here from the municipality centroid as a defensive fallback.",
                                  NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(stage3_centroid_note) ~ paste(note, stage3_centroid_note, sep = " | "),
      is.na(note)  & !is.na(stage3_centroid_note) ~ stage3_centroid_note,
      TRUE ~ note
    ),
    latitude = dplyr::coalesce(latitude, lat_centroid),
    longitude = dplyr::coalesce(longitude, lon_centroid)
  )

# Log any Stage-3 imputation (expected to be empty; see note above)
stage3_centroid_log <- culex_updated %>%
  dplyr::filter(used_stage3_centroid) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "missing_coordinate_imputed_at_stage3",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = NA_character_,
    reason = "Coordinates were still missing after the Stage-2 national merge; imputed here from the municipality centroid as a defensive fallback. This is NOT expected to happen -- see Section 2 comments."
  )
qc_agg_log[["stage3_centroid_imputed"]] <- stage3_centroid_log

culex_updated <- culex_updated %>%
  dplyr::select(-lon_centroid, -lat_centroid, -Municipality_match, -Province_match,
                -used_stage3_centroid, -stage3_centroid_note, -.row_id)

# Verification
missing_coords <- culex_updated %>%
  dplyr::filter(is.na(latitude)) %>%
  dplyr::distinct(Municipality, Province)

nrow(stage3_centroid_log)
if (nrow(missing_coords) > 0) {
  cat("Municipality/Province combinations still unresolved after imputation:\n")
  print(missing_coords)
} else {
  cat("No Municipality/Province combinations remain unresolved.\n")
}


##### 2.1. check coords #####
out_of_bounds <- culex_updated %>%
  dplyr::filter(!dplyr::between(latitude, 35.0, 47.5) | !dplyr::between(longitude, 6.5, 18.5))

out_of_bounds_log <- out_of_bounds %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "coordinate_out_of_italy_bbox_at_stage3",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = paste0("lat=", latitude, ", lon=", longitude),
    reason = "Coordinates fall outside the Italy bounding box after the Stage-3 defensive re-check. This is NOT expected to happen -- see Section 2 comments."
  )
qc_agg_log[["out_of_bounds_stage3"]] <- out_of_bounds_log

nrow(out_of_bounds_log)

ggplot2::ggplot() +
  ggplot2::geom_sf(data = ita_muni, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = culex_updated, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 0.5, alpha = 0.4) +
  ggplot2::coord_sf(xlim = c(6.0, 19.0), ylim = c(35.0, 48.0)) +
  ggplot2::theme_bw()


##### 2.2. HARMONISE FACTOR LEVELS & FIX MISSING WEEKS #####
levels(as.factor(culex_updated$sex))
levels(as.factor(culex_updated$life_stage))
levels(as.factor(culex_updated$trap_type))

culex_updated <- culex_updated %>%
  dplyr::mutate(
    date = as.Date(date),
    week = dplyr::coalesce(as.integer(week), as.integer(lubridate::isoweek(date)))
  )

missing_week_log <- culex_updated %>%
  dplyr::filter(is.na(week)) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "missing_week_at_stage3",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = NA_character_,
    reason = "Epidemiological week could not be determined from the recorded value or re-derived from the sampling date."
  )
qc_agg_log[["missing_week"]] <- missing_week_log

culex_updated <- culex_updated %>%
  dplyr::filter(!is.na(week)) %>%
  dplyr::mutate(
    trap_type = stringr::str_to_upper(stringr::str_trim(trap_type)),
    trap_type = stringr::str_replace_all(trap_type, "-", "_"),
    trap_type = stringr::str_replace_all(trap_type, " ", "_")
  )

nrow(missing_week_log)

levels(as.factor(culex_updated$life_stage))
levels(as.factor(culex_updated$trap_type))


#### 3. SPATIO-TEMPORAL AGGREGATION (grid-based cell assignment) ####
# Filter db to remove remaining NAs ONLY after fixing them.
still_missing_coord_log <- culex_updated %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "missing_coordinate_final_drop",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = NA_character_,
    reason = "Coordinates remained missing after all Stage-3 imputation attempts; record could not be assigned to a grid cell and was dropped. This is NOT expected to happen -- see Section 2 comments."
  )
qc_agg_log[["missing_coordinate_final_drop"]] <- still_missing_coord_log

db <- culex_updated %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))

# Convert to sf object
db_sf <- sf::st_as_sf(db, coords = c("longitude", "latitude"), crs = 4326)

# Cell assignment: a single extraction against the pre-built Italy grid
# (see the file header for why this replaces the previous independent
# cellFromXY() + nearest-valid-cell coastal-snapping logic entirely).
# terra::extract() on a SpatVector prepends an ID column, unlike the
# raw-coordinate-matrix form used inside 1_2_reference_grid_final.R's own
# cross-checks -- selected here by name, not position, to avoid that
# exact class of indexing mistake.
extracted <- terra::extract(c(cellid_italy_r, provenance_italy_r), terra::vect(db_sf))

db_mapped <- db %>%
  dplyr::mutate(
    ID          = extracted$cell_id,
    grid_filled = as.logical(extracted$filled),
    grid_fill_dist_m = extracted$fill_dist_m
  )

# Points genuinely outside the Italy grid's domain (not just a raw sea
# cell
still_no_cell_log <- db_mapped %>%
  dplyr::filter(is.na(ID)) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "no_valid_grid_cell",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date), value_raw = NA_character_,
    reason = "No cell_id found in the Italy reference grid at this coordinate (outside the grid's domain, or beyond its coastal gap-fill reach); record excluded. See 1_2_reference_grid_final.R."
  )
qc_agg_log[["no_valid_grid_cell"]] <- still_no_cell_log

# Informational: records resolved via a gap-filled
# coastal cell rather than an originally observed ERA5-Land cell.
coastal_grid_filled_log <- db_mapped %>%
  dplyr::filter(!is.na(ID), grid_filled) %>%
  dplyr::transmute(
    region = "aggregation_stage", check = "assigned_to_gapfilled_grid_cell",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.character(date),
    value_raw = sprintf("%.0f m from nearest observed cell", grid_fill_dist_m),
    reason = "Coordinate fell on an ERA5-Land sea cell; resolved via the pre-built Italy reference grid's gap-filled cell_id (Hijmans et al. 2005; see 1_2_reference_grid_final.R). Record retained, not excluded."
  )
qc_agg_log[["coastal_grid_filled"]] <- coastal_grid_filled_log

db_mapped <- db_mapped %>%
  dplyr::filter(!is.na(ID)) %>%
  dplyr::select(-grid_filled, -grid_fill_dist_m)

nrow(still_no_cell_log)
nrow(coastal_grid_filled_log)

# Aggregation
#
# multiple genuinely
# distinct sampling records that share the same grid cell, epidemiological
# week, year, trap type, species, life stage and sex are combined into a
# single weekly summary value per cell. This is a methodological aggregation, not an error
# correction, and no information is discarded: n_records_aggregated
# records, for every output row, exactly how many raw records fed into
# it.
#
# In addition to the weekly median (already present), the interquartile
# range is now also retained per cell/week/trap_type/species/life_stage/
# sex, as two columns (value_q1 = 25th percentile, value_q3 = 75th
# percentile of the raw values aggregated into that cell), so the spread
# of raw observations behind each median is not lost.
db_aggregated <- db_mapped %>%
  dplyr::group_by(
    ID,             # space (ERA5-Land grid cell, via the reference grid)
    week,
    year,           # time (week and year)
    trap_type,      # no mixing trap types
    species,
    life_stage,
    sex
  ) %>%
  dplyr::summarise(
    value_q1 = stats::quantile(value, 0.25, na.rm = TRUE, names = FALSE, type = 7),  # IQR lower bound (25th percentile)
    value_q3 = stats::quantile(value, 0.75, na.rm = TRUE, names = FALSE, type = 7),  # IQR upper bound (75th percentile)
    value    = median(value, na.rm = TRUE),          # weekly median per cell x trap_type x sex
    
    n_records_aggregated = dplyr::n(),   # how many raw records fed into this row
    
    # Metadata recovery
    Country = dplyr::first(Country),
    Region = dplyr::first(Region),
    Institute = dplyr::first(Institute),
    contact_person = dplyr::first(contact_person),
    contact_person_email = dplyr::first(contact_person_email),
    # volume = dplyr::first(volume),
    # substrate = dplyr::first(substrate),
    # larvicide_presence = dplyr::first(larvicide_presence),
    # larvicide_type = dplyr::first(larvicide_type),
    Canonical_name = dplyr::first(Canonical_name),
    kingdom = dplyr::first(kingdom),
    phylum = dplyr::first(phylum),
    class = dplyr::first(class),
    order = dplyr::first(order),
    family = dplyr::first(family),
    genus = dplyr::first(genus),
    EPSG = dplyr::first(EPSG),
    municipality_centroid = dplyr::case_when(
      all(municipality_centroid == "no", na.rm = TRUE) ~ "no",
      all(municipality_centroid == "yes", na.rm = TRUE) ~ "yes",
      TRUE ~ paste0("mixed (n=", sum(municipality_centroid == "yes", na.rm = TRUE),
                    " yes, n=", sum(municipality_centroid == "no", na.rm = TRUE), " no)")
    ),
    note = dplyr::na_if(
      paste(
        sprintf("N = %d \"%s\"",
                as.integer(table(note[!is.na(note) & note != ""])),
                names(table(note[!is.na(note) & note != ""]))),
        collapse = " | "
      ),
      ""
    ),
    .groups = "drop"
  ) %>%
  dplyr::select(ID, week, year, trap_type, species, life_stage, sex,
                value, value_q1, value_q3, n_records_aggregated, dplyr::everything())

str(db)

head(db_aggregated$note[!is.na(db_aggregated$note)])
head(db_aggregated$note[grepl("\" \\| N =", db_aggregated$note)])

# Final db aggregate
# longitude/latitude are recovered from the ITALY GRID raster (cell_id is
# native to that raster's own dimensions.
db_final <- db_aggregated %>%
  dplyr::mutate(
    longitude = terra::xFromCell(cellid_italy_r, ID),
    latitude  = terra::yFromCell(cellid_italy_r, ID)
  ) %>%
  # Rename ID to cell_id (reviewer request: unique grid cell identifier)
  dplyr::rename(cell_id = ID) %>%
  # Standardise region names to English
  dplyr::mutate(
    Region = dplyr::case_when(
      Region == "Toscana" ~ "Tuscany",
      TRUE ~ Region
    )
  ) %>%
  dplyr::select(cell_id, any_of(names(db)), value_q1, value_q3, n_records_aggregated)


ggplot2::ggplot() +
  ggplot2::geom_sf(data = ita_muni, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db_final, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 0.5, alpha = 0.4) +
  ggplot2::coord_sf(xlim = c(6.0, 19.0), ylim = c(35.0, 48.0)) +
  ggplot2::theme_bw()


#### 4. DIAGNOSTICS & TEMPORAL CHECKS ####
# unique weeks on original db
check_original <- culex_updated %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(n_weeks_orig = n_distinct(paste(year, week, sep="-")), .groups = "drop")

# unique weeks in aggregated db
check_final <- db_final %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(n_weeks_final = n_distinct(paste(year, week, sep="-")), .groups = "drop")

# check for differences
check_summary <- check_original %>%
  dplyr::left_join(check_final, by = "Region") %>%
  dplyr::mutate(
    n_weeks_final = ifelse(is.na(n_weeks_final), 0, n_weeks_final),
    lost_weeks = n_weeks_orig - n_weeks_final
  )

print(check_summary)
readr::write_csv(check_summary, "../main_db/qc_temporal_coverage_diagnostics.csv")


#### 5. FINAL DATA STRUCTURE & LEVELS CHECK ####
# Check final dataset structure and column formats (Data Types)
dplyr::glimpse(db_final)

# Descriptive statistics for numeric variables
summary(dplyr::select(db_final, tidyselect::where(is.numeric)))

# Check levels for categorical variables post-aggregation
print(table(db_final$sex, useNA = "always"))

print(table(db_final$life_stage, useNA = "always"))

print(table(db_final$trap_type, useNA = "always"))

print(table(db_final$Region, useNA = "always"))

# check for missing values (NA) per column
print(colSums(is.na(db_final)))

# duplicate check
dup_key_check <- db_final %>%
  dplyr::count(cell_id, year, week, trap_type, species, life_stage, sex, name = "n_rows") %>%
  dplyr::filter(n_rows > 1)

nrow(dup_key_check)
if (nrow(dup_key_check) > 0) {
  print(dup_key_check)
  readr::write_csv(dup_key_check, "../main_db/qc_aggregation_key_duplicates.csv")
}
stopifnot(
  "Aggregation key (cell_id/year/week/trap_type/species/life_stage/sex) is not unique in db_final -- bug in Section 3" =
    nrow(dup_key_check) == 0
)

# study period, enforced a second time 
stopifnot(
  "db_final contains records outside the declared study period -- check Section 1 filtering" =
    all(db_final$year >= STUDY_YEAR_MIN & db_final$year <= STUDY_YEAR_MAX)
)

# completeness of fields essential to the modelling use of
# this dataset.
stopifnot(
  "Missing cell_id in db_final"                = all(!is.na(db_final$cell_id)),
  "Missing coordinates in db_final"             = all(!is.na(db_final$latitude) & !is.na(db_final$longitude)),
  "Missing catch value (median) in db_final"    = all(!is.na(db_final$value)),
  "Negative catch value (median) in db_final"   = all(db_final$value >= 0),
  "Missing year/week in db_final"               = all(!is.na(db_final$year) & !is.na(db_final$week)),
  "Missing n_records_aggregated in db_final"    = all(!is.na(db_final$n_records_aggregated) & db_final$n_records_aggregated >= 1)
)



#### 6. QC SUMMARY REPORT ####
qc_agg_log_combined <- dplyr::bind_rows(qc_agg_log)
readr::write_csv(qc_agg_log_combined, qc_agg_log_path)

n_excluded_off_target   <- nrow(off_target_species)
n_excluded_out_of_period <- nrow(out_of_period)
n_excluded_stage3_checks <- nrow(stage3_centroid_log) + nrow(out_of_bounds_log) +
  nrow(missing_week_log) + nrow(still_missing_coord_log) + nrow(still_no_cell_log)
n_coastal_grid_filled    <- nrow(coastal_grid_filled_log)
n_entering_aggregation    <- nrow(db_mapped)
n_final_aggregated_rows   <- nrow(db_final)
n_georeferenced_centroid_upstream <- sum(db_final$municipality_centroid == "yes", na.rm = TRUE)

qc_summary_aggregation <- tibble::tibble(
  metric = c(
    "n_input_records_from_stage2",
    "n_excluded_off_target_species",
    "n_excluded_out_of_study_period",
    "n_excluded_defensive_checks_stage3",
    "n_records_entering_aggregation",
    "n_coastal_records_resolved_via_gapfilled_grid_cell",
    "n_final_aggregated_rows",
    "n_rows_georeferenced_via_centroid_upstream_or_stage3",
    "mean_raw_records_per_aggregated_row"
  ),
  value = c(
    n_input,
    n_excluded_off_target,
    n_excluded_out_of_period,
    n_excluded_stage3_checks,
    n_entering_aggregation,
    n_coastal_grid_filled,
    n_final_aggregated_rows,
    n_georeferenced_centroid_upstream,
    round(mean(db_final$n_records_aggregated), 2)
  )
)

print(qc_summary_aggregation, n = Inf)

readr::write_csv(qc_summary_aggregation, "../main_db/qc_summary_report_aggregation.csv")


#### 6b. visual check of median and iqr ####
library(ggplot2)
library(scales)

national_trend <- db_final %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    avg_median = mean(value, na.rm = TRUE),
    avg_q1 = mean(value_q1, na.rm = TRUE),
    avg_q3 = mean(value_q3, na.rm = TRUE),
    .groups = "drop"
  )

plot_national <- ggplot(national_trend, aes(x = week, y = avg_median)) +
  geom_ribbon(aes(ymin = avg_q1, ymax = avg_q3), fill = "steelblue", alpha = 0.3) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  scale_x_continuous(breaks = seq(min(national_trend$week), max(national_trend$week), by = 2)) +
  theme_bw() +
  labs(
    title = "National Weekly Trend (Culex pipiens)",
    subtitle = "Solid line represents the mean of medians; shaded area represents the mean IQR",
    x = "Epidemiological Week",
    y = "Captures (Aggregated values)"
  )

print(plot_national)


regional_trend <- db_final %>%
  dplyr::group_by(Region, week) %>%
  dplyr::summarise(
    avg_median = mean(value, na.rm = TRUE),
    avg_q1 = mean(value_q1, na.rm = TRUE),
    avg_q3 = mean(value_q3, na.rm = TRUE),
    .groups = "drop"
  )

plot_regional_trend <- ggplot(regional_trend, aes(x = week, y = avg_median)) +
  geom_ribbon(aes(ymin = avg_q1, ymax = avg_q3, fill = Region), alpha = 0.3) +
  geom_line(aes(color = Region), linewidth = 0.8) +
  facet_wrap(~ Region, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Weekly Trend by Region (Median and IQR)",
    x = "Epidemiological Week",
    y = "Captures"
  )

print(plot_regional_trend)


plot_regional_box <- ggplot(db_final, aes(x = reorder(Region, value, FUN = median), y = value)) +
  geom_boxplot(fill = "lightgrey", color = "darkblue", outlier.color = "red", outlier.size = 0.5, alpha = 0.7) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000)) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Distribution of Aggregated Values by Region",
    subtitle = "Ordered by increasing median. Y-axis on log1p scale. Red dots indicate spatiotemporal outliers.",
    x = "Region",
    y = "Weekly median per cell (Aggregated values)"
  )

print(plot_regional_box)


#### 7. SAVE FINAL OUTPUT ####
outdir <- "../main_db/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

write.csv(db_final, file = paste0(outdir, "db_aggregated.csv"), row.names = FALSE)
