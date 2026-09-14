# =============================================================================
# DATA HARMONISATION — National Database Merge (Samplings)
# =============================================================================
# Author    : Eleonora Longo
#             PhD Student in Agrifood and Environmental Science
#             Center of Agriculture, Food and Environment (C3A)
#             University of Trento
#             Via Edmund Mach, 1 - 38098 San Michele all'Adige (TN)
#             eleonora.longo@unitn.it
#
# Co-Authors: Blaha M., et al.
#
# Purpose   : Aggregates and merges all previously cleaned and harmonised
#             regional mosquito surveillance datasets into a single,
#             comprehensive national master database. Enforces uniform data
#             types across all columns and performs a second, NATIONAL-LEVEL
#             layer of quality control (in addition to the region-specific
#             checks already performed in the 1_0_*_final.R scripts), so
#             that every QC decision applied to the final dataset is
#             documented, reproducible, and reportable to reviewers.
#
# Input     : ../main_db/clean_data/*_samplings_clean.csv
#             ../main_db/qc_exclusions_log.csv (Stage-1 region-level
#             exclusions, written by each 1_0_*_final.R script -- READ ONLY,
#             this script never writes to it; see the design note by
#             qc_log_path in section 2)
#             ../other_data/comuni.geojson (ISTAT municipality polygons,
#             must carry both a municipality name field ['name'] and a
#             province name field ['prov_name'])
#
# Output    : ../main_db/db_samplings_clean.csv          (unchanged schema)
#             ../main_db/qc_exclusions_log_merge_stage.csv (NEW, exclusively
#             owned by this script -- national-level validation failures
#             and national-level exact duplicates; never mixed into the
#             Stage-1 qc_exclusions_log.csv)
#             ../main_db/qc_municipality_province_mismatches.csv
#             ../main_db/qc_spatial_outliers.csv
#             ../main_db/manual_review_queue.csv
#             ../main_db/qc_summary_report.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#       1.1.  Samplings (Load regional datasets)
#    2. Merge Samplings
#       2.1.  Removing NA in values
#       2.2.  Merge and enforce uniform data types
#       2.3.  Standardise column names and region names
#       2.4.  National-level validation: coercion failures, invalid dates,
#             invalid coordinates, missing values
#       2.5.  National-level duplicate detection
#       2.6.  Coordinate <-> Municipality/Province consistency check
#       2.7.  Spatial outlier detection (trap positional consistency +
#             robust within-municipality distance check)
#       2.8.  Manual review queue
#    3. QC Summary Report (reviewer-facing)
#    4. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0. libraries and working directory ####
library(dplyr)
library(tidyverse)
library(sf)         

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# QC log paths.
qc_log_path        <- "../main_db/qc_exclusions_log.csv"
qc_merge_log_path <- "../main_db/qc_exclusions_log_merge_stage.csv"


#### 1. DATA ####
##### 1.1. samplings #####
# abruzzo_molise <- readr::read_csv("../main_db/clean_data/abruzzo_molise_samplings_clean.csv", col_types = cols(.default = "c"))

campania_calabria <- readr::read_csv("../main_db/clean_data/campania_calabria_samplings_clean.csv", col_types = cols(.default = "c"))

emilia <- readr::read_csv("../main_db/clean_data/emiliaromagna_samplings_clean.csv", col_types = cols(.default = "c"))

liguria <- readr::read_csv("../main_db/clean_data/liguria_samplings_clean.csv", col_types = cols(.default = "c"))

lombardia <- readr::read_csv("../main_db/clean_data/lombardia_samplings_clean.csv", col_types = cols(.default = "c"))

piemonte <- readr::read_csv("../main_db/clean_data/piemonte_samplings_clean.csv", col_types = cols(.default = "c"))

puglia_basilicata <- readr::read_csv("../main_db/clean_data/puglia_basilicata_samplings_clean.csv", col_types = cols(.default = "c"))

sardegna <- readr::read_csv("../main_db/clean_data/sardegna_samplings_clean.csv", col_types = cols(.default = "c"))

sicilia <- readr::read_csv("../main_db/clean_data/sicilia_samplings_clean.csv", col_types = cols(.default = "c"))

toscana_lazio <- readr::read_csv("../main_db/clean_data/toscana_lazio_samplings_clean.csv", col_types = cols(.default = "c"))

trentino <- readr::read_csv("../main_db/clean_data/trentino_samplings_clean.csv", col_types = cols(.default = "c"))

umbria_marche <- readr::read_csv("../main_db/clean_data/umbria_marche_samplings_clean.csv", col_types = cols(.default = "c"))

veneto_friuli <- readr::read_csv("../main_db/clean_data/veneto_friuli_samplings_clean.csv", col_types = cols(.default = "c"))


#### 2. merge samplings ####
##### 2.1. removing na in values #####
# range(abruzzo_molise$value)
range(campania_calabria$value)
# campania_calabria <- campania_calabria %>% dplyr::filter(!is.na(value))
range(emilia$value)
range(liguria$value)
range(lombardia$value)
range(piemonte$value)
# piemonte <- piemonte %>% dplyr::filter(!is.na(value))
# piemonte <- piemonte %>% dplyr::filter(!value %in% c("n.d.", "ND", "nd",
#                                                      "SPEGNIMENTO TRAPPOLE DELLE PROVINCE POSITIVE SEGUITO DA ALCUNE RIACCENSIONI",
#                                                      "SPEGNIMENTO TRAPPOLE DELLE PROVINCE POSITIVE (CON ALCUNE ECCEZIONI)"))
range(puglia_basilicata$value)
range(sardegna$value)
range(sicilia$value)
range(toscana_lazio$value)
range(trentino$value)
range(umbria_marche$value)
range(veneto_friuli$value)
# veneto_friuli <- veneto_friuli %>%  dplyr::filter(!is.na(value))


##### 2.2. merge #####
n_regional <- tibble::tribble(
  ~region,              ~n_retained_regional,
  "campania_calabria",  nrow(campania_calabria),
  "emiliaromagna",      nrow(emilia),
  "liguria",            nrow(liguria),
  "lombardia",          nrow(lombardia),
  "piemonte",           nrow(piemonte),
  "puglia_basilicata",  nrow(puglia_basilicata),
  "sardegna",           nrow(sardegna),
  "sicilia",            nrow(sicilia),
  "toscana_lazio",      nrow(toscana_lazio),
  "trentino",           nrow(trentino),
  "umbria_marche",      nrow(umbria_marche),
  "veneto_friuli",      nrow(veneto_friuli)
)

dbs <- list(
  # abruzzo_molise,
  campania_calabria, emilia, liguria,
  lombardia, piemonte, puglia_basilicata, sardegna, sicilia,
  toscana_lazio, trentino, umbria_marche, veneto_friuli
)

dbs <- lapply(dbs, function(df) {
  df[] <- lapply(df, as.character)
  df
})

db <- dplyr::bind_rows(dbs)

stopifnot(
  "Row count changed during bind_rows() -- investigate column names/types across regional files before proceeding" =
    nrow(db) == sum(n_regional$n_retained_regional)
)

db <- db %>%
  dplyr::mutate(
    year_raw      = year,
    week_raw      = week,
    date_raw      = date,
    value_raw     = value,
    latitude_raw  = latitude,
    longitude_raw = longitude
  )

db <- db %>%
  dplyr::mutate(
    year = as.integer(year),
    week = as.integer(week),
    date = as.Date(date),
    value = as.numeric(value),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    municipality_centroid = as.factor(municipality_centroid),
    id_trap = as.character(id_trap),
    trap_type = as.factor(trap_type),
    Canonical_name = as.character(Canonical_name),
    kingdom = as.factor(kingdom),
    phylum = as.factor(phylum),
    class = as.factor(class),
    order = as.factor(order),
    family = as.factor(family),
    genus = as.factor(genus),
    species = as.factor(species),
    life_stage = as.factor(life_stage),
    sex = as.factor(sex),
    EPSG = as.integer(EPSG),
    note = as.character(note)
  )

levels(as.factor(db$sex))
str(db)


##### 2.3. Standardise column names and region names #####
db <- db %>%
  dplyr::mutate(
    Region = dplyr::case_when(
      Region == "Toscana"                ~ "Tuscany",
      Region == "Piemonte"               ~ "Piedmont",
      Region == "Lombardia"              ~ "Lombardy",
      Region == "Sicilia"                ~ "Sicily",
      Region == "Sardegna"               ~ "Sardinia",
      Region == "Puglia"                 ~ "Apulia",
      Region == "Friuli Venezia Giulia"  ~ "Friuli-Venezia Giulia",
      Region == "Trentino-Alto Adige"    ~ "Autonomous Province of Trento",
      TRUE                               ~ Region
    )
  )

print(unique(db$Region))

# change lower_snake_case region identifiers used in
# qc_exclusions_log.csv 
region_lookup <- db %>%
  dplyr::distinct(Region) %>%
  dplyr::mutate(
    region = dplyr::case_when(
      stringr::str_detect(Region, "Campania|Calabria")            ~ "campania_calabria",
      Region == "Emilia-Romagna"                                   ~ "emiliaromagna",
      Region == "Liguria"                                          ~ "liguria",
      Region == "Lombardy"                                         ~ "lombardia",
      Region == "Piedmont"                                         ~ "piemonte",
      stringr::str_detect(Region, "Apulia|Basilicata")             ~ "puglia_basilicata",
      Region == "Sardinia"                                         ~ "sardegna",
      Region == "Sicily"                                           ~ "sicilia",
      stringr::str_detect(Region, "Tuscany|Lazio")                 ~ "toscana_lazio",
      Region == "Autonomous Province of Trento"                    ~ "trentino",
      stringr::str_detect(Region, "Umbria|Marche")                 ~ "umbria_marche",
      stringr::str_detect(Region, "Veneto|Friuli")                 ~ "veneto_friuli",
      TRUE ~ NA_character_
    )
  )

if (any(is.na(region_lookup$region))) {
  warning("region_lookup could not match the following Region value(s) to a source script: ",
          paste(region_lookup$Region[is.na(region_lookup$region)], collapse = ", "),
          " -- update the case_when() above if a new region has been added to the pipeline. ",
          "Records for this Region will be logged under region = NA in qc_exclusions_log.csv ",
          "until this is fixed.")
}


##### 2.4. National-level validation: dates, coordinates, missing values #####
ITALY_LAT_RANGE <- c(35.0, 47.5)  
ITALY_LON_RANGE <- c(6.5, 18.5)

coercion_check <- db %>%
  dplyr::transmute(
    Region, Municipality, date, date_raw, value_raw, latitude_raw, longitude_raw, year_raw, week_raw,
    fail_date  = is.na(date)      & !is.na(date_raw)      & trimws(date_raw)      != "",
    fail_value = is.na(value)     & !is.na(value_raw)     & trimws(value_raw)     != "",
    fail_lat   = is.na(latitude)  & !is.na(latitude_raw)  & trimws(latitude_raw)  != "",
    fail_lon   = is.na(longitude) & !is.na(longitude_raw) & trimws(longitude_raw) != "",
    fail_year  = is.na(year)      & !is.na(year_raw)      & trimws(year_raw)      != "",
    fail_week  = is.na(week)      & !is.na(week_raw)      & trimws(week_raw)      != "",
    out_of_bounds = !is.na(latitude) & !is.na(longitude) &
      (!dplyr::between(latitude, ITALY_LAT_RANGE[1], ITALY_LAT_RANGE[2]) |
         !dplyr::between(longitude, ITALY_LON_RANGE[1], ITALY_LON_RANGE[2])),
    future_date = !is.na(date) & date > Sys.Date(),
    implausible_early_date = !is.na(date) & date < as.Date("2000-01-01")
  )

sum(coercion_check$fail_date)
sum(coercion_check$fail_value)
sum(coercion_check$fail_lat)
sum(coercion_check$fail_lon)
sum(coercion_check$fail_year)
sum(coercion_check$fail_week)
sum(coercion_check$out_of_bounds)
sum(coercion_check$future_date)
sum(coercion_check$implausible_early_date)

# Log 
national_validation_log <- dplyr::bind_rows(
  coercion_check %>% dplyr::filter(fail_date) %>%
    dplyr::transmute(region = Region, check = "invalid_date_postmerge", sheet = NA_character_,
                     row_index = NA_integer_, Municipality, date = as.Date(NA), value_raw = date_raw,
                     reason = "Date string could not be parsed at the national-merge stage."),
  coercion_check %>% dplyr::filter(fail_value) %>%
    dplyr::transmute(region = Region, check = "invalid_value_postmerge", sheet = NA_character_,
                     row_index = NA_integer_, Municipality, date, value_raw,
                     reason = "Value string could not be parsed at the national-merge stage."),
  coercion_check %>% dplyr::filter(fail_lat | fail_lon) %>%
    dplyr::transmute(region = Region, check = "invalid_coordinate_postmerge", sheet = NA_character_,
                     row_index = NA_integer_, Municipality, date,
                     value_raw = paste0("lat=", latitude_raw, ", lon=", longitude_raw),
                     reason = "Coordinate string could not be parsed at the national-merge stage."),
  coercion_check %>% dplyr::filter(out_of_bounds) %>%
    dplyr::transmute(region = Region, check = "coordinate_out_of_italy_bbox", sheet = NA_character_,
                     row_index = NA_integer_, Municipality, date,
                     value_raw = paste0("lat=", latitude_raw, ", lon=", longitude_raw),
                     reason = sprintf("Coordinates fall outside the Italy bounding box (lat %s-%s, lon %s-%s).",
                                      ITALY_LAT_RANGE[1], ITALY_LAT_RANGE[2], ITALY_LON_RANGE[1], ITALY_LON_RANGE[2]))
)

print(national_validation_log)

# Missing-value profile for every column
print(colSums(is.na(db)))

# check on 'value', 'date', 'year', 'week', 'latitude' and 'longitude' 
stopifnot(
  "Missing catch value (value) after national merge"     = all(!is.na(db$value)),
  "Negative catch values detected after national merge"  = all(db$value >= 0),
  "Missing date after national merge"                    = all(!is.na(db$date)),
  "Missing year after national merge"                    = all(!is.na(db$year)),
  "Missing week after national merge"                    = all(!is.na(db$week)),
  "Missing coordinates after national merge"             = all(!is.na(db$latitude) & !is.na(db$longitude))
)

# drop Temporary diagnostic columns 
db <- db %>% dplyr::select(-dplyr::ends_with("_raw"))


##### 2.5. National-level duplicate detection #####
key_cols <- setdiff(names(db), "note")

db <- db %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
  dplyr::mutate(dup_group_size = dplyr::n(), dup_row_in_group = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Regions with no stable trap identifier in the source data (id_trap
    # is NA for every record: Sardinia, Tuscany/Lazio, Campania/Calabria)
    # cannot be safely deduplicated by "identical on every field" --
    # verified directly against the raw Sardinia data that groups of 8-9
    # identical zero-catch rows recur consistently across many dates in
    # the same municipality (e.g. Oristano), far too regular to be
    # copy-paste errors; the far more plausible explanation is multiple
    # distinct physical traps sharing one municipality-level coordinate
    # because no per-trap coordinate was available in the source. Since
    # id_trap is part of the grouping key above, this property is
    # well-defined per group (a group is either entirely NA-id_trap or
    # entirely not). These groups are NOT collapsed; kept as recorded
    # with an explanatory note, pending confirmation from each region's
    # referent.
    is_notrap_ambiguous = dup_group_size > 1 & is.na(id_trap)
  )

postmerge_duplicates <- db %>%
  dplyr::filter(dup_group_size > 1, dup_row_in_group > 1, !is_notrap_ambiguous) %>%
  dplyr::transmute(
    region = Region, check = "exact_duplicate_postmerge", sheet = NA_character_,
    row_index = NA_integer_, Municipality, date,
    value_raw = as.character(value),
    reason = "Exact duplicate row (identical on every field except note) identified during the national merge; collapsed to one observation."
  )

n_dup_groups <- db %>% dplyr::filter(dup_group_size > 1) %>% dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) %>% nrow()
n_dup_groups

nrow(postmerge_duplicates)
if (nrow(postmerge_duplicates) > 0) {
  print(dplyr::count(postmerge_duplicates, region, sort = TRUE))
}

merge_stage_log <- dplyr::bind_rows(national_validation_log, postmerge_duplicates)
readr::write_csv(merge_stage_log, qc_merge_log_path)

db <- db %>%
  dplyr::mutate(
    dup_note = dplyr::case_when(
      is_notrap_ambiguous ~ "Identical to other record(s) sharing the same municipality-level coordinate; this region has no stable trap identifier in the source data, so this cannot be confirmed as a true duplicate versus a genuinely distinct physical trap -- kept as recorded, pending confirmation from the regional referent.",
      dup_group_size > 1 & dup_row_in_group == 1 ~ sprintf("%d exact duplicate row(s) removed at the national-merge stage (see qc_exclusions_log_merge_stage.csv).",
                                                           dup_group_size - 1),
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(dup_note) ~ paste(note, dup_note, sep = " | "),
      is.na(note)  & !is.na(dup_note) ~ dup_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::filter(!(dup_group_size > 1 & dup_row_in_group > 1 & !is_notrap_ambiguous)) %>%
  dplyr::select(-dup_group_size, -dup_row_in_group, -dup_note, -is_notrap_ambiguous) 


##### 2.6. Coordinate <-> Municipality/Province consistency check #####
ita_muni <- sf::st_read("../other_data/comuni.geojson", quiet = TRUE) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    Municipality_upper = stringr::str_to_upper(stringr::str_trim(name)),
    Province_upper     = stringr::str_to_upper(stringr::str_trim(prov_name))
  )

province_en_it_map <- c(
  "TURIN"                 = "TORINO",
  "MILAN"                 = "MILANO",
  "ROME"                  = "ROMA",
  "MANTUA"                = "MANTOVA",
  "SYRACUSE"              = "SIRACUSA",
  "NAPLES"                = "NAPOLI",
  "FLORENCE"              = "FIRENZE",
  "VENICE"                = "VENEZIA",
  "GENOA"                 = "GENOVA",
  "PADUA"                 = "PADOVA",
  "MONZA AND BRIANZA"     = "MONZA E DELLA BRIANZA",
  "MONZA E BRIANZA"       = "MONZA E DELLA BRIANZA",
  "PESARO AND URBINO"     = "PESARO E URBINO"
)

db_for_check <- db %>%
  dplyr::filter(
    !is.na(latitude), !is.na(longitude),
    !is.na(Municipality),
    is.na(municipality_centroid) | municipality_centroid != "yes"
  ) %>%
  dplyr::mutate(
    declared_muni     = stringr::str_to_upper(stringr::str_trim(Municipality)),
    declared_prov_raw = ifelse(is.na(Province), NA_character_, stringr::str_to_upper(stringr::str_trim(Province))),
    declared_prov     = dplyr::if_else(
      !is.na(declared_prov_raw) & declared_prov_raw %in% names(province_en_it_map),
      unname(province_en_it_map[declared_prov_raw]),
      declared_prov_raw
    )
  )

db_sf <- db_for_check %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs    = 4326,
    remove = FALSE
  )

db_check <- sf::st_join(
  db_sf,
  ita_muni %>% dplyr::select(Municipality_upper, Province_upper),
  join = sf::st_within
) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    matched_muni     = Municipality_upper,
    matched_prov     = Province_upper,
    coord_muni_match = declared_muni == matched_muni,
    coord_prov_match = is.na(declared_prov) | declared_prov == matched_prov
  )

muni_mismatches <- db_check %>%
  dplyr::filter(!coord_muni_match | is.na(matched_muni))

prov_mismatches <- db_check %>%
  dplyr::filter(!coord_prov_match | (is.na(matched_prov) & !is.na(declared_prov)))

nrow(db_for_check)
nrow(muni_mismatches)
round(100 * nrow(muni_mismatches) / nrow(db_for_check), 2)
nrow(prov_mismatches)
round(100 * nrow(prov_mismatches) / nrow(db_for_check), 2)

if (nrow(muni_mismatches) > 0) {
  cat("\nMunicipality mismatches by Region:\n")
  print(muni_mismatches %>% dplyr::count(Region, sort = TRUE))
  cat("\nUnique (declared vs matched) Municipality pairs -- first 20:\n")
  print(
    muni_mismatches %>%
      dplyr::distinct(Region, declared_muni, matched_muni) %>%
      dplyr::arrange(Region, declared_muni) %>%
      head(20)
  )
}
if (nrow(prov_mismatches) > 0) {
  cat("\nProvince mismatches by Region:\n")
  print(prov_mismatches %>% dplyr::count(Region, sort = TRUE))
  cat("\nUnique (declared vs matched) Province pairs -- first 20:\n")
  print(
    prov_mismatches %>%
      dplyr::distinct(Region, declared_prov, matched_prov) %>%
      dplyr::arrange(Region, declared_prov) %>%
      head(20)
  )
}

qc_muniprov_path <- "../main_db/qc_municipality_province_mismatches.csv"
readr::write_csv(
  db_check %>%
    dplyr::filter(!coord_muni_match | is.na(matched_muni) | !coord_prov_match | (is.na(matched_prov) & !is.na(declared_prov))) %>%
    dplyr::select(Region, Province, Municipality, id_trap, date, latitude, longitude,
                  declared_muni, matched_muni, coord_muni_match,
                  declared_prov, matched_prov, coord_prov_match),
  qc_muniprov_path
)

rm(db_for_check, db_sf)


##### 2.7. Spatial outlier detection #####
TRAP_DIST_THRESHOLD_M <- 500
MIN_N_FOR_MAD         <- 5
MAD_K                 <- 3
MIN_DIST_FLOOR_M      <- 1000

outlier_base <- db %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude),
                is.na(municipality_centroid) | municipality_centroid != "yes")

outlier_visits <- outlier_base %>%
  dplyr::distinct(Region, Province, Municipality, id_trap, date, latitude, longitude)

trap_ref <- outlier_visits %>%
  dplyr::filter(!is.na(id_trap)) %>%
  dplyr::group_by(id_trap) %>%
  dplyr::summarise(
    lat_ref = stats::median(latitude, na.rm = TRUE),
    lon_ref = stats::median(longitude, na.rm = TRUE),
    n_trap_records = dplyr::n(),
    .groups = "drop"
  )

trap_check <- outlier_visits %>%
  dplyr::filter(!is.na(id_trap)) %>%
  dplyr::left_join(trap_ref, by = "id_trap")

trap_pts    <- sf::st_as_sf(trap_check, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
trap_refpts <- sf::st_as_sf(trap_check, coords = c("lon_ref", "lat_ref"), crs = 4326, remove = FALSE)
trap_check$dist_from_trap_median_m <- as.numeric(sf::st_distance(trap_pts, trap_refpts, by_element = TRUE))
trap_check <- trap_check %>%
  dplyr::mutate(trap_position_outlier = n_trap_records >= 2 & dist_from_trap_median_m > TRAP_DIST_THRESHOLD_M)

muni_ref <- outlier_visits %>%
  dplyr::filter(!is.na(Municipality)) %>%
  dplyr::group_by(Municipality) %>%
  dplyr::summarise(
    lat_med = stats::median(latitude, na.rm = TRUE),
    lon_med = stats::median(longitude, na.rm = TRUE),
    n_muni_records = dplyr::n(),
    .groups = "drop"
  )

muni_check <- outlier_visits %>%
  dplyr::filter(!is.na(Municipality)) %>%
  dplyr::left_join(muni_ref, by = "Municipality")

muni_pts    <- sf::st_as_sf(muni_check, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
muni_refpts <- sf::st_as_sf(muni_check, coords = c("lon_med", "lat_med"), crs = 4326, remove = FALSE)
muni_check$dist_from_muni_median_m <- as.numeric(sf::st_distance(muni_pts, muni_refpts, by_element = TRUE))

muni_check <- muni_check %>%
  dplyr::group_by(Municipality) %>%
  dplyr::mutate(
    med_dist_m         = stats::median(dist_from_muni_median_m),
    mad_dist_m         = stats::mad(dist_from_muni_median_m, constant = 1.4826),
    robust_threshold_m = pmax(med_dist_m + MAD_K * mad_dist_m, MIN_DIST_FLOOR_M),
    muni_mad_outlier   = n_muni_records >= MIN_N_FOR_MAD & dist_from_muni_median_m > robust_threshold_m
  ) %>%
  dplyr::ungroup()

qc_muniprov_coltypes <- readr::cols(
  date = readr::col_date(), latitude = readr::col_double(), longitude = readr::col_double(),
  coord_muni_match = readr::col_logical(), coord_prov_match = readr::col_logical(),
  .default = readr::col_character()
)
muni_fail_set <- unique(readr::read_csv(qc_muniprov_path, col_types = qc_muniprov_coltypes)$declared_muni)

spatial_outliers <- trap_check %>%
  dplyr::select(Region, Province, Municipality, id_trap, date, latitude, longitude,
                dist_from_trap_median_m, trap_position_outlier) %>%
  dplyr::full_join(
    muni_check %>%
      dplyr::select(Region, Province, Municipality, id_trap, date, latitude, longitude,
                    dist_from_muni_median_m, robust_threshold_m, muni_mad_outlier),
    by = c("Region", "Province", "Municipality", "id_trap", "date", "latitude", "longitude")
  ) %>%
  dplyr::filter(dplyr::coalesce(trap_position_outlier, FALSE) | dplyr::coalesce(muni_mad_outlier, FALSE)) %>%
  dplyr::mutate(
    also_fails_polygon_check = stringr::str_to_upper(stringr::str_trim(Municipality)) %in% muni_fail_set,
    verification = dplyr::case_when(
      also_fails_polygon_check ~ "Likely error: also falls outside its declared Municipality polygon (section 2.6)",
      TRUE ~ "Statistically distant but within the declared Municipality polygon: recommend visual/manual check"
    )
  )

sum(trap_check$trap_position_outlier, na.rm = TRUE)
sum(muni_check$muni_mad_outlier, na.rm = TRUE)
nrow(spatial_outliers)
sum(spatial_outliers$also_fails_polygon_check)

readr::write_csv(spatial_outliers, "../main_db/qc_spatial_outliers.csv")

rm(outlier_base, outlier_visits, trap_ref, trap_check, trap_pts, trap_refpts,
   muni_ref, muni_check, muni_pts, muni_refpts, muni_fail_set)


##### 2.8. Manual review queue #####
qc_outliers_coltypes <- readr::cols(
  date = readr::col_date(), latitude = readr::col_double(), longitude = readr::col_double(),
  trap_position_outlier = readr::col_logical(), muni_mad_outlier = readr::col_logical(),
  also_fails_polygon_check = readr::col_logical(),
  .default = readr::col_character()
)

manual_review_queue <- dplyr::bind_rows(
  readr::read_csv(qc_muniprov_path, col_types = qc_muniprov_coltypes) %>%
    dplyr::transmute(
      Region, Province, Municipality, id_trap, date, latitude, longitude,
      flag_type = dplyr::case_when(
        !coord_muni_match | is.na(matched_muni) ~ "municipality_mismatch",
        TRUE ~ "province_mismatch"
      ),
      flag_detail = sprintf("declared Municipality/Province: %s / %s -- matched: %s / %s",
                            declared_muni, dplyr::coalesce(declared_prov, "NA"),
                            dplyr::coalesce(matched_muni, "NA"), dplyr::coalesce(matched_prov, "NA"))
    ),
  readr::read_csv("../main_db/qc_spatial_outliers.csv", col_types = qc_outliers_coltypes) %>%
    dplyr::transmute(
      Region, Province, Municipality, id_trap, date, latitude, longitude,
      flag_type = "spatial_outlier",
      flag_detail = verification
    )
) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    manually_reviewed = FALSE,
    reviewer_decision  = NA_character_,
    reviewer_note      = NA_character_
  )

readr::write_csv(manual_review_queue, "../main_db/manual_review_queue.csv")

cat("\n--- Manual review queue (section 2.8) ---\n")
cat("Records queued for manual/expert review: ", nrow(manual_review_queue),
    " of ", nrow(db), " total records (", round(100 * nrow(manual_review_queue) / nrow(db), 2),
    "%) -- see manual_review_queue.csv\n", sep = "")


#### 3. QC SUMMARY REPORT ####
# IMPORTANT LIMITATION for the five source scripts that each cover TWO
# administrative Regions (campania_calabria, puglia_basilicata,
# toscana_lazio, umbria_marche, veneto_friuli): n_retained_regional,
# n_excluded_stage1 and n_duplicates_stage1 are Stage-1 (regional-script)
# quantities, and Stage 1 only tracks exclusions by 'region' (the source
# SCRIPT), never by which of its two admin Regions a given exclusion
# belongs to. These three columns therefore show the FULL SOURCE-FILE
# total on BOTH of that file's rows, not a per-Region split -- e.g.
# campania_calabria's n_retained_regional is the same number on the
# Calabria row and the Campania row. This is why they must be summed once
# per DISTINCT source file below, not once per displayed row, and why the
# per-row consistency check is done at the source-file level too. If a
# true per-Region split of Stage-1 figures is ever needed, it would
# require each of those five regional scripts to log the admin Region
# (not just Municipality) alongside every exclusion -- out of scope here.
stage1_log <- if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE)
} else {
  tibble::tibble(region = character(), check = character())
}

stage2_log <- if (file.exists(qc_merge_log_path)) {
  readr::read_csv(qc_merge_log_path, show_col_types = FALSE)
} else {
  tibble::tibble(region = character(), check = character())
}

excluded_stage1   <- stage1_log %>% dplyr::count(region, name = "n_excluded_stage1")
duplicates_stage1 <- stage1_log %>% dplyr::filter(stringr::str_detect(check, "duplicate")) %>%
  dplyr::count(region, name = "n_duplicates_stage1")

excluded_stage2   <- stage2_log %>% dplyr::count(region, name = "n_excluded_stage2") %>%
  dplyr::rename(Region = region)
duplicates_stage2 <- stage2_log %>% dplyr::filter(stringr::str_detect(check, "duplicate")) %>%
  dplyr::count(region, name = "n_duplicates_stage2") %>%
  dplyr::rename(Region = region)

n_retained_final <- db %>%
  dplyr::count(Region, name = "n_retained_final")

n_centroid <- db %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(n_georeferenced_centroid = sum(municipality_centroid == "yes", na.rm = TRUE), .groups = "drop")

correction_pattern <- "(?i)corrected|standardised|standardized|recomputed|reconciled|resolved(?! using)|typo"
n_corrected <- db %>%
  dplyr::mutate(is_corrected = !is.na(note) & stringr::str_detect(note, correction_pattern) &
                  !stringr::str_detect(note, "(?i)centroid")) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(n_corrected = sum(is_corrected), .groups = "drop")

qc_summary <- region_lookup %>%
  dplyr::left_join(n_regional, by = "region") %>%
  dplyr::left_join(excluded_stage1, by = "region") %>%
  dplyr::left_join(duplicates_stage1, by = "region") %>%
  dplyr::left_join(excluded_stage2, by = "Region") %>%
  dplyr::left_join(duplicates_stage2, by = "Region") %>%
  dplyr::left_join(n_retained_final, by = "Region") %>%
  dplyr::left_join(n_centroid, by = "Region") %>%
  dplyr::left_join(n_corrected, by = "Region") %>%
  dplyr::mutate(
    dplyr::across(c(n_retained_regional, n_excluded_stage1, n_duplicates_stage1,
                    n_excluded_stage2, n_duplicates_stage2, n_retained_final,
                    n_georeferenced_centroid, n_corrected), ~ tidyr::replace_na(., 0)),
    n_raw_reconstructed  = n_retained_regional + n_excluded_stage1,
    n_duplicates_removed = n_duplicates_stage1 + n_duplicates_stage2
  ) %>%
  dplyr::select(
    region, Region, n_raw_reconstructed, n_excluded_stage1, n_retained_regional,
    n_excluded_stage2, n_retained_final, n_duplicates_stage1, n_duplicates_stage2,
    n_duplicates_removed, n_georeferenced_centroid, n_corrected
  ) %>%
  dplyr::arrange(region, Region)


file_level_totals <- qc_summary %>%
  dplyr::distinct(region, n_retained_regional, n_excluded_stage1, n_duplicates_stage1) %>%
  dplyr::summarise(
    n_retained_regional = sum(n_retained_regional),
    n_excluded_stage1   = sum(n_excluded_stage1),
    n_duplicates_stage1 = sum(n_duplicates_stage1)
  )

region_level_totals <- qc_summary %>%
  dplyr::summarise(
    n_excluded_stage2        = sum(n_excluded_stage2),
    n_duplicates_stage2      = sum(n_duplicates_stage2),
    n_retained_final         = sum(n_retained_final),
    n_georeferenced_centroid = sum(n_georeferenced_centroid),
    n_corrected              = sum(n_corrected)
  )

qc_summary_total <- dplyr::bind_cols(
  tibble::tibble(region = "TOTAL", Region = "All regions"),
  file_level_totals,
  region_level_totals
) %>%
  dplyr::mutate(
    n_raw_reconstructed  = n_retained_regional + n_excluded_stage1,
    n_duplicates_removed = n_duplicates_stage1 + n_duplicates_stage2
  ) %>%
  dplyr::select(dplyr::all_of(names(qc_summary)))

qc_summary <- dplyr::bind_rows(qc_summary, qc_summary_total)


consistency_check <- qc_summary %>%
  dplyr::filter(region != "TOTAL") %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_retained_regional   = dplyr::first(n_retained_regional),
    n_retained_final_sum  = sum(n_retained_final),
    n_excluded_stage2_sum = sum(n_excluded_stage2),
    .groups = "drop"
  ) %>%
  dplyr::mutate(ok = n_retained_regional == n_retained_final_sum + n_excluded_stage2_sum)

if (!all(consistency_check$ok)) {
  warning("QC summary inconsistency for source file(s): ",
          paste(consistency_check$region[!consistency_check$ok], collapse = ", "))
} else {
  cat("\nInternal consistency check passed for every source file.\n")
}


print(qc_summary, n = Inf)


readr::write_csv(qc_summary, "../main_db/qc_summary_report.csv")


#### 4. SAVE CLEAN DATA ####
outdir <- "../main_db/"

write.csv(db, file = paste0(outdir, "db_samplings_clean.csv"), row.names = FALSE)
