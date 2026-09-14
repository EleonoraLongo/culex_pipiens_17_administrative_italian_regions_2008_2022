# =============================================================================
# DATA HARMONISATION — Tuscany & Lazio mosquito surveillance (2009–2023)
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
# Purpose   : Cleans and harmonises the entomological surveillance dataset
#             for Tuscany and Lazio (Istituto Zooprofilattico Sperimentale
#             del Lazio e della Toscana M. Aleandri). Produces a standardised
#             CSV compatible with the national harmonised mosquito database.
#
# Input     : ../izs_data/ToscanaLazio/toscana_lazio_Dataset_FEM_augmented_with_WN_2023.xlsx
#               (sheet "FEM_augmented_final": the pre-reconciled 2009-2023
#               series, 3819 original FEM rows + 889 rows added from a
#               separate West Nile dataset that weren't already in FEM.
#               The workbook also carries a "manual_corrections" sheet
#               documenting 10 specific f/m value fixes applied while
#               reconciling the two sources -- verified below to already
#               be reflected in FEM_augmented_final's values, so not
#               reapplied, only flagged in note for provenance)
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/toscana_lazio_samplings_clean.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#    2. Add Municipality Information
#    3. Data Cleaning
#    4. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0. Libraries and working directory ####
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(stringr)
library(lubridate)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. DATA ####
##### 1.1. samplings #####
db <- readxl::read_excel(
  "../izs_data/ToscanaLazio/toscana_lazio_Dataset_FEM_augmented_with_WN_2023.xlsx",
  sheet = "FEM_augmented_final"
)

# the same workbook documents 10 rows with a manually-corrected f/m value
# (reconciling this source against a separate West Nile dataset); spot-
# checked against FEM_augmented_final by date/coordinate and confirmed
# already reflected in the values read above -- kept here only to flag
# provenance in note (Section 3.2), not to reapply anything
manual_corrections <- readxl::read_excel(
  "../izs_data/ToscanaLazio/toscana_lazio_Dataset_FEM_augmented_with_WN_2023.xlsx",
  sheet = "manual_corrections"
)


##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% c("Lazio", "Toscana"))


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. ADD MUNICIPALITY INFORMATION ####

names(db)[names(db) == "Comune"] <- "Municipality"

levels(as.factor(db$Municipality))

# comuni are stored with underscores instead of spaces (e.g.
# "Castiglion_Fiorentino")
db$Municipality <- as.character(db$Municipality)
db$Municipality <- gsub("_", " ", db$Municipality)
db$Municipality <- gsub("\\s+", " ", db$Municipality)
db$Municipality <- trimws(db$Municipality)

# Municipality ISTAT match diagnostic (before correction)
muni_not_match <- setdiff(toupper(db$Municipality), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# corrections
corrections <- c(
  "Castel del piano" = "Castel del Piano",
  "Massa marittima" = "Massa Marittima",
  "C della Pescaia" = "Castiglione della Pescaia",
  "San Giuliano T" = "San Giuliano Terme",
  "Villa San Giovanni" = "Villa San Giovanni in Tuscia",
  "Bagni a Ripoli" = "Bagni di Ripoli",
  # confirmed by the national merge script's point-in-polygon check as a
  # naming-only issue, not a geographic error: "Gallicano" is ambiguous
  # (ISTAT disambiguates a homonymous municipality in Lucca, Tuscany, from
  # this one in Lazio via the "nel Lazio" suffix)
  "Gallicano" = "Gallicano nel Lazio"
)

if (!"note" %in% names(db)) db$note <- NA_character_

municipality_before <- db$Municipality
db$Municipality <- dplyr::recode(db$Municipality, !!!corrections)
changed <- municipality_before != db$Municipality
muni_note <- ifelse(changed,
                    sprintf("Municipality typo corrected from '%s' to '%s'.", municipality_before, db$Municipality),
                    NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(muni_note) ~ paste(db$note, muni_note, sep = " | "),
  is.na(db$note) & !is.na(muni_note)  ~ muni_note,
  TRUE ~ db$note
)

# Municipality ISTAT match diagnostic 
muni_not_match <- setdiff(toupper(db$Municipality), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)


#### 3. Data cleaning ####
##### 3.1. columns with assigned data #####
stopifnot(
  "Unexpected column structure in FEM_augmented_final -- verify upstream reconciliation" =
    all(c("Data", "Anno", "Regione", "Provincia", "Municipality", "Latitude", "Longitude",
          "Tipo_trappola", "Cx_pipiens_f", "Cx_pipiens_m", "Cx_pipiens_tot") %in% names(db)),
  "Cx_pipiens_f + Cx_pipiens_m must equal Cx_pipiens_tot in every row" =
    all(db$Cx_pipiens_f + db$Cx_pipiens_m == db$Cx_pipiens_tot),
  "Negative catch values detected" =
    all(db$Cx_pipiens_f >= 0, db$Cx_pipiens_m >= 0, db$Cx_pipiens_tot >= 0)
)

names(db)[names(db) == "Data"] <- "date"
names(db)[names(db) == "Anno"] <- "year"
names(db)[names(db) == "Regione"] <- "Region"
names(db)[names(db) == "Provincia"] <- "Province"
names(db)[names(db) == "Latitude"] <- "latitude"
names(db)[names(db) == "Longitude"] <- "longitude"
names(db)[names(db) == "Tipo_trappola"] <- "trap_type"

##### 3.2. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale del Lazio e della Toscana M. Aleandri"
db$contact_person <- "Federico Romiti"
db$contact_person_email <- "federico.romiti@izslt.it"
db$life_stage <- "adults"
db$EPSG <- "4326"

db$id_trap <- NA
# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA
# db$WNV_test <- NA
# db$n_pool <- NA


##### 3.3. date validation #####
# dd/mm/yyyy throughout the 2009-2023 series, confirmed on inspection
db <- db %>%
  dplyr::mutate(date = lubridate::dmy(date))

db <- db %>%
  dplyr::mutate(week = lubridate::week(date))

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2009, 2023)) %>%
  nrow()

n_date_unparsed
n_date_out_of_range

db <- db %>%
  dplyr::mutate(
    date_note = ifelse(is.na(date), "Date could not be parsed; flagged for review.", NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(date_note) ~ paste(note, date_note, sep = " | "),
      is.na(note) & !is.na(date_note)  ~ date_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-date_note)

# manual correction from excel
manual_note_matches <- manual_corrections %>%
  dplyr::mutate(row_n = dplyr::row_number()) %>%
  dplyr::rename(date = date_key)

db <- db %>% dplyr::mutate(.row_id = dplyr::row_number())

manual_hits <- db %>%
  dplyr::inner_join(manual_note_matches, by = "date", relationship = "many-to-many") %>%
  dplyr::filter(abs(latitude - lat_key) < 0.1, abs(longitude - lon_key) < 0.1) %>%
  dplyr::group_by(row_n) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(.row_id, manual_note = sprintf("Value reconciled during 2023 FEM/West Nile merge: %s.", correction_note))

length(unique(manual_hits$.row_id))
nrow(manual_corrections)

db <- db %>%
  dplyr::left_join(manual_hits, by = ".row_id") %>%
  dplyr::mutate(
    note = dplyr::case_when(
      !is.na(note) & !is.na(manual_note) ~ paste(note, manual_note, sep = " | "),
      is.na(note) & !is.na(manual_note)  ~ manual_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-.row_id, -manual_note)


##### 3.4. check duplicates #####
dup_profile <- db %>%
  dplyr::group_by(date, Region, Province, Municipality, latitude, longitude) %>%
  dplyr::summarise(
    n_raw_rows = dplyr::n(),
    n_distinct_rows = dplyr::n_distinct(paste(trap_type, Cx_pipiens_f, Cx_pipiens_m, Cx_pipiens_tot)),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_raw_rows > 1)

nrow(dup_profile)
sum(dup_profile$n_distinct_rows == 1)
sum(dup_profile$n_distinct_rows > 1)

dup_profile %>%
  dplyr::filter(n_distinct_rows > 1) %>%
  print(n = Inf)

# Stage 1: zero-catch rows accompanying a real catch are export
# artefacts, not independent traps (verified: always exact duplicates
# within the group). Logged before dropping, since this needs the raw
# f/m/tot columns that won't exist after merging.
db <- db %>%
  dplyr::group_by(date, Region, Province, Municipality, latitude, longitude) %>%
  dplyr::mutate(
    has_nonzero_catch = any(Cx_pipiens_f > 0 | Cx_pipiens_m > 0),
    n_zero_siblings = sum(has_nonzero_catch & Cx_pipiens_f == 0 & Cx_pipiens_m == 0)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is_zero_artifact = has_nonzero_catch & Cx_pipiens_f == 0 & Cx_pipiens_m == 0)

stage1_dropped <- db %>%
  dplyr::filter(is_zero_artifact) %>%
  dplyr::transmute(
    region = "toscana_lazio", check = "duplicate_export_artifact",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = "0",
    reason = "Duplicate zero-catch row from the same session (export artefact)."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "toscana_lazio") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(stage1_dropped, qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::mutate(
    survivor_note1 = ifelse(!is_zero_artifact & n_zero_siblings > 0,
                            sprintf("%d duplicate zero-catch row(s) removed (see qc_exclusions_log.csv).", n_zero_siblings),
                            NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(survivor_note1) ~ paste(note, survivor_note1, sep = " | "),
      is.na(note) & !is.na(survivor_note1)  ~ survivor_note1,
      TRUE ~ note
    )
  ) %>%
  dplyr::filter(!is_zero_artifact) %>%
  dplyr::select(-has_nonzero_catch, -is_zero_artifact, -n_zero_siblings, -survivor_note1)

nrow(db)

# NOTE: the "Stage 2" exact-duplicate collapse that used to run here has
# been REMOVED. This region has no stable trap identifier (id_trap = NA,
# Section 3.2), so a group of records identical on every field cannot be
# confirmed as a true duplicate versus multiple distinct physical traps
# sharing one municipality-level coordinate (the same pattern confirmed,
# at much larger scale, for Sardinia: groups of 8-9 identical zero-catch
# rows recurring across many dates in the same municipality -- too
# regular to be copy-paste errors). This is now handled once, uniformly,
# for every no-id_trap region (Sardinia, Tuscany/Lazio, Campania/
# Calabria) in the national merge script (1_1_merged_db_final.R, Section
# 2.5), which flags and keeps these records with an explanatory note
# instead of silently collapsing them, pending confirmation from each
# region's referent (here, Federico Romiti, IZSLT).


##### 3.5. province & region full names #####
levels(as.factor(db$Province))

db <- db %>%
  dplyr::mutate(
    province_before = Province,
    Province = dplyr::recode(Province,
                             "Massa_Carrara" = "Massa-Carrara",
                             "Roma" = "Rome"
    ),
    province_note = ifelse(province_before != Province,
                           sprintf("Province standardised from '%s' to '%s'.", province_before, Province),
                           NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(province_note) ~ paste(note, province_note, sep = " | "),
      is.na(note) & !is.na(province_note)  ~ province_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-province_before, -province_note)

levels(as.factor(db$Province))

db <- db %>%
  dplyr::mutate(Region = dplyr::recode(Region, "Toscana" = "Tuscany"))

levels(as.factor(db$Region))


##### 3.6. trap type #####
levels(as.factor(db$trap_type))

db <- db %>%
  dplyr::mutate(trap_type = dplyr::recode(trap_type, "CDC" = "CDC_CO2"))
# was "CDC_LIKE_CO2" -- confirmed with IZSLT (2026): Tuscany and Lazio
# deploy CDC light traps baited with CO2, not the CDC-like variant

table(db$trap_type, useNA = "always")


##### 3.6b. sea point check #####
# Both Tuscany (Tyrrhenian coast) and Lazio (Tyrrhenian coast) have a
# substantial coastline, so a recorded coordinate can plausibly fall
# offshore. Same row-level check used for Sicilia/Sardegna/Emilia-Romagna/
# Puglia-Basilicata: only the specific reading is invalidated (set to
# NA), not the whole municipality; it is then picked up automatically by
# the existing missing-coordinate -> centroid fallback in Section 3.7.
geojson_valid <- sf::st_make_valid(geojson)

db_sf <- db %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

sea_points <- db_sf %>%
  dplyr::filter(lengths(sf::st_intersects(geometry, geojson_valid)) == 0) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

print(sea_points, n = Inf)

db <- db %>%
  dplyr::mutate(
    is_sea_point = paste(round(latitude, 3), round(longitude, 3)) %in%
      paste(round(sea_points$latitude, 3), round(sea_points$longitude, 3)),
    sea_note = ifelse(is_sea_point,
                      "Recorded coordinates fell in the sea (this specific reading only, not the whole municipality); replaced with municipality centroid.",
                      NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(sea_note) ~ paste(note, sea_note, sep = " | "),
      is.na(note) & !is.na(sea_note)  ~ sea_note,
      TRUE ~ note
    ),
    latitude = dplyr::if_else(is_sea_point, NA_real_, latitude),
    longitude = dplyr::if_else(is_sea_point, NA_real_, longitude)
  ) %>%
  dplyr::select(-is_sea_point, -sea_note)


##### 3.6c. coordinate error + genuine border cases + province-only corrections #####
# All verified against comuni.geojson (point-in-polygon match, plus
# explicit polygon-adjacency (touches()) to distinguish genuine
# coordinate errors from plausible edge-of-municipality trap locations).
# Scoped by (rounded) coordinate, not by Municipality name, since several
# of these municipalities (Aprilia, Velletri, Cisterna di Latina,
# Capalbio, Grosseto) have OTHER trap locations correctly positioned that
# a name-based correction would wrongly touch. There is no id_trap for
# this region (see Section 3.2), so coordinate is the only usable key.

# (a) GENUINE COORDINATE ERROR: declared Grosseto, but 9.87 km inside the
# (non-adjacent) municipality of Massa Marittima -- not a border case.
db <- db %>%
  dplyr::mutate(
    is_coord_error = round(latitude, 4) == round(43.015289, 4) &
      round(longitude, 4) == round(10.841541, 4),
    error_note = ifelse(is_coord_error,
                        "Recorded coordinates fell 9.87 km inside the (non-adjacent) municipality of Massa Marittima; treated as a coordinate transcription error, not a border case -- discarded and re-derived from the declared municipality's (Grosseto) centroid.",
                        NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(error_note) ~ paste(note, error_note, sep = " | "),
      is.na(note) & !is.na(error_note)  ~ error_note,
      TRUE ~ note
    ),
    latitude  = dplyr::if_else(is_coord_error, NA_real_, latitude),
    longitude = dplyr::if_else(is_coord_error, NA_real_, longitude)
  ) %>%
  dplyr::select(-is_coord_error, -error_note)

# (b) GENUINE BORDER CASES: verified adjacent (touches(), 0 km)
border_case_coords <- tibble::tribble(
  ~lat,        ~lon,         ~matched_muni,
  41.506668,   12.709295,    "Nettuno",           # Aprilia (56 rows)
  41.530476,   12.670929,    "Aprilia",           # Cisterna di Latina (7 rows)
  41.9784,     13.0254,      "Agosta",            # Marano Equo (18 rows)
  41.692866,   12.688441,    "Genzano di Roma",   # Velletri (13 rows)
  43.344412,   11.26654,     "Siena",             # Monteriggioni (22 rows)
  42.676677,   11.552413,    "Manciano",          # Capalbio (1 row)
  42.9162,     11.1163,      "Roccastrada"        # Grosseto (1 row)
) %>%
  dplyr::mutate(lat_r = round(lat, 4), lon_r = round(lon, 4))

db <- db %>%
  dplyr::mutate(lat_r = round(latitude, 4), lon_r = round(longitude, 4)) %>%
  dplyr::left_join(border_case_coords %>% dplyr::select(lat_r, lon_r, matched_muni), by = c("lat_r", "lon_r")) %>%
  dplyr::mutate(
    border_note = ifelse(
      !is.na(matched_muni),
      sprintf("Coordinates fall within the adjacent municipality of '%s' (point-in-polygon check; confirmed touching polygons); declared Municipality ('%s') and coordinates kept as recorded -- plausible genuine edge-of-municipality trap location, not treated as an error.",
              matched_muni, Municipality),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(border_note) ~ paste(note, border_note, sep = " | "),
      is.na(note) & !is.na(border_note)  ~ border_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-lat_r, -lon_r, -matched_muni, -border_note)

# (c) PROVINCE-ONLY CORRECTIONS: Municipality and coordinates are already
# correct here (Velletri and Nettuno are both genuinely in Roma province
# per ISTAT), but a subset of their records declare Province = "Latina"
# -- a data-entry error confined to these specific coordinates, unrelated
# to the border cases above (Province is already "Rome", English, at
# this point in the script -- see Section 3.5).
province_error_coords <- tibble::tribble(
  ~lat,        ~lon,
  41.610600,   12.776200,   # Velletri, mislabelled Latina (2 rows)
  41.641700,   12.806200,   # Velletri, mislabelled Latina (5 rows)
  41.510411,   12.665847    # Nettuno, mislabelled Latina (4 rows)
) %>%
  dplyr::mutate(lat_r = round(lat, 4), lon_r = round(lon, 4))

db <- db %>%
  dplyr::mutate(lat_r = round(latitude, 4), lon_r = round(longitude, 4)) %>%
  dplyr::mutate(
    is_prov_error = paste(lat_r, lon_r) %in% paste(province_error_coords$lat_r, province_error_coords$lon_r),
    province_before2 = Province,
    Province = dplyr::if_else(is_prov_error, "Rome", Province),
    prov_error_note = dplyr::if_else(
      is_prov_error,
      sprintf("Province corrected from '%s' to 'Rome': %s is genuinely in Rome province per ISTAT, independent of any nearby border case.", province_before2, Municipality),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(prov_error_note) ~ paste(note, prov_error_note, sep = " | "),
      is.na(note) & !is.na(prov_error_note)  ~ prov_error_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-lat_r, -lon_r, -is_prov_error, -province_before2, -prov_error_note)


##### 3.7. coordinate check & centroid imputation #####
summary(db[, c("latitude", "longitude")])

tl_lat_range <- c(40.8, 44.5)
tl_lon_range <- c(9.5, 14.1)

db %>%
  dplyr::filter(!is.na(latitude) &
                  (!dplyr::between(latitude, tl_lat_range[1], tl_lat_range[2]) |
                     !dplyr::between(longitude, tl_lon_range[1], tl_lon_range[2]))) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = tl_lon_range, ylim = tl_lat_range) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

# centroid computation for missing lat and lon
geojson_coords <- geojson %>%
  sf::st_transform(crs = 3857) %>%
  sf::st_centroid() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(
    name_upper = toupper(trimws(name)),
    lon_centroid = sf::st_coordinates(geometry)[, "X"],
    lat_centroid = sf::st_coordinates(geometry)[, "Y"]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name_upper, lon_centroid, lat_centroid)

n_missing_before <- sum(is.na(db$latitude) | is.na(db$longitude) |
                          !dplyr::between(db$latitude, tl_lat_range[1], tl_lat_range[2]) |
                          !dplyr::between(db$longitude, tl_lon_range[1], tl_lon_range[2]))

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    coord_missing = is.na(latitude) | is.na(longitude),
    coord_invalid = !coord_missing &
      (!dplyr::between(latitude, tl_lat_range[1], tl_lat_range[2]) |
         !dplyr::between(longitude, tl_lon_range[1], tl_lon_range[2])),
    is_bad_coord = coord_missing | coord_invalid,
    used_centroid = is_bad_coord & !is.na(lat_centroid),
    municipality_centroid = ifelse(used_centroid, "yes", "no"),
    centroid_note = dplyr::case_when(
      used_centroid & coord_missing ~ "Coordinates were missing; derived from municipality centroid.",
      used_centroid & coord_invalid ~ "Coordinates were present but implausible (outside Tuscany/Lazio); derived from municipality centroid.",
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(centroid_note) ~ paste(note, centroid_note, sep = " | "),
      is.na(note) & !is.na(centroid_note)  ~ centroid_note,
      TRUE ~ note
    ),
    longitude = dplyr::if_else(is_bad_coord, lon_centroid, longitude),
    latitude  = dplyr::if_else(is_bad_coord, lat_centroid, latitude)
  ) %>%
  dplyr::select(-lon_centroid, -lat_centroid, -Municipality_upper, -coord_missing,
                -coord_invalid, -is_bad_coord, -used_centroid, -centroid_note)

n_centroid_imputed <- sum(db$municipality_centroid == "yes")
n_still_missing <- sum(is.na(db$latitude) | is.na(db$longitude))
n_missing_before
n_centroid_imputed
n_still_missing


##### 3.8. mosquito species values and sex #####
# every row already carries f, m AND tot (no NA in any of the three, in
# this source) with f + m == tot verified in 3.1; the total column is
# therefore always redundant with f/m and is dropped rather than kept as
# a third row, so a session is never double-counted
db <- db %>%
  tidyr::pivot_longer(
    cols = c(Cx_pipiens_f, Cx_pipiens_m, Cx_pipiens_tot),
    names_to = "type",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  dplyr::group_by(date, Region, Province, Municipality, latitude, longitude) %>%
  dplyr::filter(!(any(type %in% c("Cx_pipiens_f", "Cx_pipiens_m")) & type == "Cx_pipiens_tot")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sex = dplyr::case_when(
      type == "Cx_pipiens_f" ~ "F",
      type == "Cx_pipiens_m" ~ "M",
      TRUE ~ NA_character_
    ),
    species = "Culex pipiens"
  ) %>%
  dplyr::select(-type)

table(db$sex, useNA = "always")
summary(db$value)


##### 3.9. mosquito species (taxonomy match) #####
db <- db %>%
  dplyr::mutate(species_raw = dplyr::case_when(
    species == "Ae_albopicitus" ~ "Aedes albopictus",
    species == "Ae_detritus" ~ "Aedes detritus",
    species == "Ae_vexans" ~ "Aedes vexans",
    species == "Ae_vittatus" ~ "Aedes vittatus",
    species == "An_algeriensis" ~ "Anopheles algeriensis",
    species == "An_claviger_petranani" ~ "Anopheles claviger",
    species == "An_maculipennis" ~ "Anopheles maculipennis",
    species == "An_plumbeus" ~ "Anopheles plumbeus",
    species == "Cq_richiardii" ~ "Coquillettidia richiardii",
    species == "Cs_annulata" ~ "Culiseta annulata",
    species == "Cs_longiareolata" ~ "Culiseta longiareolata",
    species == "Cs_morsitans" ~ "Culiseta morsitans",
    species == "Cs_subochrea" ~ "Culiseta subochrea",
    species == "Cx_hortensis" ~ "Culex hortensis",
    species == "Cx_impudicus" ~ "Culex impudicus",
    species == "Cx_theileri" ~ "Culex theileri",
    species == "Oc_caspius" ~ "Aedes caspius",
    species == "Oc_detritus" ~ "Aedes detritus",
    species == "Oc_geniculatus" ~ "Aedes geniculatus",
    species == "Oc_rusticus" ~ "Aedes rusticus",
    species == "Or_pulcripalpis" ~ "Orthopodomyia pulcripalpis",
    TRUE ~ species
  )) %>%
  dplyr::select(-species)

species_not_found <- db %>%
  dplyr::anti_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(species_raw) %>%
  dplyr::distinct()

print(species_not_found)

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(-species_raw)

# same trap for each municipality?
different_trap <- db %>%
  dplyr::group_by(Municipality, date) %>%
  dplyr::summarise(n_traps_same_day = dplyr::n_distinct(latitude, longitude, trap_type), .groups = "drop") %>%
  dplyr::filter(n_traps_same_day > 1) %>%
  dplyr::arrange(dplyr::desc(n_traps_same_day))

print(different_trap, n = Inf)

different_trap_details <- db %>%
  dplyr::inner_join(different_trap %>% dplyr::select(Municipality, date), by = c("Municipality", "date")) %>%
  dplyr::group_by(Municipality, date, latitude, longitude, trap_type) %>%
  dplyr::summarise(
    total_value = sum(value, na.rm = TRUE),
    sexes_recorded = paste(unique(na.omit(sex)), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Municipality, date, latitude, longitude)

print(different_trap_details, n = Inf)


##### 3.10. order columns #####
db <- db %>%
  dplyr::select(
    year, week, date, value, Country, Region, Province, Municipality, longitude,
    latitude, municipality_centroid, Institute, contact_person, contact_person_email, id_trap, trap_type,
    # volume, substrate, larvicide_presence, larvicide_type,
    Canonical_name, kingdom, phylum, class, order, family, genus, species, life_stage, sex, EPSG,
    note
    # WNV_test, n_pool
  )

str(db)

stopifnot(
  "Missing date after harmonisation" = all(!is.na(db$date)),
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2009-2023 range" = all(dplyr::between(db$year, 2009, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


#### 4. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "toscana_lazio_samplings_clean.csv"), row.names = FALSE)