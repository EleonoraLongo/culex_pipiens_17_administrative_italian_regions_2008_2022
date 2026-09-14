# =============================================================================
# DATA HARMONISATION — Sardinia mosquito surveillance (2015–2023)
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
# Purpose   : Cleans and harmonises the IZS Sardegna (Istituto Zooprofilattico 
#             Sperimentale della Sardegna) entomological surveillance dataset 
#             for Sardinia (2015–2023). Produces a standardised CSV compatible 
#             with the national harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/sardegna_samplings_clean.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#    2. Data Cleaning
#    3. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0. libraries and working directory ####
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### 1. DATA ####
##### 1.1. samplings #####
db15 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 1)
db16 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 2)
db17 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 3)
db18 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 4)
db19 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 5)
db20 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 6)
db21 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 7)
db22 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 8)
db23 <- readxl::read_excel("../izs_data/Sardegna/Catture Piano West Nile_Sardegna 2015-2023 (per Caputo)1.xlsx", sheet = 9)

n_before_bind <- nrow(db15) + nrow(db16) + nrow(db17) + nrow(db18) + nrow(db19) +
  nrow(db20) + nrow(db21) + nrow(db22) + nrow(db23)

db <- rbind(db15, db16, db17, db18, db19, db20, db21, db22, db23)

stopifnot(
  "Row count changed during bind -- investigate before proceeding" =
    nrow(db) == n_before_bind
)

##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")
geojson <- geojson %>% dplyr::filter(reg_name %in% "Sardegna")

##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. Data cleaning ####
##### 2.1. change column names #####
names(db)
names(db)[names(db) == "Data di cattura"] <- "date"
names(db)[names(db) == "COMUNE"] <- "Municipality"
names(db)[names(db) == "LATITUDINE"] <- "latitude"
names(db)[names(db) == "LONGITUDINE"] <- "longitude"
names(db)[names(db) == "Tipologia di trappola"] <- "trap_type"


##### 2.2. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Region <- "Sardinia"
db$Institute <- "Istituto Zooprofilattico Sperimentale della Sardegna"
db$contact_person <- "Cipriano Foxi"
db$contact_person_email <- "cipriano.foxi@izs-sardegna.it"
db$Canonical_name <- "Common house mosquito"
db$kingdom <- "Animalia"
db$phylum <- "Arthropoda"
db$class <- "Insecta"
db$order <- "Diptera"
db$family <- "Culicidae"
db$genus <- "Culex"
db$species <- "pipiens"
db$life_stage <- "adults"
db$EPSG <- "4326"
db$id_trap <- NA
db$note <- NA_character_


##### 2.3. format date #####
db$date <- lubridate::ymd(db$date)

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2015, 2023)) %>%
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

db <- db %>%
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  )


##### 2.4. municipality name check #####
# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

municipality_before <- db$Municipality
db$Municipality <- dplyr::recode(db$Municipality,
                                 "PALMAS ARBOREA (TIRIA)" = "Palmas Arborea",
                                 "S. G. SUERGIU"          = "San Giovanni Suergiu",
                                 "GALTELLI'"              = "Galtellì",
                                 "SIURGUS D."             = "Siurgus Donigala",
                                 "TORTOLI'"               = "Tortolì"
)
changed <- municipality_before != db$Municipality
muni_note <- ifelse(changed,
                    sprintf("Municipality typo corrected from '%s' to '%s'.", municipality_before, db$Municipality),
                    NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(muni_note) ~ paste(db$note, muni_note, sep = " | "),
  is.na(db$note) & !is.na(muni_note)  ~ muni_note,
  TRUE ~ db$note
)

muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)


##### 2.4b. coordinate error corrections (verified against comuni.geojson) #####
# Three declared municipalities have every single one of their records
# sharing ONE fixed, wrong coordinate (verified: no other trap exists
# under these names with a different, correct position, so it is safe to
# correct by coordinate without risking any genuinely-correct record):
#   - Palmas Arborea (119 records): falls in Oristano (touching neighbour,
#     but this specific case was already established in an earlier review
#     as a coordinate error to correct, not a border case to keep -- the
#     single shared coordinate for all 119 records is not plausible as
#     119 independent edge-of-boundary readings)
#   - Marrubiu (4 records): falls in Zeddiani, 16.5 km away -- not adjacent
#   - Dorgali (1 record): falls in Ozieri, 44.9 km away -- not adjacent
coord_error_coords <- tibble::tribble(
  ~lat,      ~lon,     ~muni_upper,
  39.8670,   8.7065,   "PALMAS ARBOREA",
  39.9901,   8.5948,   "MARRUBIU"
) 
# Dorgali handled separately below since its own municipality-name typo
# does not exist -- only the coordinate is wrong for this one record.

db <- db %>%
  dplyr::mutate(
    Municipality_up_tmp = toupper(trimws(Municipality)),
    is_coord_error = (round(latitude, 4) == round(39.8670, 4) & round(longitude, 4) == round(8.7065, 4) & Municipality_up_tmp == "PALMAS ARBOREA") |
      (round(latitude, 4) == round(39.9901, 4) & round(longitude, 4) == round(8.5948, 4) & Municipality_up_tmp == "MARRUBIU") |
      (round(latitude, 4) == round(40.6268, 4) & round(longitude, 4) == round(8.9739, 4) & Municipality_up_tmp == "DORGALI"),
    error_note = dplyr::case_when(
      Municipality_up_tmp == "PALMAS ARBOREA" & is_coord_error ~ "Recorded coordinates fell inside Oristano; all records under this municipality name share this identical coordinate (not plausible as independent edge-of-boundary readings) -- treated as a coordinate error, discarded and re-derived from the Palmas Arborea centroid.",
      Municipality_up_tmp == "MARRUBIU" & is_coord_error ~ "Recorded coordinates fell 16.5 km inside the (non-adjacent) municipality of Zeddiani; treated as a coordinate transcription error -- discarded and re-derived from the Marrubiu centroid.",
      Municipality_up_tmp == "DORGALI" & is_coord_error ~ "Recorded coordinates fell 44.9 km inside the (non-adjacent) municipality of Ozieri (isolated to a single 2020 record; the other 165 Dorgali records use a different, correct coordinate); treated as a coordinate transcription error -- discarded and re-derived from the Dorgali centroid.",
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(error_note) ~ paste(note, error_note, sep = " | "),
      is.na(note) & !is.na(error_note)  ~ error_note,
      TRUE ~ note
    ),
    latitude  = dplyr::if_else(is_coord_error, NA_real_, latitude),
    longitude = dplyr::if_else(is_coord_error, NA_real_, longitude)
  ) %>%
  dplyr::select(-Municipality_up_tmp, -is_coord_error, -error_note)


##### 2.5. coordinate check #####
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, 38.8, 41.4) | !dplyr::between(longitude, 8.1, 9.9)) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(8.1, 9.9), ylim = c(38.8, 41.4)) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()


##### 2.6. centroid imputation #####
geojson_sar_coords <- geojson |>
  sf::st_transform(crs = 3857) |>
  sf::st_centroid() |>
  sf::st_transform(crs = 4326) |>
  dplyr::mutate(
    lon_centroid = sf::st_coordinates(geometry)[, "X"],
    lat_centroid = sf::st_coordinates(geometry)[, "Y"],
    name_upper = toupper(name) 
  ) |>
  sf::st_drop_geometry() %>%
  dplyr::select(name_upper, lon_centroid, lat_centroid)

if (!"latitude" %in% names(db)) db$latitude <- NA_real_
if (!"longitude" %in% names(db)) db$longitude <- NA_real_
if (!"note" %in% names(db)) db$note <- NA_character_

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_sar_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    used_centroid_na = is.na(latitude) & !is.na(lat_centroid),
    municipality_centroid = ifelse(used_centroid_na, "yes", "no"),
    centroid_note = ifelse(used_centroid_na, 
                           "Coordinates were missing; derived from municipality centroid.", 
                           NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(centroid_note) ~ paste(note, centroid_note, sep = " | "),
      is.na(note) & !is.na(centroid_note)  ~ centroid_note,
      TRUE ~ note
    ),
    longitude = dplyr::coalesce(longitude, lon_centroid),
    latitude  = dplyr::coalesce(latitude, lat_centroid)
  ) %>%
  dplyr::select(-lon_centroid, -lat_centroid, -used_centroid_na, -centroid_note)

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  nrow()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(8.1, 9.9), ylim = c(38.8, 41.4)) +
  ggplot2::theme_bw()

# points that fall in the sea
geojson_valid <- sf::st_make_valid(geojson)

db_sf <- db %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

sea_points <- db_sf %>%
  dplyr::filter(lengths(sf::st_intersects(geometry, geojson_valid)) == 0) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Municipality_upper, latitude, longitude) %>%
  dplyr::distinct()

print(sea_points, n = Inf)

db <- db %>%
  dplyr::left_join(geojson_sar_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    is_sea_point = paste(round(latitude, 3), round(longitude, 3)) %in%
      paste(round(sea_points$latitude, 3), round(sea_points$longitude, 3)),
    municipality_centroid = ifelse(is_sea_point, "yes", municipality_centroid),
    sea_note = ifelse(is_sea_point, 
                      "Recorded coordinates fell in the sea (this specific reading only, not the whole municipality); replaced with municipality centroid.", 
                      NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(sea_note) ~ paste(note, sea_note, sep = " | "),
      is.na(note) & !is.na(sea_note)  ~ sea_note,
      TRUE ~ note
    ),
    longitude = dplyr::if_else(is_sea_point, lon_centroid, longitude),
    latitude  = dplyr::if_else(is_sea_point, lat_centroid, latitude)
  ) %>%
  dplyr::select(-lon_centroid, -lat_centroid, -is_sea_point, -sea_note, -Municipality_upper)

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(8.1, 9.9), ylim = c(38.8, 41.4)) +
  ggplot2::theme_bw()

table(db$municipality_centroid, useNA = "always")


##### 2.7 municipality #####
italian_title_case <- function(x) {
  minuscole <- c("di", "da", "del", "della", "dello", "degli", "delle", 
                 "e", "in", "con", "su", "per", "tra", "fra", "all'")
  x_titled <- str_to_title(x)
  words <- str_split(x_titled, " ", simplify = TRUE)
  for(i in seq_len(ncol(words))) {
    if(i > 1 && tolower(words[1, i]) %in% minuscole) {
      words[1, i] <- tolower(words[1, i])
    }
  }
  paste(words, collapse = " ")
}

db$Municipality <- as.character(db$Municipality)
db$Municipality <- sapply(db$Municipality, italian_title_case)

levels(as.factor(db$Municipality))

db$Municipality <- recode(db$Municipality,
                          "Tortoli'" = "Tortolì",
                          "Sant'anna Arresi" = "Sant'Anna Arresi")

levels(as.factor(db$Municipality))

##### 2.7. assign province #####
# CORRECTED (2026): 11 of the original 59 entries assigned the wrong
# province -- verified one-by-one against comuni.geojson (current ISTAT
# boundaries). Most reflect the 2016 creation of "Sud Sardegna", which
# absorbed municipalities differently than this table originally assumed
# (some ex-Cagliari municipalities moved to Sud Sardegna; some ex-Ogliastra
# municipalities moved to Nuoro instead of Sud Sardegna).
province_map <- c(
  "Abbasanta" = "Oristano", "Alghero" = "Sassari", "Arborea" = "Oristano",
  "Badesi" = "Sassari", "Bari Sardo" = "Nuoro", "Baunei" = "Nuoro",
  "Bessude" = "Sassari", "Bortigiadas" = "Sassari", "Bosa" = "Oristano",
  "Cabras" = "Oristano", "Cagliari" = "Cagliari", "Cuglieri" = "Oristano",
  "Decimoputzu" = "Sud Sardegna", "Domus De Maria" = "Sud Sardegna", "Dorgali" = "Nuoro",
  "Galtellì" = "Nuoro", "Girasole" = "Nuoro", "Guasila" = "Sud Sardegna",
  "Guspini" = "Sud Sardegna", "Lotzorai" = "Nuoro", "Lunamatrona" = "Sud Sardegna",
  "Marrubiu" = "Oristano", "Muravera" = "Sud Sardegna", "Musei" = "Sud Sardegna",
  "Nuoro" = "Nuoro", "Olbia" = "Sassari", "Oliena" = "Nuoro",
  "Olmedo" = "Sassari", "Oristano" = "Oristano", "Orosei" = "Nuoro",
  "Oschiri" = "Sassari", "Ottana" = "Nuoro", "Ozieri" = "Sassari",
  "Palau" = "Sassari", "Palmas Arborea" = "Oristano", "Pozzomaggiore" = "Sassari",
  "San Giovanni Suergiu" = "Sud Sardegna", "San Teodoro" = "Sassari", "Sanluri" = "Sud Sardegna",
  "Sant'Anna Arresi" = "Sud Sardegna", "Sassari" = "Sassari", "Serdiana" = "Sud Sardegna",
  "Serramanna" = "Sud Sardegna", "Serrenti" = "Sud Sardegna", "Siniscola" = "Nuoro",
  "Siurgus Donigala" = "Sud Sardegna", "Solarussa" = "Oristano", "Sorso" = "Sassari",
  "Stintino" = "Sassari", "Teulada" = "Sud Sardegna", "Thiesi" = "Sassari",
  "Tortolì" = "Nuoro", "Tresnuraghes" = "Oristano", "Tula" = "Sassari",
  "Usellus" = "Oristano", "Uta" = "Cagliari", "Villanova Monteleone" = "Sassari",
  "Villasimius" = "Sud Sardegna", "Zeddiani" = "Oristano"
)

db <- db %>%
  dplyr::mutate(
    Province = province_map[Municipality]
  )

sum(is.na(db$Province))

db %>%
  dplyr::filter(is.na(Province)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()


##### 2.8. divide species and sex #####
# Assess data integrity: verify if the sum of detailed sub-counts matches the reported total.
# (kept as a note on the record, not an exclusion: the pivot below trusts the
# sex-specific subcounts as recorded, so a mismatch is a QA flag, not a
# reason to drop the row.)
total_check <- db %>%
  dplyr::filter(!(is.na(`Cx pi M`) & is.na(`Cx pi F NI`) & is.na(`Cx pi F I`))) %>%
  dplyr::mutate(
    recomputed_total = rowSums(dplyr::across(c(`Cx pi M`, `Cx pi F NI`, `Cx pi F I`)), na.rm = TRUE)
  ) %>%
  dplyr::filter(recomputed_total != `Cx pi T`) %>%
  dplyr::select(Municipality, date, `Cx pi M`, `Cx pi F NI`, `Cx pi F I`, `Cx pi T`, recomputed_total)

print(total_check)

db <- db %>%
  dplyr::mutate(
    recomputed_total_check = rowSums(dplyr::across(c(`Cx pi M`, `Cx pi F NI`, `Cx pi F I`)), na.rm = TRUE),
    arithmetic_mismatch = !(is.na(`Cx pi M`) & is.na(`Cx pi F NI`) & is.na(`Cx pi F I`)) &
      recomputed_total_check != `Cx pi T`,
    mismatch_note = ifelse(arithmetic_mismatch,
                           sprintf("Reported total (%s) did not match M+F NI+F I (%s); sex-specific subcounts retained as recorded.",
                                   as.character(`Cx pi T`), recomputed_total_check),
                           NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(mismatch_note) ~ paste(note, mismatch_note, sep = " | "),
      is.na(note) & !is.na(mismatch_note)  ~ mismatch_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-recomputed_total_check, -arithmetic_mismatch, -mismatch_note)

# Process records containing detailed sex/physiological status counts.
db_detailed <- db %>%
  dplyr::filter(!(is.na(`Cx pi M`) & is.na(`Cx pi F NI`) & is.na(`Cx pi F I`))) %>%
  # Impute implicit zeros: if at least one sub-category is recorded, remaining NAs represent 0 catches.
  dplyr::mutate(
    dplyr::across(c(`Cx pi M`, `Cx pi F NI`, `Cx pi F I`), ~tidyr::replace_na(., 0))
  ) %>%
  # Reshape dataset from wide to long format for downstream epidemiological analysis.
  tidyr::pivot_longer(
    cols = c(`Cx pi M`, `Cx pi F NI`, `Cx pi F I`),
    names_to = "category",
    values_to = "value",
    values_drop_na = TRUE # Retains all records since implicit NAs were imputed to 0
  ) %>%
  # Extract and assign sex variables based on the original column nomenclature.
  dplyr::mutate(
    sex = dplyr::case_when(
      stringr::str_detect(category, "M$") ~ "M",
      stringr::str_detect(category, "F") ~ "F",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-category)

# Isolate sampling sessions lacking detailed sex/physiological breakdowns.
db_missing_details <- db %>%
  dplyr::filter(is.na(`Cx pi M`) & is.na(`Cx pi F NI`) & is.na(`Cx pi F I`))

# Preserve sampling effort for zero-catch events (essential for accurate abundance modeling).
# Explicitly generate 0-count records for both Females and Males.
db_zeros_F <- db_missing_details %>%
  dplyr::filter(`Cx pi T` == 0) %>%
  dplyr::mutate(value = 0, sex = "F")

db_zeros_M <- db_missing_details %>%
  dplyr::filter(`Cx pi T` == 0) %>%
  dplyr::mutate(value = 0, sex = "M")

# Handle positive catches lacking sex classification (unsexed specimens).
# Retain the total catch value but assign NA to the sex attribute.
db_unsexed <- db_missing_details %>%
  dplyr::filter(`Cx pi T` > 0 | is.na(`Cx pi T`)) %>%
  dplyr::mutate(value = `Cx pi T`, sex = NA_character_)

# Recombine processed subsets into the primary dataset and discard legacy wide-format columns.
db <- dplyr::bind_rows(db_detailed, db_zeros_F, db_zeros_M, db_unsexed) %>%
  dplyr::select(-`Cx pi M`, -`Cx pi F NI`, -`Cx pi F I`, -`Cx pi T`)

##### 2.8b. only numbers in value #####
# `db_unsexed` can carry value = NA when neither a total (Cx pi T) nor any
# sex-specific subcount was recorded (session effectively unreadable); these
# were previously left in `db` unfiltered and unlogged. Now excluded and
# tracked in the shared log, same as every other regional script.
REGION_NAME <- "sardegna"

no_sampling_rows <- db %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::transmute(
    region = REGION_NAME, check = "no_sampling_event",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = NA_character_,
    reason = "No total catch (Cx pi T) was recorded and no sex-specific subcounts were available for this session."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != REGION_NAME) %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(no_sampling_rows, qc_log_path, append = file.exists(qc_log_path))

db <- db %>% dplyr::filter(!is.na(value))

# Final quality control checks
sum(db$value, na.rm = TRUE)

table(db$sex, useNA = "always")

# same trap for each municipality?
different_trap <- db %>%
  dplyr::group_by(Municipality, date) %>%
  dplyr::summarise(n_traps_same_day = dplyr::n_distinct(latitude, longitude, trap_type), .groups = "drop") %>%
  dplyr::filter(n_traps_same_day > 1) %>%
  dplyr::arrange(desc(n_traps_same_day))

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


##### 2.9. order columns #####
db <- db %>% 
  dplyr::select(
    year, week, date, value, Country, Region, Province, Municipality, longitude,
    latitude, municipality_centroid, Institute, contact_person, contact_person_email, id_trap, trap_type,
    Canonical_name, kingdom, phylum, class, order, family, genus, species, life_stage, sex, EPSG,
    note
  )

str(db)

stopifnot(
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2015-2023 range" = all(dplyr::between(db$year, 2015, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude)),
  "Missing Province remains after lookup" = all(!is.na(db$Province))
)


#### 3. save clean data ####
outdir <- "../main_db/clean_data/"
write.csv(db, file = paste0(outdir, "sardegna_samplings_clean.csv"), row.names = FALSE)