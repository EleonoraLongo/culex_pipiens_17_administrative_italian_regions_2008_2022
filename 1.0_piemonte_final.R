# =============================================================================
# DATA HARMONISATION — Piemonte mosquito surveillance (2014–2023)
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
# Purpose   : Cleans and harmonises the IPLA (Istituto per le Piante da Legno
#             e l'Ambiente) entomological surveillance dataset for Piedmont 
#             (2014–2023). Produces a standardised CSV compatible with the 
#             national harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx (Sheets 1 to 11)
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/piemonte_samplings_clean.csv
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
##### 1.1. samplings#####
traps_spec <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 1)

traps_spec <- traps_spec %>%
  dplyr::filter(!is.na(Stazione))

# 2014
db14 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 2)

col_names <- names(db14)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db14) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db14 <- db14 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2015
db15 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 3)

col_names <- names(db15)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db15) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db15 <- db15 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2016
db16 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 4)

col_names <- names(db16)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db16) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db16 <- db16 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2017
db17 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 5)

col_names <- names(db17)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db17) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db17 <- db17 %>%
  dplyr::filter(!is.na(Stazione)) %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2018
db18 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 6)

col_names <- names(db18)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db18) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db18 <- db18 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2019
db19 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 7)

col_names <- names(db19)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db19) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db19 <- db19 %>%
  dplyr::filter(!is.na(Stazione)) %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2020
db20 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 8)

col_names <- names(db20)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db20) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db20 <- db20 %>%
  dplyr::filter(!is.na(Stazione)) %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2021
db21 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 9)

col_names <- names(db21)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db21) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db21 <- db21 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2022
db22 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 10)

col_names <- names(db22)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db22) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db22 <- db22 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# 2023
db23 <- readxl::read_excel("../izs_data/Piemonte_Liguria/Dati_IPLA.xlsx", sheet = 11)

col_names <- names(db23)
excel_date_nums <- as.numeric(col_names[-1])
real_dates <- as.Date(excel_date_nums, origin = "1899-12-30")
names(db23) <- c("Stazione", format(real_dates, "%Y-%m-%d"))

db23 <- db23 %>%
  tidyr::pivot_longer(
    cols = -Stazione,
    names_to = "date",   
    values_to = "value" 
  )

# db tot
db <- rbind(db14, db15, db16, db17, db18, db19, db20, db21, db22, db23)


# join with traps spec
db <- db %>%
  dplyr::left_join(select(traps_spec, Stazione, Provincia, Trappola, Latitudine, Longitudine), 
                   by = "Stazione")


# ##### pools ####
# pools <- readxl::read_excel("../izs_data/Piemonte/Positività Ipla.xlsx")

##### 1.2. geo data #####
# municipality
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% "Piemonte")


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")



#### 2. Data cleaning ####
##### 2.1. change column names #####
names(db)

names(db)[names(db) == "Stazione"] <- "id_trap"
names(db)[names(db) == "Provincia"] <- "Province"
names(db)[names(db) == "Trappola"] <- "trap_type"
names(db)[names(db) == "Latitudine"] <- "latitude"
names(db)[names(db) == "Longitudine"] <- "longitude"


##### 2.2. add municipality #####
db <- db %>%
  dplyr::mutate(
    Municipality = stringr::str_remove(id_trap, " - .*$")
  )


##### 2.3. standardize latitude and longitude #####
db <- db %>%
  dplyr::mutate(
    latitude = str_remove_all(latitude, "[°*]") %>% str_trim(),
    longitude = str_remove_all(longitude, "[°*]") %>% str_trim(),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )


##### 2.4. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Region <- "Piedmont"
db$Institute <- "Istituto per le Piante da Legno e l'Ambiente"
db$contact_person <- "Andrea Mosca"
db$contact_person_email <- "mosca@ipla.org"
db$Canonical_name <- "Common house mosquito"
db$kingdom <- "Animalia"
db$phylum <- "Arthropoda"
db$class <- "Insecta"
db$order <- "Diptera"
db$family <- "Culicidae"
db$genus <- "Culex"
db$species <- "pipiens"
db$life_stage <- "adults"
db$sex <- "F"
db$EPSG <- "4326"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA


##### 2.5. format date #####
db$date <- lubridate::ymd(db$date)

if (!"note" %in% names(db)) db$note <- NA_character_

# columns year and week
db <- db %>%
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  )

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2014, 2023)) %>%
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


##### 2.6. coordinate check #####
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, 44.0, 46.5) | !dplyr::between(longitude, 6.5, 9.1)) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(6.5, 9.1), ylim = c(44.0, 46.5)) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

##### 2.6b. coordinate error + genuine border cases (verified against comuni.geojson) #####
# Scoped by id_trap (the original "Stazione" text), which uniquely
# identifies each physical station -- no risk of touching a different,
# correctly-placed station under the same municipality name (relevant
# for Turin, which has 7 stations; only "Torino - Vallere" is affected).

# (a) GENUINE COORDINATE ERROR: not adjacent (6.6 km away, not touching)
db <- db %>%
  dplyr::mutate(
    is_coord_error = id_trap == "Occhieppo Superiore",
    error_note = ifelse(is_coord_error,
                        "Recorded coordinates fell 6.6 km inside the (non-adjacent) municipality of Vigliano Biellese; treated as a coordinate transcription error, not a border case -- discarded and re-derived from the declared municipality's centroid.",
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

# (b) GENUINE BORDER CASES: verified adjacent (touches(), 0 km). "Cerano
# - Sud" is a cross-REGIONAL case (Piedmont/Lombardy, along the Ticino
# river) -- the only one of these five not confined to Piedmont itself.
border_case_traps <- tibble::tribble(
  ~id_trap,                ~matched_muni,       ~matched_region,
  "Beura Cardezza",        "Villadossola",      "Piedmont",
  "Capriglio",             "Montafia",          "Piedmont",
  "Torino - Vallere",      "Moncalieri",        "Piedmont",
  "Cerano - Sud",          "Cassolnovo",        "Lombardy"
)

db <- db %>%
  dplyr::left_join(border_case_traps, by = "id_trap") %>%
  dplyr::mutate(
    border_note = ifelse(
      !is.na(matched_muni),
      sprintf("Coordinates fall within the adjacent municipality of '%s'%s (point-in-polygon check; confirmed touching polygons); declared Municipality ('%s') and coordinates kept as recorded -- plausible genuine edge-of-municipality trap location, not treated as an error.",
              matched_muni,
              ifelse(matched_region == "Piedmont", "", paste0(", ", matched_region)),
              Municipality),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(border_note) ~ paste(note, border_note, sep = " | "),
      is.na(note) & !is.na(border_note)  ~ border_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-matched_muni, -matched_region, -border_note)


##### 2.7. municipality name check & centroid imputation #####

# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# corrections
muni_lookup <- tibble::tribble(
  ~raw,                       ~clean,                          ~type,
  "S. ALBANO STURA",          "SANT'ALBANO STURA",             "typo",
  "S. AMBROGIO DI TORINO",    "SANT'AMBROGIO DI TORINO",       "typo",
  "S. BENIGNO CANAVESE",      "SAN BENIGNO CANAVESE",          "typo",
  "S. MAURO",                 "SAN MAURO TORINESE",            "typo",
  "S. SALVATORE MONFERRATO",  "SAN SALVATORE MONFERRATO",      "typo",
  "CASALE",                   "CASALE MONFERRATO",             "typo",
  "VILLARDORA",               "VILLAR DORA",                   "typo",
  "BEURA CARDEZZA",           "BEURA-CARDEZZA",                "typo",
  "AGLIANO",                  "AGLIANO TERME",                 "typo",
  "BALDISSERO",               "BALDISSERO D'ALBA",             "typo",
  "CASTAGNOLE LANZE",         "CASTAGNOLE DELLE LANZE",        "typo",
  "GATTICO VERUNO",           "GATTICO-VERUNO",                "merged",
  "LEINÌ",                    "LEINI",                         "typo",
  "REVIGLIASCO",              "REVIGLIASCO D'ASTI",            "typo",
  "VENARIA",                  "VENARIA REALE",                 "typo",
  "BELLINZAGO",               "BELLINZAGO NOVARESE",           "typo",
  "CAMAGNA",                  "CAMAGNA MONFERRATO",            "typo"
)

db <- db %>%
  dplyr::mutate(Municipality_up = toupper(trimws(Municipality))) %>%
  dplyr::left_join(muni_lookup, by = c("Municipality_up" = "raw")) %>%
  dplyr::mutate(
    muni_note = dplyr::case_when(
      type == "typo" ~ sprintf("Municipality typo corrected from '%s' to '%s'.", Municipality_up, clean),
      type == "merged" ~ sprintf("'%s' is a historical municipality name; merged into '%s'.", Municipality_up, clean),
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(muni_note) ~ paste(note, muni_note, sep = " | "),
      is.na(note) & !is.na(muni_note)  ~ muni_note,
      TRUE ~ note
    ),
    Municipality = dplyr::coalesce(clean, Municipality_up)
  ) %>%
  dplyr::select(-Municipality_up, -clean, -type, -muni_note)

muni_not_match_after <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match_after)
print(muni_not_match_after)

# compute centroids
geojson_piem_coords <- geojson |>
  sf::st_transform(crs = 3857) |>
  sf::st_centroid() |>
  sf::st_transform(crs = 4326) |>
  dplyr::mutate(
    lon_centroid = sf::st_coordinates(geometry)[, "X"],
    lat_centroid = sf::st_coordinates(geometry)[, "Y"],
    name_upper = toupper(trimws(name))
  ) |>
  sf::st_drop_geometry() %>%
  dplyr::select(name_upper, lon_centroid, lat_centroid)

n_missing_before <- sum(is.na(db$latitude) | is.na(db$longitude))

db <- db %>%
  dplyr::left_join(geojson_piem_coords, by = c("Municipality" = "name_upper")) %>%
  dplyr::mutate(
    used_centroid = is.na(latitude) & !is.na(lat_centroid),
    municipality_centroid = ifelse(used_centroid, "yes", "no"),
    centroid_note = ifelse(used_centroid,
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
  dplyr::select(-lon_centroid, -lat_centroid, -used_centroid, -centroid_note)

n_centroid_imputed <- sum(db$municipality_centroid == "yes")
n_still_missing <- sum(is.na(db$latitude) | is.na(db$longitude))
n_missing_before
n_centroid_imputed
n_still_missing


##### 2.8. Province long name #####
db <- db %>%
  dplyr::mutate(Province = dplyr::recode(Province,
                                         "AL" = "Alessandria",
                                         "AT" = "Asti",
                                         "BI" = "Biella",
                                         "CN" = "Cuneo",
                                         "NO" = "Novara",
                                         "TO" = "Turin",
                                         "VB" = "Verbano-Cusio-Ossola",
                                         "VC" = "Vercelli"
  ))


#####pools#####
# pools1 <- pools %>%
#   dplyr::mutate(date = as.Date(data)) %>%
#   dplyr::select(località, date, `pool analizzati`) %>%
#   dplyr::group_by(località, date) %>%
#   dplyr::summarise(n_pool = sum(`pool analizzati`, na.rm = TRUE), .groups = "drop")
# 
# db <- db %>%
#   dplyr::left_join(pools1, by = c("Municipality" = "località", "date" = "date")) %>%
#   dplyr::mutate(WNV_test = ifelse(!is.na(n_pool), "yes", NA_character_))

# db$WNV_test <- NA
# db$n_pool <- NA


##### 2.9. standardize value #####
levels(as.factor(db$value))

REGION_NAME <- "piemonte"
STRUCTURAL_PLACEHOLDERS <- c("-")
UNCERTAIN_PLACEHOLDERS <- c("n.d.", "nd", "ND")
EXTRACT_DIGITS <- FALSE

value_raw_before <- as.character(db$value)

is_structural <- !is.na(value_raw_before) &
  toupper(trimws(value_raw_before)) %in% toupper(STRUCTURAL_PLACEHOLDERS)
is_uncertain <- !is.na(value_raw_before) &
  toupper(trimws(value_raw_before)) %in% toupper(UNCERTAIN_PLACEHOLDERS)
is_blank <- is.na(value_raw_before) | trimws(value_raw_before) == ""
value_numeric <- if (EXTRACT_DIGITS) {
  suppressWarnings(as.numeric(stringr::str_extract(value_raw_before, "\\d+")))
} else {
  suppressWarnings(as.numeric(value_raw_before))
}
is_unparseable_other <- !is_structural & !is_uncertain & !is_blank & is.na(value_numeric)

no_sampling_rows <- db %>%
  dplyr::mutate(value_raw = value_raw_before) %>%
  dplyr::filter(is_structural | is_blank) %>%
  dplyr::transmute(
    region = REGION_NAME, check = "no_sampling_event",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw,
    reason = "Trap was not checked on this date (blank cell or source placeholder)."
  )

invalid_value_rows <- db %>%
  dplyr::mutate(value_raw = value_raw_before) %>%
  dplyr::filter(is_uncertain | is_unparseable_other) %>%
  dplyr::transmute(
    region = REGION_NAME, check = "invalid_value",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw,
    reason = "Value was recorded but not a usable number."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != REGION_NAME) %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(dplyr::bind_rows(no_sampling_rows, invalid_value_rows), qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::mutate(value = dplyr::if_else(is_structural | is_uncertain | is_blank | is_unparseable_other,
                                       NA_real_, value_numeric)) %>%
  dplyr::filter(!is.na(value))

levels(as.factor(db$value))


##### 2.10. standardize trap type ####
levels(as.factor(db$trap_type))

db$trap_type <- as.factor(db$trap_type)

levels(db$trap_type)[levels(db$trap_type) == "CDC-CO2"] <- "CDC_CO2"
levels(db$trap_type)[levels(db$trap_type) == "BG-CO2/Lure"] <- "BG_CO2"

levels(as.factor(db$trap_type))


##### 2.11. order columns #####
db <- db %>% 
  dplyr::select(
    year, week, date, value, Country, Region, Province, Municipality, longitude,
    latitude, municipality_centroid, Institute, contact_person, contact_person_email, id_trap, trap_type,
    # volume, substrate, larvicide_presence, larvicide_type, 
    Canonical_name, kingdom,
    phylum, class, order, family, genus, species, life_stage, sex, EPSG, note 
    # WNV_test, n_pool
  )

str(db)

stopifnot(
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2014-2023 range" = all(dplyr::between(db$year, 2014, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


# ####POOL DATA####
# #####change colmn names#####
# names(pools)
# 
# names(pools)[names(pools) == "data"] <- "date"
# names(pools)[names(pools) == "prov."] <- "Province"
# names(pools)[names(pools) == "località"] <- "Municipality"
# names(pools)[names(pools) == "specie"] <- "species"
# 
# 
# #####add columns with NA or fixed values#####
# pools$Country <- "Italy"
# pools$Region <- "Piedmont"
# pools$Institute <- "Istituto per le Piante da Legno e l'Ambiente"
# pools$contact_person <- "Andrea Mosca"
# pools$contact_person_email <- "mosca@ipla.org"
# pools$life_stage <- "adults"
# 
# pools$longitude <- NA
# pools$latitude <- NA
# pools$id_trap <- NA
# pools$sex <- NA
# pools$pool_value <- NA
# 
# 
# #####year and week#####
# pools$date <- lubridate::ymd(pools$date)
# 
# #columns year and week
# pools <- pools %>%
#   dplyr::mutate(
#     week = lubridate::week(date),
#     year = lubridate::year(date)
#   )
# 
# 
# #####Province long name#####
# pools <- pools %>%
#   dplyr::mutate(Province = dplyr::recode(Province,
#                                          "AL" = "Alessandria",
#                                          "AT" = "Asti",
#                                          "BI" = "Biella",
#                                          "CN" = "Cuneo",
#                                          "NO" = "Novara",
#                                          "TO" = "Turin",
#                                          "VB" = "Verbano-Cusio-Ossola",
#                                          "VC" = "Vercelli"
#   ))
# 
# 
# #####standardize municipality#####
# pools <- pools %>%
#   dplyr::mutate(Municipality = dplyr::recode(Municipality,
#                                              "Alessandria - Cimitero" = "Alessandria",
#                                              "Alessandria - Villa del Foro" = "Alessandria",
#                                              "Morano sul Po - Due Sture" = "Morano sul Po",
#                                              "Novara - Autoporto" = "Novara",
#                                              "Novara - Centro" = "Novara",
#                                              "Novara - Olengo" = "Novara",
#                                              "S. Mauro - Pescarito" = "San Mauro Torinese",
#                                              "S. Salvatore Monferrato" = "San Salvatore Monferrato",
#                                              "Tortona - Rivalta Scrivia" = "Tortona",
#                                              "Trino - Partecipanza" = "Trino"
#   ))
# 
# 
# #####standardize species#####
# levels(as.factor(pools$species))
# 
# pools <- pools %>%
#   dplyr::mutate(
#     species = dplyr::case_when(
#       species == "Oc. caspius" ~ "Aedes caspius",
#       species == "An. maculipennis" ~ "Anopheles maculipennis",
#       species == "Cx. modestus" ~ "Culex modestus",
#       species == "Cx. pipiens" ~ "Culex pipiens",
#       TRUE ~ species  
#     )
#   )
# 
# 
# pools <- pools %>%
#   dplyr::rename(species_raw = species)
# 
# taxonomy_lookup <- data.frame(
#   species_raw = c(
#     "Aedes caspius", "Anopheles maculipennis", "Culex modestus", "Culex pipiens"
#   ),
#   Canonical_name = c(
#     "Floodwater mosquito", "Maculipennis complex", "Modest mosquito", "Common house mosquito"
#   ),
#   kingdom = rep("Animalia", 4),
#   phylum = rep("Arthropoda", 4),
#   class = rep("Insecta", 4),
#   order = rep("Diptera", 4),
#   family = rep("Culicidae", 4),
#   genus = c(
#     "Aedes", "Anopheles", "Culex", "Culex"
#   ),
#   species = c(
#     "caspius", "maculipennis", "modestus", "pipiens"
#   ),
#   stringsAsFactors = FALSE
# )
# 
# pools <- pools %>%
#   dplyr::left_join(taxonomy_lookup, by = "species_raw")
# 
# pools <- pools %>% dplyr::select(-species_raw)
# 
# 
# #####positive test column#####
# pools <- pools %>%
#   dplyr::mutate(row_id = row_number()) %>%
#   tidyr::uncount(`pool analizzati`) %>%
#   dplyr::group_by(row_id) %>%
#   dplyr::mutate(
#     valid_positive = virus %in% c("WNV2", "WN-2/USU"),
#     WNV_positive = if_else(valid_positive & row_number() <= first(`pool positivi`), "yes", "no")
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-row_id, -valid_positive)
# 
# 
# #####add id pools#####
# pools <- pools %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(id_pool = row_number())
# 
# 
# #####order columns#####
# pools <- pools %>% 
#   dplyr::select(
#     year, week, date, Country, Region, Province, Municipality, longitude, latitude,
#     Institute, contact_person, contact_person_email, id_trap, Canonical_name,
#     kingdom, phylum, class, order, family, genus, species, life_stage, sex, id_pool,
#     pool_value, WNV_positive
#   )


#### 3. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "piemonte_samplings_clean.csv"), row.names = FALSE)

# write.csv(pools, file = paste0(outdir, "piemonte_pools_clean.csv"), row.names = FALSE)