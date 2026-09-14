# =============================================================================
# DATA HARMONISATION — Campania & Calabria mosquito surveillance (2022–2024)
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
# Purpose   : Cleans and harmonises the IZS Mezzogiorno entomological
#             surveillance dataset (ENTADUI exam) for Campania and Calabria
#             regions (2022–2024). Produces a standardised CSV compatible
#             with the national harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Campania_Calabria/ENTADUI202222024.csv
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/campania_calabria_samplings_clean.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#    2. Add Municipality Information
#    3. Data Cleaning
#    4. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0.Libraries and working directory ####
# libraries
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
db <- read.csv(
  "../izs_data/Campania_Calabria/calabria_campania_ENTADUI202222024.csv", 
  header = TRUE, 
  sep = ";"
)

str(db)

names(db)

db <- db %>%
  dplyr::select(
    ID_REGIONE, DATA_PREL, PRV, CITTA, DESCSOS, RISULTATO_SOSTANZA, CAP)

##### 1.2. geo data #####
# municipality
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% c("Campania", "Calabria"))


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. ADD MUNICIPALITY INFORMATION ####
geojson$name <- toupper(geojson$name) # make all caps

# check if all municipality are correct
setdiff(db$CITTA, geojson$name)

if (!"note" %in% names(db)) db$note <- NA_character_

db <- db %>%
  dplyr::mutate(
    CITTA_before = CITTA,
    CITTA = ifelse(CITTA == "CASSANO ALLO IONIO", "CASSANO ALL'IONIO", CITTA),
    muni_note = ifelse(CITTA_before != CITTA,
                       sprintf("Municipality typo corrected from '%s' to '%s'.", CITTA_before, CITTA),
                       NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(muni_note) ~ paste(note, muni_note, sep = " | "),
      is.na(note) & !is.na(muni_note)  ~ muni_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-CITTA_before, -muni_note)

setdiff(db$CITTA, geojson$name)

# join 
a <- geojson %>% 
  dplyr::select(name, prov_name)

names(a)[names(a) == "name"] <- "CITTA"

db <- dplyr::left_join(db, a, by = "CITTA")


#### 3. Data cleaning ####
##### 3.1. remove unused columns #####
db <- db %>% 
  dplyr::select(-PRV, -geometry)


##### 3.2. add columns with NA #####
db$id_trap <- NA
# db$n_pool <- NA
# db$WNV_test <- NA
# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA

##### 3.3. columns name #####
names(db)

names(db)[names(db) == "ID_REGIONE"] <- "Region"
names(db)[names(db) == "DATA_PREL"] <- "date"
names(db)[names(db) == "CITTA"] <- "Municipality"
names(db)[names(db) == "RISULTATO_SOSTANZA"] <- "value"
names(db)[names(db) == "prov_name"] <- "Province"


##### 3.4. new columns #####
db$trap_type <- "CDC_CO2"
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale del Mezzogiorno"
db$contact_person <- "Claudio de Martinis"
db$contact_person_email <- "claudio.demartinis@izsmportici.it"
db$life_stage <- "adults"
db$EPSG <- "4326"

#####  3.5. format date #####
db$date <- lubridate::dmy(db$date)

# columns year and week
db <- db %>%
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  )

# Date validity
n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2022, 2024)) %>%
  nrow()

n_date_unparsed
n_date_out_of_range

if (!"note" %in% names(db)) db$note <- NA_character_

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


##### 3.6. add lat and lon #####
# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# extract coordinates of municipality centroids
geojson_coords <- geojson %>%
  sf::st_transform(crs = 3857) %>%
  sf::st_centroid() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(
    name = toupper(trimws(name)),
    lon_centroid = sf::st_coordinates(geometry)[, "X"],
    lat_centroid = sf::st_coordinates(geometry)[, "Y"]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name, lon_centroid, lat_centroid)

if (!"latitude" %in% names(db)) db$latitude <- NA_real_
if (!"longitude" %in% names(db)) db$longitude <- NA_real_
if (!"note" %in% names(db)) db$note <- NA_character_

n_missing_before <- sum(is.na(db$latitude) | is.na(db$longitude))

db <- db %>%
  dplyr::left_join(geojson_coords, by = c("Municipality" = "name")) %>%
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

# same trap for each municipality?
different_trap <- db %>%
  dplyr::filter(DESCSOS == "TOTALE INSETTI") %>%
  dplyr::count(Municipality, date, name = "n_trap_same_day") %>%
  dplyr::filter(n_trap_same_day > 1)

different_trap %>% 
  dplyr::as_tibble() %>% 
  print(n = Inf)

different_trap_details <- db %>%
  dplyr::filter(DESCSOS == "TOTALE INSETTI") %>%
  dplyr::group_by(Municipality, date) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Municipality, date, DESCSOS, value) %>%
  dplyr::arrange(Municipality, date)

print(different_trap_details, n = Inf)


##### 3.7. coordinate check #####
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!between(latitude, 37.9, 41.5) | !between(longitude, 15.6, 16.5)) %>%
  dplyr::select(Municipality, latitude, longitude)

ggplot2::ggplot() +
  geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +  # municipality polygons
  geom_point(data = db, aes(x = longitude, y = latitude),
             color = "red", size = 1.5, alpha = 0.6) +
  coord_sf(xlim = c(13.5, 17.5), ylim = c(37.5, 41.5)) +
  theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

db_sf <- db %>%
  dplyr::filter(!is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


##### 3.8. divide species and sex #####
levels(as.factor(db$DESCSOS))

# rows with no exam category recorded at all (DESCSOS missing) -- distinct
# from "TOTALE INSETTI"-only sessions handled below.
missing_descsos_rows <- db %>%
  dplyr::filter(is.na(DESCSOS)) %>%
  dplyr::transmute(
    region = "campania_calabria", check = "missing_exam_category",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = as.character(value),
    reason = "No exam category (DESCSOS) recorded; likely an analysis not yet reported."
  )

db <- db %>%
  dplyr::filter(!is.na(DESCSOS))

n_sessions <- db %>% dplyr::distinct(Municipality, date) %>% nrow()

n_filtered_sessions <- db %>% 
  dplyr::filter(DESCSOS != "TOTALE INSETTI") %>% 
  dplyr::distinct(Municipality, date) %>% 
  nrow()

n_sessions
n_filtered_sessions 
n_totale_insetti_excluded <- n_sessions - n_filtered_sessions 

totale_insetti_excluded_rows <- db %>%
  dplyr::filter(DESCSOS == "TOTALE INSETTI") %>%
  dplyr::group_by(Municipality, date) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    region = "campania_calabria", check = "unspecified_totale_insetti_only",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = as.character(value),
    reason = "Session reported only an unspecified insect total, no species-level breakdown."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "campania_calabria") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(dplyr::bind_rows(missing_descsos_rows, totale_insetti_excluded_rows), qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::filter(DESCSOS != "TOTALE INSETTI") %>%
  dplyr::mutate(
    species = stringr::str_trim(str_remove(DESCSOS, "\\s+(FEMMINA/E|MASCHIO/I)$")),
    sex = dplyr::case_when(
      stringr::str_detect(DESCSOS, "FEMMINA/E$") ~ "F",
      stringr::str_detect(DESCSOS, "MASCHIO/I$") ~ "M",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-DESCSOS)

levels(as.factor(db$sex))

# standardize species
levels(as.factor(db$species))

db <- db %>%
  dplyr::mutate(
    species = dplyr::case_when(
      species == "" ~ NA_character_,
      species == "ALTRI INSETTI" ~ "OTHER INSECTS",
      species == "CULEX SP." ~ "CULEX SP",
      species == "CULEX SP.FEMMINA/E" ~ "CULEX SP",
      species == "CULICIDI" ~ "CULICIDAE",
      species == "OCHLEROTATUS CASPIUS" ~ "AEDES CASPIUS",
      species == "OCHLEROTATUS DETRITUS" ~ "AEDES DETRITUS",
      TRUE ~ species  
    )
  )

levels(as.factor(db$species))

# all taxonomic columns
db <- db %>%
  dplyr::rename(species_raw = species)

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = c("species_raw" = "species_raw"))

db <- db %>% dplyr::select(-species_raw)

levels(as.factor(db$species))


##### 3.9. location name formatting #####
db$Region <- as.character(db$Region)
db$Region <- stringr::str_to_title(db$Region)

italian_title_case <- function(x) {
  lower_case <- c("di", "da", "del", "della", "dello", "degli", "delle", 
                 "e", "in", "con", "su", "per", "tra", "fra", "all'")
  x_titled <- stringr::str_to_title(x)
  words <- stringr::str_split(x_titled, " ", simplify = TRUE)
  for(i in seq_len(ncol(words))) {
    if(i > 1 && tolower(words[1, i]) %in% lower_case) {
      words[1, i] <- tolower(words[1, i])
    }
   }
  paste(words, collapse = " ")
}

db$Municipality <- as.character(db$Municipality)
db$Municipality <- sapply(db$Municipality, italian_title_case)


##### 3.10. only numbers in abundance #####
REGION_NAME <- "campania_calabria"
STRUCTURAL_PLACEHOLDERS <- c()
UNCERTAIN_PLACEHOLDERS <- c()
EXTRACT_DIGITS <- TRUE

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
    reason = "Value contained no extractable number."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != REGION_NAME) %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(dplyr::bind_rows(no_sampling_rows, invalid_value_rows), qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::mutate(
    value = dplyr::if_else(is_structural | is_uncertain | is_blank | is_unparseable_other,
                           NA_integer_, as.integer(value_numeric))
  ) %>%
  dplyr::filter(!is.na(value))


##### 3.11. order columns #####
db <- db %>% 
  dplyr::select(
    year, week, date, value, Country, Region, Province, Municipality, longitude,
    latitude, municipality_centroid, Institute, contact_person, contact_person_email, id_trap, trap_type, 
    # volume,substrate, larvicide_presence, larvicide_type, 
    Canonical_name, kingdom, phylum,
    class, order, family, genus, species, life_stage, sex, EPSG, note
    # WNV_test, n_pool
  )


str(db)

stopifnot(
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2022-2024 range" = all(dplyr::between(db$year, 2022, 2024)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


#### 4. save clean data ####
outdir <- "../main_db/clean_data/"
write.csv(db, file = paste0(outdir, "campania_calabria_samplings_clean.csv"), row.names = FALSE)
