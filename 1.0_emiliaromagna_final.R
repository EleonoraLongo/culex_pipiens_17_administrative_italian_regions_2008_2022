# =============================================================================
# DATA HARMONISATION — Emilia-Romagna mosquito surveillance (2013–2023)
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
# Purpose   : Cleans and harmonises the IZS Lombardia ed Emilia-Romagna
#             entomological surveillance dataset for Emilia-Romagna (2013–2023).
#             Produces a standardised CSV compatible with the national
#             harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Emilia-Romagna/WNV_ER_mosq_13_18.xlsx
#             ../izs_data/Emilia-Romagna/WNV_ER_mosq_19_22.xlsx
#             ../izs_data/Emilia-Romagna/Cxpi_ER_2023.xlsx (sheet "zanzare")
#             ../izs_data/Emilia-Romagna/Cx_pi_ER.xlsx (sheet 2)
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/emiliaromagna_samplings_clean.csv
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
a <- readxl::read_excel("../izs_data/Emilia-Romagna/WNV_ER_mosq_13_18.xlsx")
b <- readxl::read_excel("../izs_data/Emilia-Romagna/WNV_ER_mosq_19_22.xlsx")
c <- readxl::read_excel("../izs_data/Emilia-Romagna/Cxpi_ER_2023.xlsx", sheet = "zanzare")

# drop pathogen-screening fields immediately on ingestion (out of scope for
# this phase) so that raw column sets are directly comparable before binding
a <- a %>% dplyr::select(-dplyr::any_of(c("tested", "WNV test")))
b <- b %>% dplyr::select(-dplyr::any_of(c("tested", "WNV test")))
c <- c %>% dplyr::select(-dplyr::any_of(c("tested", "WNV test")))

stopifnot(
  "Column structure mismatch between 2013-18 and 2019-22 raw files" =
    identical(names(a), names(b)),
  "Column structure mismatch between 2019-22 and 2023 raw files" =
    identical(names(b), names(c))
)

db <- dplyr::bind_rows(a, b, c)

##### 1.2. geo data #####
# traps lat and lon
traps <- readxl::read_excel("../izs_data/Emilia-Romagna/Cx_pi_ER.xlsx", sheet = 2)

# municipality
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% "Emilia-Romagna")

##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. data cleaning ####
##### 2.1. add lat and lon of traps #####
db$trap_code <- toupper(db$trap_code)
traps$trap_code <- toupper(traps$trap_code)

db <- db %>%
  dplyr::left_join(
    traps %>% 
      dplyr::select(trap_code, dplyr::any_of(setdiff(names(traps), names(db)))),
    by = "trap_code"
  )

##### 2.2. columns name #####
names(db)

names(db)[names(db) == "trap_code"] <- "id_trap"
names(db)[names(db) == "pool size"] <- "value"
names(db)[names(db) == "municipality"] <- "Municipality"
names(db)[names(db) == "province"] <- "Province"
names(db)[names(db) == "trap_model"] <- "trap_type"
names(db)[names(db) == "x"] <- "longitude"
names(db)[names(db) == "y"] <- "latitude"

names(db)

##### 2.3. add columns with NA or fixed values#####
db$Region <- "Emilia-Romagna"
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale Lombardia ed Emilia-Romagna"
db$contact_person <- "Mattia Calzolari"
db$contact_person_email <- "mattia.calzolari@izsler.it"
db$life_stage <- "adults"
db$sex <- "F"
db$EPSG <- "4326"

levels(as.factor(db$sex))

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA


##### 2.4. format date ####
db$date <- lubridate::ymd(db$date)

# columns year and week
db <- db %>%
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  )

# Date validity
n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2013, 2023)) %>%
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


##### 2.3. coordinate check #####
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, 43.7, 45.2) | !dplyr::between(longitude, 9.1, 12.9)) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(9.1, 12.9), ylim = c(43.7, 45.2)) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(id_trap, Municipality) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Municipality) %>% 
  print(n = Inf)


##### 2.3b. sea point check #####
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


###### add missing lat and lon ######
# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

db %>% 
  dplyr::filter(is.na(Municipality))

db <- db %>%
  dplyr::mutate(Municipality = toupper(trimws(Municipality))) %>%
  dplyr::mutate(
    Municipality = dplyr::case_when(
      id_trap == "COMA02" | is.na(Municipality) ~ "COMACCHIO",
      Municipality == "ANZOLA EMILIA" ~ "ANZOLA DELL'EMILIA",
      Municipality == "REGGIO-EMILIA" ~ "REGGIO NELL'EMILIA",
      Municipality == "GRAGNANO TREBBIESE" ~ "GRAGNANO TREBBIENSE",
      Municipality == "ORBOLO MEZZANI" ~ "SORBOLO MEZZANI",
      Municipality == "AN PIETRO IN CASALE" ~ "SAN PIETRO IN CASALE",
      Municipality == "CREVALLCORE" ~ "CREVALCORE",
      Municipality == "S. GIOVANNI IN PERSICETO" ~ "SAN GIOVANNI IN PERSICETO",
      Municipality %in% c("FORLI`", "FORLI") ~ "FORLÌ", 
      Municipality == "ZIBELLO" ~ "POLESINE ZIBELLO",
      Municipality == "TRECASALI" ~ "SISSA TRECASALI",
      Municipality == "SANT'AGOSTINO" ~ "TERRE DEL RENO",
      Municipality %in% c("RO", "BERRA") ~ "RIVA DEL PO",
      Municipality == "MEZZANI" ~ "SORBOLO MEZZANI",
      TRUE ~ Municipality
    )
  )

# compute centroids
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

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(9.1, 12.9), ylim = c(43.7, 45.2)) +
  ggplot2::theme_bw()


##### 2.3c. genuine border cases (verified against comuni.geojson) #####
# Flagged by the national merge script's point-in-polygon check (see
# project chat log). All five confirmed as genuine border cases, not
# errors: sf::st_distance() between the declared and matched municipality
# polygons is exactly 0 for every one (i.e. the two polygons touch), not
# an eyeballed call from the zoomed plot alone -- Castel San Giovanni in
# particular looked more clear-cut on the map than the others but is
# geometrically identical to them (touching, distance 0). Municipality
# and coordinates are kept as declared; only a note documents the check.
border_case_traps <- tibble::tribble(
  ~id_trap, ~matched_muni,
  "WN073A", "Parma",
  "IZ02",   "Sarmato",
  "IZ01",   "Sorbolo Mezzani",
  "IZ06",   "Modena",
  "IZ04",   "Correggio"
)

db <- db %>%
  dplyr::left_join(border_case_traps, by = "id_trap") %>%
  dplyr::mutate(
    border_note = ifelse(
      !is.na(matched_muni),
      sprintf("Coordinates fall within the adjacent municipality of '%s' (point-in-polygon check; confirmed touching polygons, distance 0 m); declared Municipality ('%s') and coordinates kept as recorded -- plausible genuine edge-of-municipality trap location, not treated as an error.",
              matched_muni, Municipality),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(border_note) ~ paste(note, border_note, sep = " | "),
      is.na(note) & !is.na(border_note)  ~ border_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-matched_muni, -border_note)


##### 2.4. province complete names #####
levels(as.factor(db$Province))

db <- db %>%
  dplyr::mutate(
    Province = dplyr::recode(Province,
                             "BO" = "Bologna",
                             "MO" = "Modena",
                             "FE" = "Ferrara",
                             "PR" = "Parma",
                             "RE" = "Reggio nell'Emilia",
                             "PC" = "Piacenza",
                             "FC" = "Forlì-Cesena",
                             "RN" = "Rimini",
                             "RA" = "Ravenna"
    ),
    # "Reggio Emilia" (common form) vs "Reggio nell'Emilia" (official ISTAT
    # name) is a naming-only mismatch, confirmed by the national merge
    # script's province consistency check -- not a geographic error, see
    # chat log. Recorded here for every affected row rather than corrected
    # silently.
    reggio_note = dplyr::if_else(
      Province == "Reggio nell'Emilia",
      "Province recorded using the official ISTAT name 'Reggio nell'Emilia' (commonly written 'Reggio Emilia').",
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(reggio_note) ~ paste(note, reggio_note, sep = " | "),
      is.na(note) & !is.na(reggio_note)  ~ reggio_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-reggio_note)

levels(as.factor(db$Province))


##### 2.5. municipality #####
italian_title_case <- function(x) {
  minuscole <- c("di", "da", "del", "della", "dello", "degli", "delle", 
                 "e", "in", "con", "su", "per", "tra", "fra", "all'")
  x_titled <- stringr::str_to_title(x)
  words <- stringr::str_split(x_titled, " ", simplify = TRUE)
  for(i in seq_len(ncol(words))) {
    if(i > 1 && tolower(words[1, i]) %in% minuscole) {
      words[1, i] <- tolower(words[1, i])
    }
  }
  paste(words, collapse = " ")
}

db$Municipality <- as.character(db$Municipality)
db$Municipality <- sapply(db$Municipality, italian_title_case)


##### 2.6. standardize trap type #####
levels(as.factor(db$trap_type))

db <- db %>%
  dplyr::mutate(
    trap_type = dplyr::case_when(
      is.na(trap_type) & year == 2018 ~ "CDC_LIKE_CO2",
      toupper(trap_type) == "CO2" ~ "CDC_LIKE_CO2",
      tolower(trap_type) == "gravid" ~ "GRAVID",
      tolower(trap_type) == "gravid*" ~ "GRAVID",
      TRUE ~ trap_type  
    )
  )

table(db$trap_type, useNA = "always")


##### 2.7. standardize species #####
levels(as.factor(db$species))

db <- db %>%
  dplyr::mutate(
    species = stringr::str_squish(species),  # trims stray leading/trailing
    # whitespace found in the raw
    # file (e.g. "Coquilletidia ",
    # "Aedes berlandi ") that would
    # otherwise silently fail both
    # this case_when and the
    # taxonomy_lookup join below
    species = dplyr::case_when(
      species == "Ae. albopictus" ~ "Aedes albopictus",
      species == "Ae. berlandi" ~ "Aedes berlandi",
      species == "Ae. cantans" ~ "Aedes cantans",
      species == "Ae. caspius" ~ "Aedes caspius",
      species == "Ae. cinereus" ~ "Aedes cinereus",
      species == "Ae. detritus" ~ "Aedes detritus",
      species == "Ae. flavescens" ~ "Aedes flavescens",
      species == "Ae. geniculatus" ~ "Aedes geniculatus",
      species == "Ae. rusticus" ~ "Aedes rusticus",
      species == "Ae. vexans" ~ "Aedes vexans",
      species == "Ae.sp" ~ "Aedes spp",
      species == "Aedes albopictus" ~ "Aedes albopictus",
      species == "Aedes berlandi" ~ "Aedes berlandi",
      species == "Aedes cantans?" ~ "Aedes cantans",
      species == "Aedes caspius" ~ "Aedes caspius",
      species == "Aedes cinereus" ~ "Aedes cinereus",
      species == "Aedes detritus" ~ "Aedes detritus",
      species == "Aedes geniculatus" ~ "Aedes geniculatus",
      species == "Aedes sp?" ~ "Aedes spp",
      species == "Aedes vexans" ~ "Aedes vexans",
      species == "An. maculipennis s.l." ~ "Anopheles maculipennis sl",
      species == "Anopheles plumbeus" ~ "Anopheles plumbeus",
      species == "Anophleses claviger" ~ "Anopheles claviger",
      species == "Coquillettidia" ~ "Coquillettidia spp",
      species == "Coquillettidia richiardi" ~ "Coquillettidia richiardii",
      species == "Cq. richiardii" ~ "Coquillettidia richiardii",
      species == "Cs. annulata" ~ "Culiseta annulata",
      species == "Cs. longiareolata" ~ "Culiseta longiareolata",
      species == "Culex modestus" ~ "Culex modestus",
      species == "Culex pipiens" ~ "Culex pipiens",
      species == "Culiseta annulata" ~ "Culiseta annulata",
      species == "Culiseta spp" ~ "Culiseta spp",
      species == "Cx. mimeticus" ~ "Culex mimeticus",
      species == "Cx. modestus" ~ "Culex modestus",
      species == "Cx. pipiens" ~ "Culex pipiens",
      species == "Ur. unguiculata" ~ "Uranotaenia unguiculata",
      TRUE ~ species  
    )
  )


db <- db %>%
  dplyr::rename(species_raw = species)

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw")

db %>%
  dplyr::filter(is.na(genus)) %>%
  dplyr::select(species_raw, id_trap, year) %>%
  dplyr::distinct() %>% 
  print(n = Inf)


##### 2.8. only numbers in value #####
REGION_NAME <- "emiliaromagna"
STRUCTURAL_PLACEHOLDERS <- c()
UNCERTAIN_PLACEHOLDERS <- c()
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


##### 2.8b. aggregate pool-level records (RESTORED) #####
n_pool_rows_before <- nrow(db)

db <- db %>%
  dplyr::group_by(id_trap, date, species_raw) %>%
  dplyr::mutate(value = sum(value, na.rm = TRUE), n_pools = dplyr::n()) %>%
  dplyr::ungroup()

table(db$n_pools)

# flag (not exclude) sessions whose SUMMED value is unusually large
large_pool_sums <- db %>%
  dplyr::filter(n_pools > 1, value > 1000) %>%
  dplyr::distinct(id_trap, date, species_raw, value, n_pools)

nrow(large_pool_sums)
print(large_pool_sums, n = Inf)
readr::write_csv(large_pool_sums, "../main_db/qc_emiliaromagna_large_pool_sums.csv")

db <- db %>%
  dplyr::distinct(id_trap, date, species_raw, .keep_all = TRUE) %>%
  dplyr::select(-n_pools, -species_raw)

n_pool_rows_before
nrow(db)
n_pool_rows_before - nrow(db)


##### 2.9. order columns #####
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
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2013-2023 range" = all(dplyr::between(db$year, 2013, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


#### 3. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "emiliaromagna_samplings_clean.csv"), row.names = FALSE)