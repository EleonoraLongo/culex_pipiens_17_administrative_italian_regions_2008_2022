# =============================================================================
# DATA HARMONISATION — Umbria & Marche mosquito surveillance (2018–2023)
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
#             for Umbria and Marche (Istituto Zooprofilattico Sperimentale
#             dell'Umbria e delle Marche "Togo Rosati"). Produces a
#             standardised CSV compatible with the national harmonised
#             mosquito surveillance database.
#
# Input     : ../izs_data/Umbria_Marche/umbria_marche_database con catturec_pipiens e PCR WND E USUTU 2020_2023.xlsx
#               (sheets 1-4: one per year, 2020-2023, wide format with a
#               column per ISO week)
#             ../izs_data/Umbria_Marche/cp_2018_2019_izsum.xlsx
#               (historical 2018-2019 update sent by IZSUM in June 2026,
#               long format, filtered here to DESGERME == "Culex pipiens")
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/umbria_marche_samplings_clean.csv
#             (../main_db/clean_data/umbria_marche_pools_clean.csv -- not
#             yet built; PCR/WNV/USUTU sheets exist in the source but are
#             not wired up here)
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
library(ISOweek)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. DATA ####
##### 1.1. samplings #####
read_year_sheet <- function(sheet_num) {
  d <- readxl::read_excel(
    "../izs_data/Umbria_Marche/umbria_marche_database con catturec_pipiens e PCR WND E USUTU 2020_2023.xlsx",
    sheet = sheet_num
  )
  colnames(d) <- d[1, ]
  d[-1, ]
}

db_2020 <- read_year_sheet(1)
db_2021 <- read_year_sheet(2)
db_2022 <- read_year_sheet(3)
db_2023 <- read_year_sheet(4)

# 2018-2019
db_2018_2019 <- readxl::read_excel("../izs_data/Umbria_Marche/cp_2018_2019_izsum.xlsx", sheet = 1)

stopifnot(
  "Unexpected structure in the 2018-2019 raw sheet -- verify sheet order/name" =
    all(c("ANNO", "DETTAGLIOTIPOCAMPIONE", "REGIONE", "PROVINCIA", "COMUNE",
          "DESGERME", "GERMEVALORE", "LATITUDINE2", "LONGITUDINE2",
          "SettimanaAnno") %in% names(db_2018_2019))
)

# wide (one column per week) to long
db_2020 <- db_2020 %>% tidyr::pivot_longer(cols = `32`:`48`, names_to = "week", values_to = "value")
db_2021 <- db_2021 %>% tidyr::pivot_longer(cols = `21`:`44`, names_to = "week", values_to = "value")
db_2022 <- db_2022 %>% tidyr::pivot_longer(cols = `20`:`47`, names_to = "week", values_to = "value")
db_2023 <- db_2023 %>% tidyr::pivot_longer(cols = `22`:`46`, names_to = "week", values_to = "value")

db_2018_2019 <- db_2018_2019 %>%
  dplyr::filter(DESGERME == "Culex pipiens") %>%
  dplyr::transmute(
    anno = ANNO,
    `Trappola utilizzata` = DETTAGLIOTIPOCAMPIONE,
    Regione = REGIONE,
    Provincia = PROVINCIA,
    Comune = COMUNE,
    Latitudine = LATITUDINE2,
    Longitudine = LONGITUDINE2,
    week = as.character(SettimanaAnno),
    value = GERMEVALORE,
    RISULTATO = RISULTATO
  )

# each year's sheet spells the same column differently; harmonise before
# binding
names(db_2021)[names(db_2021) == "ANNO"] <- "anno"
names(db_2021)[names(db_2021) == "TRAPPOLA UTILIZZATA"] <- "Trappola utilizzata"
names(db_2022)[names(db_2022) == "trappola utilizzata"] <- "Trappola utilizzata"
names(db_2023)[names(db_2023) == "trappolla utilizzata"] <- "Trappola utilizzata"
names(db_2020)[names(db_2020) == "REGIONE"] <- "Regione"
names(db_2022)[names(db_2022) == "regione"] <- "Regione"
names(db_2020)[names(db_2020) == "PROV"] <- "Provincia"
names(db_2023)[names(db_2023) == "provincia"] <- "Provincia"
names(db_2020)[names(db_2020) == "COMUNE"] <- "Comune"
names(db_2023)[names(db_2023) == "comune"] <- "Comune"
names(db_2023)[names(db_2023) == "RECODE_Latitudine"] <- "Latitudine"
names(db_2023)[names(db_2023) == "RECODE_Longitudine"] <- "Longitudine"


clean_coords <- function(df) {
  df %>%
    dplyr::mutate(
      Latitudine  = as.numeric(gsub(",", ".", trimws(as.character(Latitudine)))),
      Longitudine = as.numeric(gsub(",", ".", trimws(as.character(Longitudine))))
    )
}
db_2020      <- clean_coords(db_2020)
db_2021      <- clean_coords(db_2021)
db_2022      <- clean_coords(db_2022)
db_2023      <- clean_coords(db_2023)
db_2018_2019 <- clean_coords(db_2018_2019)

db_2018_2019 <- db_2018_2019 %>% dplyr::mutate(anno = as.character(anno))

db <- dplyr::bind_rows(db_2020, db_2021, db_2022, db_2023, db_2018_2019)

nrow(db)


##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% c("Umbria", "Marche"))


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. ADD MUNICIPALITY INFORMATION ####
names(db)[names(db) == "Comune"] <- "Municipality"
db$Municipality <- as.character(db$Municipality)

# Municipality ISTAT match diagnostic (before correction) - Case-insensitive
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match) 

db <- db %>%
  dplyr::mutate(Municipality = stringr::str_to_title(Municipality))

if (!"note" %in% names(db)) db$note <- NA_character_

muni_not_match_exact <- setdiff(trimws(db$Municipality), trimws(geojson$name))
print(muni_not_match_exact)

# corrections
corrections <- c(
  "Colli Al Metauro" = "Colli al Metauro",
  "Magliano Di Tenna" = "Magliano di Tenna",
  "Montecalvo In Foglia" = "Montecalvo in Foglia",
  "Montefiore Dell'Aso" = "Montefiore dell'Aso",
  "Ponzano Di Fermo" = "Ponzano di Fermo",
  "San Benedetto Del Tronto" = "San Benedetto del Tronto",
  "Sassocorvaro" = "Sassocorvaro Auditore"
)

municipality_before <- db$Municipality
db$Municipality <- dplyr::recode(db$Municipality, !!!corrections)
changed <- municipality_before != db$Municipality
muni_note <- ifelse(changed,
                    sprintf("Municipality corrected from '%s' to '%s'.", municipality_before, db$Municipality),
                    NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(muni_note) ~ paste(db$note, muni_note, sep = " | "),
  is.na(db$note) & !is.na(muni_note)  ~ muni_note,
  TRUE ~ db$note
)

# Municipality ISTAT match diagnostic (after correction) -- should be empty
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)


#### 3. Data cleaning ####
##### 3.1. check duplicates #####
# the 2018-2019 sheet contains
# exactly one genuine exact-duplicate pair on all 14 source columns
# (Passignano sul Trasimeno, week 39/2019, value = 0)
n_before <- nrow(db)

exact_dup <- duplicated(db)
dup_rows <- db[exact_dup, ] %>%
  dplyr::transmute(
    region = "umbria_marche", check = "exact_duplicate",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = NA, value_raw = as.character(value),
    reason = "Exact duplicate row (data-entry double-write); collapsed to one observation."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "umbria_marche") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(dup_rows, qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::distinct() %>%
  dplyr::select(-dplyr::any_of("RISULTATO"))

n_before
nrow(db)
n_before - nrow(db)


##### 3.2. change remaining column names #####
names(db)

names(db)[names(db) == "anno"] <- "year"
names(db)[names(db) == "Trappola utilizzata"] <- "trap_type"
names(db)[names(db) == "Regione"] <- "Region"
names(db)[names(db) == "Provincia"] <- "Province"
names(db)[names(db) == "Latitudine"] <- "latitude"
names(db)[names(db) == "Longitudine"] <- "longitude"


##### 3.2b. sea point check #####
# Marche has a substantial Adriatic coastline (Umbria is landlocked), so
# a recorded coordinate can plausibly fall offshore. Same row-level check
# used for Sicilia/Sardegna/Emilia-Romagna/Puglia-Basilicata/Toscana-Lazio:
# only the specific reading is invalidated (set to NA), not the whole
# municipality; it is then picked up automatically by the existing
# missing-coordinate -> centroid fallback in Section 3.3.
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


##### 3.2c. placeholder coordinate + genuine border cases (verified against comuni.geojson) #####

# (a) PLACEHOLDER COORDINATE: (43.0, 12.0) is a suspiciously round, non-
# specific value (not a real GPS reading) used for exactly 3 unrelated
# records (Umbertide x2, Gubbio x1); it happens to fall inside Città
# della Pieve purely by geographic coincidence, not because these traps
# are anywhere near it. Falls inside the region's normal coordinate
# range (42.3-44.0 / 11.8-14.0), so the existing out-of-bounds check in
# Section 3.3 would not catch it 
db <- db %>%
  dplyr::mutate(
    is_placeholder_coord = round(latitude, 4) == 43.0 & round(longitude, 4) == 12.0,
    placeholder_note = ifelse(is_placeholder_coord,
                              "Recorded coordinates (43.0, 12.0) are a round, non-specific placeholder rather than a real reading; discarded and re-derived from the municipality centroid.",
                              NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(placeholder_note) ~ paste(note, placeholder_note, sep = " | "),
      is.na(note) & !is.na(placeholder_note)  ~ placeholder_note,
      TRUE ~ note
    ),
    latitude  = dplyr::if_else(is_placeholder_coord, NA_real_, latitude),
    longitude = dplyr::if_else(is_placeholder_coord, NA_real_, longitude)
  ) %>%
  dplyr::select(-is_placeholder_coord, -placeholder_note)

# (b) GENUINE BORDER CASES: verified adjacent (touches(), 0 km) against
# comuni.geojson. Coordinates are rounded to 3 decimal places (~110 m)
# before matching, which absorbs the plain-float vs comma-decimal-string
# representation difference across sheets without needing duplicate
# table rows. Three of these (Massignano, Montelupone, Carassai) are
# scoped by coordinate rather than by name only because Fermo also needs coordinate scoping
# (only ONE of its 8 distinct coordinates borders Monte Urano; the other
# 7 correctly fall within Fermo itself), so the same matching mechanism
# is used consistently for all coordinate-based entries below.
border_case_coords <- tibble::tribble(
  ~lat,          ~lon,           ~matched_muni,
  43.054569,     13.849197,      "Cupra Marittima",      # Massignano
  43.36425,      13.54945,       "Recanati",              # Montelupone
  43.025892,     13.716019,      "Montefiore dell'Aso",   # Carassai
  43.134571,     13.699389,      "Fermo",                 # Ponzano di Fermo
  43.18655,      13.701070,      "Monte Urano"            # Fermo (this one coordinate only)
) %>%
  dplyr::mutate(lat_r = round(lat, 3), lon_r = round(lon, 3))

# Remaining border cases handled by Municipality name instead of
# coordinate, since each of these has only ONE trap in the source
# (verified: no risk of touching a different, correctly-placed record
# under the same name).
single_trap_border_munis <- tibble::tribble(
  ~muni_upper,             ~matched_muni,
  "MAGLIANO DI TENNA",     "Grottazzolina",
  "COLLI AL METAURO",      "Montefelcino",
  "URBISAGLIA",            "Tolentino",
  "ACQUASPARTA",           "Spoleto",
  "PORTO RECANATI",        "Potenza Picena",
  "NUMANA",                "Sirolo",
  "FANO",                  "Mombarocchio",
  "CASTORANO",             "Offida",
  "FILOTTRANO",            "Osimo",
  "ORVIETO",               "Baschi"
)

db <- db %>%
  dplyr::mutate(
    lat_r = round(latitude, 3),
    lon_r = round(longitude, 3),
    Municipality_up_tmp = toupper(trimws(Municipality))
  ) %>%
  dplyr::left_join(border_case_coords %>% dplyr::select(lat_r, lon_r, matched_muni), by = c("lat_r", "lon_r")) %>%
  dplyr::left_join(single_trap_border_munis, by = c("Municipality_up_tmp" = "muni_upper"), suffix = c("", "_byname")) %>%
  dplyr::mutate(
    matched_muni = dplyr::coalesce(matched_muni, matched_muni_byname),
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
  dplyr::select(-lat_r, -lon_r, -matched_muni, -matched_muni_byname, -Municipality_up_tmp, -border_note)


##### 3.3. coordinate check & centroid imputation #####
um_lat_range <- c(42.3, 44.0)
um_lon_range <- c(11.8, 14.0)

summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, um_lat_range[1], um_lat_range[2]) |
                  !dplyr::between(longitude, um_lon_range[1], um_lon_range[2])) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = um_lon_range, ylim = um_lat_range) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

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
                          db$latitude == 0 | db$longitude == 0 |
                          !dplyr::between(db$latitude, um_lat_range[1], um_lat_range[2]) |
                          !dplyr::between(db$longitude, um_lon_range[1], um_lon_range[2]))

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    is_bad_coord = is.na(latitude) | is.na(longitude) |
      latitude == 0 | longitude == 0 |
      !dplyr::between(latitude, um_lat_range[1], um_lat_range[2]) |
      !dplyr::between(longitude, um_lon_range[1], um_lon_range[2]),
    municipality_centroid = ifelse(is_bad_coord, "yes", "no"),
    centroid_note = ifelse(is_bad_coord,
                           "Coordinates were missing or invalid; derived from municipality centroid.",
                           NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(centroid_note) ~ paste(note, centroid_note, sep = " | "),
      is.na(note) & !is.na(centroid_note)  ~ centroid_note,
      TRUE ~ note
    ),
    longitude = dplyr::if_else(is_bad_coord, lon_centroid, longitude),
    latitude  = dplyr::if_else(is_bad_coord, lat_centroid, latitude)
  ) %>%
  dplyr::select(-lon_centroid, -lat_centroid, -is_bad_coord, -Municipality_upper, -centroid_note)

n_centroid_imputed <- sum(db$municipality_centroid == "yes")
n_still_missing <- sum(is.na(db$latitude) | is.na(db$longitude))
n_missing_before
n_centroid_imputed
n_still_missing


##### 3.4. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale dell'Umbria e delle Marche Togo Rosati"
db$contact_person <- "Stefano Gavaudan"
db$contact_person_email <- "s.gavaudan@izsum.it"
db$life_stage <- "adults"
db$EPSG <- "4326"
db$Canonical_name <- "Common house mosquito"
db$kingdom <- "Animalia"
db$phylum <- "Arthropoda"
db$class <- "Insecta"
db$order <- "Diptera"
db$family <- "Culicidae"
db$genus <- "Culex"
db$species <- "pipiens"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA
# db$WNV_test <- NA
# db$n_pool <- NA


##### 3.5. date #####
db <- db %>%
  dplyr::mutate(
    week = as.integer(week),
    date = ISOweek::ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))
  )

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2018, 2023)) %>%
  nrow()

n_date_unparsed
n_date_out_of_range

date_note <- ifelse(is.na(db$date), "Date could not be parsed; flagged for review.", NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(date_note) ~ paste(db$note, date_note, sep = " | "),
  is.na(db$note) & !is.na(date_note)  ~ date_note,
  TRUE ~ db$note
)


##### 3.6. value #####
# Treating a blank as "not sampled" 
na_rate_by_year <- db %>%
  dplyr::mutate(is_na_value = is.na(suppressWarnings(as.numeric(value)))) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = dplyr::n(), pct_na = round(100 * mean(is_na_value), 1), .groups = "drop")
print(na_rate_by_year)

sampled_week_gaps <- db %>%
  dplyr::filter(!is.na(suppressWarnings(as.numeric(value)))) %>%
  dplyr::mutate(week_int = as.integer(week)) %>%
  dplyr::group_by(Municipality, latitude, longitude, year) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::summarise(gap = list(diff(sort(unique(week_int)))), .groups = "drop") %>%
  tidyr::unnest(gap)

print(table(sampled_week_gaps$gap))
round(100 * mean(sampled_week_gaps$gap == 1), 1)
round(100 * mean(sampled_week_gaps$gap == 2), 1)

db <- db %>% dplyr::mutate(value = as.numeric(value))

n_before_value <- nrow(db)

no_sampling_rows <- db %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::transmute(
    region = "umbria_marche", check = "no_sampling_event",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = NA_character_,
    reason = "No catch recorded for this trap-week; the alternating-week pattern across the source (see printed diagnostic above) indicates the trap was not sampled that week, not sampled-and-empty."
  )

if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(!(region == "umbria_marche" & check == "no_sampling_event")) %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(no_sampling_rows, qc_log_path, append = file.exists(qc_log_path))

db <- db %>% dplyr::filter(!is.na(value))

n_before_value
nrow(db)
n_before_value - nrow(db)


##### 3.7. province #####
levels(as.factor(db$Province))

db <- db %>%
  dplyr::mutate(Province = dplyr::recode(Province,
                                         "AN" = "Ancona",
                                         "AP" = "Ascoli Piceno",
                                         "FM" = "Fermo",
                                         "MC" = "Macerata",
                                         "PG" = "Perugia",
                                         "PU" = "Pesaro e Urbino",
                                         "TR" = "Terni"
  ))

levels(as.factor(db$Province))


##### 3.8. trap type #####
levels(as.factor(db$trap_type))

db <- db %>%
  dplyr::mutate(
    trap_type_before = trap_type,
    trap_type = dplyr::case_when(
      trap_type %in% c("trappola a CO2", "TRAPPOLA INSETTI CDC-CO2") ~ "CDC_CO2",
      trap_type == "TRAPPOLA INSETTI GRAVID TRAP"                    ~ "GRAVID",
      trap_type == "INSETTO"                                         ~ "CDC_CO2",
      trap_type == "" | is.na(trap_type)                             ~ "CDC_CO2",
      TRUE ~ trap_type
    ),
    # was "CDC_LIKE_CO2" everywhere -- confirmed with IZSUM (2026):
    # Umbria and Marche deploy CDC light traps baited with CO2, not the
    # CDC-like variant
    trap_type_note = dplyr::case_when(
      trap_type_before == "INSETTO" ~ "Trap type recorded as generic 'INSETTO'; standardised to CDC_CO2 (IZSUM-confirmed).",
      trap_type_before == "" | is.na(trap_type_before) ~ "Trap type was missing; standardised to CDC_CO2 (IZSUM-confirmed).",
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(trap_type_note) ~ paste(note, trap_type_note, sep = " | "),
      is.na(note) & !is.na(trap_type_note)  ~ trap_type_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-trap_type_before, -trap_type_note)

table(db$trap_type, useNA = "always")


##### 3.9. id_trap #####
db <- db %>%
  dplyr::group_by(Municipality, latitude, longitude) %>%
  dplyr::mutate(id_trap = dplyr::cur_group_id()) %>%
  dplyr::ungroup()


##### 3.10. sex #####
# only females have been tested/recorded since 2022; earlier years report
# an unsexed total (both sexes pooled together)
db$year <- as.numeric(as.character(db$year))
last_year <- max(db$year, na.rm = TRUE)
threshold_year <- last_year - 1

db <- db %>%
  dplyr::mutate(sex = dplyr::case_when(
    year >= threshold_year ~ "F",
    TRUE ~ "F+M"
  ))

db %>%
  dplyr::group_by(year, sex) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  print()


##### 3.11. order columns #####
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
  "Year outside expected 2018-2023 range" = all(dplyr::between(db$year, 2018, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


#### 4. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "umbria_marche_samplings_clean.csv"), row.names = FALSE)
