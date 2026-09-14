# =============================================================================
# DATA HARMONISATION — Puglia & Basilicata mosquito surveillance (2020–2022)
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
# Purpose   : Cleans and harmonises the IZS Puglia e Basilicata (IZSPB)
#             entomological surveillance dataset (2020–2022). Produces a
#             standardised CSV compatible with the national harmonised
#             mosquito surveillance database. New script (no prior
#             version existed for this region).
#
# Input     : ../izs_data/Puglia-Basilicata/Cpipiens_IZSPB_20202022.xlsx
#               (sheets "tabella 2020", "tabella_2021", "tabella_2022")
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/puglia_basilicata_samplings_clean.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#    2. Data Cleaning
#    3. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0. Libraries and working directory ####
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(sf)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. DATA ####
##### 1.1. samplings (2020-2022) #####
db_2020 <- readxl::read_excel("../izs_data/Puglia_Basilicata/C-pipiens IZSPB, 2020-2022.xlsx",
                              sheet = "tabella 2020")
db_2021 <- readxl::read_excel("../izs_data/Puglia_Basilicata/C-pipiens IZSPB, 2020-2022.xlsx",
                              sheet = "tabella_2021")
db_2022 <- readxl::read_excel("../izs_data/Puglia_Basilicata/C-pipiens IZSPB, 2020-2022.xlsx",
                              sheet = "tabella_2022")

##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% c("Puglia", "Basilicata"))

##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. data cleaning ####
##### 2.1. harmonise raw column names & bind #####
names(db_2020)
names(db_2021)
names(db_2022)

db_2020 <- db_2020 %>%
  dplyr::rename(
    Institution   = Istituzione,
    Region        = Regione,
    Province      = provincia,
    date_raw      = `DATA CATTURA`,
    Municipality  = COMUNE,
    Coordinate    = COORDINATE,
    trap_type_raw = `TRAP.`,
    Total_raw     = TOT,
    Sesso_F       = F,
    Sesso_M       = M,
    Referenti     = Referenti
  )

db_2021 <- db_2021 %>%
  dplyr::rename(
    Institution   = Istituzione,
    Region        = Regione,
    Province      = Provincia,
    date_raw      = `Data Prelievo`,
    Municipality  = Comune,
    Coordinate    = Coordinate,
    trap_type_raw = Traps,
    Total_raw     = TOTALE,
    Referenti     = Referenti
  )

db_2022 <- db_2022 %>%
  dplyr::rename(
    Institution   = Istituzione,
    Region        = Regione,
    Province      = Provincia,
    date_raw      = `Data Prelievo`,
    Municipality  = Comune,
    Coordinate    = Coordinate,
    trap_type_raw = traps,
    Total_raw     = TOTALE,
    Referenti     = Referenti
  )

n_before_bind <- nrow(db_2020) + nrow(db_2021) + nrow(db_2022)

db <- dplyr::bind_rows(db_2020, db_2021, db_2022)

stopifnot(
  "Row count changed during bind_rows() -- investigate before proceeding" =
    nrow(db) == n_before_bind
)


##### 2.2. filter to culex pipiens records (drops summary rows) #####
# both the 2021 and 2022 sheets end with a spreadsheet grand-total row
# (blank identifying fields, only summed counts populated); filtering on
# a trimmed, exact Specie match removes these two rows 
db <- db %>%
  dplyr::mutate(Specie = trimws(Specie))

species_not_culex_pipiens <- db %>%
  dplyr::filter(is.na(Specie) | Specie != "Culex pipiens") %>%
  dplyr::select(Institution, Region, Municipality, date_raw, Specie, Total_raw)

print(species_not_culex_pipiens)

species_excluded_rows <- species_not_culex_pipiens %>%
  dplyr::transmute(
    region = "puglia_basilicata",
    check = dplyr::if_else(is.na(Specie), "reference_fragment", "off_target_species"),
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date = as.Date(date_raw), value_raw = as.character(Total_raw),
    reason = dplyr::if_else(is.na(Specie),
                            "Spreadsheet grand-total row (blank identifying fields).",
                            paste0("Species out of scope: '", Specie, "'."))
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "puglia_basilicata") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(species_excluded_rows, qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::filter(Specie == "Culex pipiens")


##### 2.3. parse coordinates #####
# "Coordinate" is a single free-text "lat , lon" string, not two numeric
# columns, and is not uniformly well-formed. Three
# known raw encodings are reconstructed explicitly; anything else becomes
# NA and is picked up by the centroid fallback in Section 2.4
parse_coordinates <- function(coord_string) {
  trimmed <- trimws(coord_string)
  parts_list <- stringr::str_split(trimmed, "\\s*,\\s*")
  
  purrr::map2_dfr(trimmed, parts_list, function(s, p) {
    if (length(p) == 2) {
      # standard "lat.decimal , lon.decimal" encoding (the vast majority
      # of rows across all three years)
      lat <- suppressWarnings(as.numeric(p[1]))
      lon <- suppressWarnings(as.numeric(p[2]))
    } else if (length(p) == 4 && all(grepl("^-?[0-9]+$", p))) {
      # comma used as BOTH decimal separator and lat/lon field separator,
      # e.g. "40,249267,18,298309" -> lat 40.249267, lon 18.298309
      # (6/46 rows in the 2020 sheet; see header note for cross-validation)
      lat <- suppressWarnings(as.numeric(paste0(p[1], ".", p[2])))
      lon <- suppressWarnings(as.numeric(paste0(p[3], ".", p[4])))
    } else {
      # comma used as the decimal separator within each coordinate, but a
      # single period used as the lat/lon field separator instead of a
      # third comma, e.g. "40,36884.17,60448" -> lat 40,36884 / lon
      # 17,60448 -> 40.36884 / 17.60448 (1/46 rows in the 2020 sheet,
      # Manduria)
      dot_parts <- strsplit(s, "\\.")[[1]]
      if (length(dot_parts) == 2 && all(grepl("^-?[0-9]+,[0-9]+$", dot_parts))) {
        lat <- suppressWarnings(as.numeric(gsub(",", ".", dot_parts[1])))
        lon <- suppressWarnings(as.numeric(gsub(",", ".", dot_parts[2])))
      } else {
        lat <- NA_real_
        lon <- NA_real_
      }
    }
    tibble::tibble(latitude = lat, longitude = lon)
  })
}

db <- db %>%
  dplyr::bind_cols(parse_coordinates(db$Coordinate))

# rows where parsing yielded NA (the one genuinely unrecoverable string,
# plus any future malformed entries) -- for manual review
db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality, Coordinate, date_raw)


##### 2.3b. sea point check #####
# Puglia and Basilicata both have long Adriatic/Ionian coastlines, so a
# parsed coordinate can genuinely land offshore. Same row-level check used
# for Sicilia/Sardegna/Emilia-Romagna: only the specific reading is
# invalidated (set to NA), not the whole municipality; it is then picked
# up automatically by the existing missing-coordinate -> centroid fallback
# in Section 2.4 (is_bad_coord already treats is.na(latitude) as bad).
if (!"note" %in% names(db)) db$note <- NA_character_

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


##### 2.4. coordinate check & centroid imputation #####
summary(db[, c("latitude", "longitude")])

bbox_lat <- c(39.7, 42.3)
bbox_lon <- c(15.0, 18.6)

db %>%
  dplyr::filter(!dplyr::between(latitude, bbox_lat[1], bbox_lat[2]) |
                  !dplyr::between(longitude, bbox_lon[1], bbox_lon[2])) %>%
  dplyr::select(Municipality, Coordinate, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = bbox_lon, ylim = bbox_lat) +
  ggplot2::theme_bw()

# compute municipality centroids for fallback
# Municipality ISTAT match diagnostic, same pattern as every other
# regional script
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

muni_lookup <- tibble::tribble(
  ~raw,      ~clean,   ~type,
  "NARDO'",  "Nardò",  "typo"
)

if (!"note" %in% names(db)) db$note <- NA_character_

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


geojson_pb_coords <- geojson |>
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

db <- db %>%
  dplyr::mutate(
    Municipality_upper = dplyr::recode(toupper(trimws(Municipality)), "NARDO'" = "NARDÒ")
  ) %>%
  dplyr::left_join(geojson_pb_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    is_bad_coord = is.na(latitude) | is.na(longitude) |
      latitude == 0 | longitude == 0 |
      !dplyr::between(latitude, bbox_lat[1], bbox_lat[2]) |
      !dplyr::between(longitude, bbox_lon[1], bbox_lon[2]),
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
  dplyr::select(-lon_centroid, -lat_centroid, -is_bad_coord, -centroid_note, -Municipality_upper)

# check
db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

table(db$municipality_centroid, useNA = "always")


##### 2.5. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale della Puglia e della Basilicata"
db$contact_person <- "Maria Assunta Cafiero"
db$contact_person_email <- "mariaaassunta.cafiero@izspb.it"
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

db$id_trap <- NA
# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA
# db$WNV_test <- NA
# db$n_pool <- NA


##### 2.6. format date #####
db$date <- lubridate::ymd(db$date_raw)

if (!"note" %in% names(db)) db$note <- NA_character_

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2020, 2022)) %>%
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
    year = lubridate::year(date),
    week = lubridate::week(date)
  )


##### 2.7. province & region #####
levels(as.factor(db$Province))
db$Province <- stringr::str_to_title(trimws(db$Province))

levels(as.factor(db$Region))
db$Region <- stringr::str_to_title(trimws(db$Region))
db$Region <- dplyr::recode(db$Region, "Puglia" = "Apulia")

levels(as.factor(db$Region))

##### 2.8. municipality #####
levels(as.factor(db$Municipality))

db$Municipality <- stringr::str_to_title(trimws(db$Municipality))

levels(as.factor(db$Municipality))


##### 2.9. standardise trap type #####
levels(as.factor(db$trap_type_raw))

db <- db %>%
  dplyr::mutate(
    trap_type = dplyr::case_when(
      trimws(trap_type_raw) %in% c("BG-sentinel", "BG - Sentinel") ~ "BG",
      trimws(trap_type_raw) == "CDC-like traps" ~ "CDC_LIKE_CO2",
      trimws(trap_type_raw) == "Gravid Trap" ~ "GRAVID",
      TRUE ~ trap_type_raw
    )
  )

table(db$trap_type, useNA = "always")


##### 2.10. sex-disaggregated value (pivot) #####
db <- db %>%
  dplyr::mutate(
    Sesso_F = dplyr::coalesce(as.numeric(Sesso_F), 0),
    Sesso_M = dplyr::coalesce(as.numeric(Sesso_M), 0)
  )

# check
db <- db %>%
  dplyr::mutate(
    recomputed_total = Sesso_F + Sesso_M,
    arithmetic_mismatch = recomputed_total != as.numeric(Total_raw),
    mismatch_note = ifelse(
      arithmetic_mismatch,
      sprintf("Reported total (%s) did not match F+M (%s); value recomputed as F+M.",
              as.character(Total_raw), recomputed_total),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(mismatch_note) ~ paste(note, mismatch_note, sep = " | "),
      is.na(note) & !is.na(mismatch_note)  ~ mismatch_note,
      TRUE ~ note
    )
  )

print(db %>% dplyr::filter(arithmetic_mismatch) %>%
        dplyr::select(Municipality, date_raw, Sesso_F, Sesso_M, Total_raw, recomputed_total))

db <- db %>% dplyr::select(-recomputed_total, -arithmetic_mismatch, -mismatch_note)

db <- db %>%
  tidyr::pivot_longer(
    cols = c(Sesso_F, Sesso_M),
    names_to = "sex_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    sex = dplyr::case_when(
      sex_col == "Sesso_F" ~ "F",
      sex_col == "Sesso_M" ~ "M"
    )
  ) %>%
  dplyr::select(-sex_col)

table(db$sex, useNA = "always")
summary(db$value)

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


##### 2.11. order columns #####
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
  "Year outside expected 2020-2022 range" = all(dplyr::between(db$year, 2020, 2022)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude)),
  "Row count is not exactly double the pre-pivot Culex pipiens record count" =
    nrow(db) == 2 * (n_before_bind - nrow(species_not_culex_pipiens))
)


#### 3. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "puglia_basilicata_samplings_clean.csv"), row.names = FALSE)
