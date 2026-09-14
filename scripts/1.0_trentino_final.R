# =============================================================================
# DATA HARMONISATION — Province of Trento mosquito surveillance (2008–2024)
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
#             for the Province of Trento (Fondazione Edmund Mach - FEM).
#             Produces a standardised CSV compatible with the national
#             harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Trentino/dati culex pipiens trentino_2008_2024_FEM.csv
#             ../izs_data/Trentino/trento lista trapple e coordinate.csv
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/trentino_samplings_clean.csv
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data
#    2. Add Municipality Information
#    3. Data Cleaning
#    4. Save Clean Data
# =============================================================================

rm(list = ls())

#### 0. libraries and working directory ####
library(dplyr)
library(tidyverse)
library(sf)
library(stringr)
library(lubridate)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. DATA ####
##### 1.1. samplings and traps #####
db <- read.csv("../izs_data/Trentino/dati culex pipiens trentino_2008_2024_FEM.csv",
               sep = ";",
               stringsAsFactors = FALSE)

traps <- read.csv("../izs_data/Trentino/trento lista trapple e coordinate.csv",
                  sep = "|",
                  quote = "",
                  stringsAsFactors = FALSE) %>%
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ stringr::str_remove_all(.x, "\"")))


##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(prov_name %in% "Trento")


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. ADD MUNICIPALITY INFORMATION ####
names(db)[names(db) == "MUNICIPALITY"] <- "Municipality"

# Municipality ISTAT match diagnostic (before correction)
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# "Zambana" merged with Nave San Rocco into "Terre d'Adige" on 1 Jan 2019
if (!"note" %in% names(db)) db$note <- NA_character_

municipality_before <- db$Municipality
db$Municipality <- dplyr::recode(db$Municipality, "Zambana" = "Terre d'Adige")
changed <- municipality_before != db$Municipality
muni_note <- ifelse(changed,
                    sprintf("'%s' is a historical municipality name; merged into '%s' on 1 Jan 2019.",
                            municipality_before, db$Municipality),
                    NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(muni_note) ~ paste(db$note, muni_note, sep = " | "),
  is.na(db$note) & !is.na(muni_note)  ~ muni_note,
  TRUE ~ db$note
)

# Municipality ISTAT match diagnostic (after correction)
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)


#### 3. Data cleaning ####
##### 3.1. db merge (traps and samplings) #####
traps %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::arrange(ID) %>%
  print(n = Inf)

traps_fixed <- traps %>%
  dplyr::mutate(
    NAME = dplyr::if_else(NAME == "", stringr::str_split_fixed(ID, "\\|", n = 9)[, 2], NAME),
    ID_TRAPTYPE = dplyr::if_else(ID_TRAPTYPE == "", stringr::str_split_fixed(ID, "\\|", n = 9)[, 3], ID_TRAPTYPE),
    LONG = dplyr::if_else(LONG == "", stringr::str_split_fixed(ID, "\\|", n = 9)[, 6], LONG),
    LAT = dplyr::if_else(LAT == "", stringr::str_split_fixed(ID, "\\|", n = 9)[, 7], LAT)
  )

traps_unique <- traps_fixed %>%
  dplyr::group_by(ID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

n_trap_dupes_dropped <- nrow(traps_fixed) - nrow(traps_unique)
n_trap_dupes_dropped

traps_unique$ID <- as.integer(traps_unique$ID)

n_before_merge <- nrow(db)

db <- dplyr::left_join(
  db, traps_unique,
  by = c("ID_TRAP" = "ID")
)

stopifnot(
  "Row count changed during trap merge -- investigate before proceeding" =
    nrow(db) == n_before_merge
)


##### 3.2. change column names #####
names(db)

names(db)[names(db) == "TRAP"] <- "id_trap"
names(db)[names(db) == "DATA"] <- "date"
names(db)[names(db) == "WEEK"] <- "week"
names(db)[names(db) == "SEX"] <- "sex"
names(db)[names(db) == "LONG"] <- "longitude"
names(db)[names(db) == "LAT"] <- "latitude"


##### 3.2b. municipality correction: point-in-polygon verified errors #####
# ZBBG1 and ZBBG2 were flagged by the national merge script's point-in-
# polygon check: both traps' recorded coordinates fall unambiguously
# within Lavis, well inside the boundary (visually confirmed)
db <- db %>%
  dplyr::mutate(
    municipality_before_ptcheck = Municipality,
    Municipality = dplyr::if_else(id_trap %in% c("ZBBG1", "ZBBG2"), "Lavis", Municipality),
    ptcheck_note = dplyr::if_else(
      id_trap %in% c("ZBBG1", "ZBBG2"),
      sprintf("Municipality corrected from '%s' to 'Lavis': point-in-polygon check against the recorded coordinates showed the trap falls unambiguously within Lavis, not the declared municipality.", municipality_before_ptcheck),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(ptcheck_note) ~ paste(note, ptcheck_note, sep = " | "),
      is.na(note) & !is.na(ptcheck_note)  ~ ptcheck_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-municipality_before_ptcheck, -ptcheck_note)


##### 3.2c. municipality boundary note (unresolved cases) #####
# id_trap "214" and "219-bis" were also flagged by the national merge
# script's point-in-polygon check, but unlike ZBBG1/ZBBG2 above their
# coordinates sit right on (or immediately next to) the administrative
# boundary 
db <- db %>%
  dplyr::mutate(
    boundary_note = dplyr::case_when(
      id_trap == "214"     ~ "Coordinates sit on the administrative boundary with Nago-Torbole; declared municipality (Arco) retained, as coordinates alone cannot determine the correct side.",
      id_trap == "219-bis" ~ "Coordinates sit on the administrative boundary with Arco; declared municipality (Dro) retained, as coordinates alone cannot determine the correct side.",
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(boundary_note) ~ paste(note, boundary_note, sep = " | "),
      is.na(note) & !is.na(boundary_note)  ~ boundary_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-boundary_note)


##### 3.3. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Region <- "Trentino-Alto Adige"
db$Institute <- "Fondazione Edmund Mach"
db$contact_person <- "Daniele Arnoldi"
db$contact_person_email <- "daniele.arnoldi@fmach.it"
db$life_stage <- "adults"
db$EPSG <- "4326"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA
# db$WNV_test <- NA
# db$n_pool <- NA


##### 3.4. format date #####
db$date <- lubridate::dmy(db$date)

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2008, 2024)) %>%
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

db$year <- lubridate::year(db$date)


##### 3.4b. aggregate pool-level records (RESTORED) #####
# multiple CODEs at the same trap/date/sex are multiple laboratory pools
# from a single visit, to be summed, not independent visits.
species_cols <- c(
  "CULEX.PIPIENS", "AEDES.ALBOPICTUS", "CULEX.HORTENSIS",
  "AEDES.KOREICUS", "OCHLEROTATUS.GENICULATUS", "CULISETA.ANNULATA",
  "CULISETA.LONGIAREOLATA", "ANOPHELES.PLUMBEUS", "ANOPHELES.MACULIPENNIS.SL",
  "ANOPHELES.SP", "AEDES.VEXANS", "ANOPHELES.CLAVIGER", "COQUILLETTIDIA.RICHIARDII",
  "OCHLEROTATUS.CASPIUS", "AEDES.GEMINUS.CINEREUS", "AEDES.SP",
  "UNKNOWN", "OCHLEROTATUS.SP.", "CULEX.S.P.", "CULEX.IMPUDICUS", "AEDES.JAPONICUS",
  "CULISETA.S.P"
)

n_pool_rows_before <- nrow(db)

db <- db %>%
  dplyr::group_by(id_trap, date, sex) %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(species_cols), ~ sum(.x, na.rm = TRUE)),
    n_pools = dplyr::n()
  ) %>%
  dplyr::ungroup()

table(db$n_pools)

db <- db %>%
  dplyr::distinct(id_trap, date, sex, .keep_all = TRUE) %>%
  dplyr::select(-n_pools)

n_pool_rows_before
nrow(db)
n_pool_rows_before - nrow(db)


##### 3.5. province #####
levels(as.factor(db$Municipality))

db <- db %>%
  dplyr::mutate(Province = dplyr::case_when(
    Municipality %in% c("Ala", "Arco", "Calceranica al Lago", "Campodenno", "Castel Ivano",
                        "Dro", "Grigno", "Imer", "Lavis", "Levico Terme", "Madruzzo",
                        "Mezzano", "Mezzocorona", "Mezzolombardo", "Nago-Torbole",
                        "Nogaredo", "Novaledo", "Ospedaletto", "Pergine Valsugana",
                        "Primiero San Martino di Castrozza", "Riva del Garda",
                        "Roverè della Luna", "Rovereto", "San Michele all'Adige",
                        "Telve", "Tenno", "Terre d'Adige", "Trento") ~ "Trento",
    TRUE ~ NA_character_
  ))

sum(is.na(db$Province))

db %>%
  dplyr::filter(is.na(Province)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()


##### 3.6. coordinate check & centroid imputation #####
db <- db %>%
  dplyr::mutate(
    latitude = stringr::str_replace_all(latitude, ",", "."),
    longitude = stringr::str_replace_all(longitude, ",", "."),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

summary(db[, c("latitude", "longitude")])

tn_lat_range <- c(45.6, 46.6)
tn_lon_range <- c(10.4, 12.0)

db %>%
  dplyr::filter(!is.na(latitude) &
                  (!dplyr::between(latitude, tn_lat_range[1], tn_lat_range[2]) |
                     !dplyr::between(longitude, tn_lon_range[1], tn_lon_range[2]))) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = tn_lon_range, ylim = tn_lat_range) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()

# centroid computation 
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
                          !dplyr::between(db$latitude, tn_lat_range[1], tn_lat_range[2]) |
                          !dplyr::between(db$longitude, tn_lon_range[1], tn_lon_range[2]))

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    coord_missing = is.na(latitude) | is.na(longitude),
    coord_invalid = !coord_missing &
      (!dplyr::between(latitude, tn_lat_range[1], tn_lat_range[2]) |
         !dplyr::between(longitude, tn_lon_range[1], tn_lon_range[2])),
    is_bad_coord = coord_missing | coord_invalid,
    used_centroid = is_bad_coord & !is.na(lat_centroid),
    municipality_centroid = ifelse(used_centroid, "yes", "no"),
    centroid_note = dplyr::case_when(
      used_centroid & coord_missing ~ "Coordinates were missing; derived from municipality centroid.",
      used_centroid & coord_invalid ~ "Coordinates were present but implausible (outside Trento province); derived from municipality centroid.",
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


##### 3.7. trap type #####
db <- db %>%
  dplyr::mutate(
    DRY_ICE_logical = dplyr::case_when(
      tolower(trimws(DRY_ICE)) == "true"  ~ TRUE,
      tolower(trimws(DRY_ICE)) == "false" ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  dplyr::mutate(
    trap_type = dplyr::case_when(
      is.na(ID_TRAPTYPE) | ID_TRAPTYPE == "" ~ NA_character_,
      ID_TRAPTYPE == "BG" & DRY_ICE_logical == TRUE ~ "BG_CO2",
      ID_TRAPTYPE == "BG" & (DRY_ICE_logical == FALSE | is.na(DRY_ICE_logical)) ~ "BG",
      ID_TRAPTYPE == "CDC" & DRY_ICE_logical == TRUE ~ "CDC_CO2",
      ID_TRAPTYPE == "CDC" & (DRY_ICE_logical == FALSE | is.na(DRY_ICE_logical)) ~ "CDC",
      TRUE ~ ID_TRAPTYPE
    )
  ) %>%
  dplyr::select(-DRY_ICE_logical)

table(db$trap_type, useNA = "always")


##### 3.8. mosquito species values and sex #####
# Pivot data
db <- db %>%
  tidyr::pivot_longer(
    cols = all_of(species_cols),
    names_to = "species_raw",
    values_to = "value",
    values_drop_na = TRUE
  )

# rows about to be dropped below (unidentified-species column reporting a
# zero count 
unknown_zero_dropped <- db %>%
  dplyr::filter(species_raw == "UNKNOWN" & value == 0) %>%
  dplyr::transmute(
    region = "trentino", check = "unknown_species_zero_count",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = "0",
    reason = "Zero-count placeholder for the unidentified-species column; not a genuine catch record."
  )

# remove unknown only if count == 0
db <- db %>%
  dplyr::filter(!(species_raw == "UNKNOWN" & value == 0))

# name standardization
db <- db %>%
  dplyr::mutate(species_raw = case_when(
    species_raw == "AEDES.ALBOPICTUS" ~ "Aedes albopictus",
    species_raw == "AEDES.VEXANS" ~ "Aedes vexans",
    species_raw == "AEDES.GEMINUS.CINEREUS" ~ "Aedes cinereus",
    species_raw == "AEDES.JAPONICUS" ~ "Aedes japonicus",
    species_raw == "AEDES.KOREICUS" ~ "Aedes koreicus",
    species_raw == "OCHLEROTATUS.CASPIUS" ~ "Aedes caspius",
    species_raw == "OCHLEROTATUS.GENICULATUS" ~ "Aedes geniculatus",
    species_raw == "ANOPHELES.CLAVIGER" ~ "Anopheles claviger",
    species_raw == "ANOPHELES.MACULIPENNIS.SL" ~ "Anopheles maculipennis sl",
    species_raw == "ANOPHELES.PLUMBEUS" ~ "Anopheles plumbeus",
    species_raw == "COQUILLETTIDIA.RICHIARDII" ~ "Coquillettidia richiardii",
    species_raw == "CULISETA.ANNULATA" ~ "Culiseta annulata",
    species_raw == "CULISETA.LONGIAREOLATA" ~ "Culiseta longiareolata",
    species_raw == "CULEX.HORTENSIS" ~ "Culex hortensis",
    species_raw == "CULEX.IMPUDICUS" ~ "Culex impudicus",
    species_raw == "CULEX.PIPIENS" ~ "Culex pipiens",
    species_raw == "CULEX.S.P." ~ "Culex spp",
    species_raw == "ANOPHELES.SP" ~ "Anopheles spp",
    species_raw %in% c("AEDES.SP", "OCHLEROTATUS.SP.") ~ "Aedes spp",
    species_raw == "CULISETA.S.P" ~ "Culiseta spp",
    species_raw == "UNKNOWN" ~ NA_character_,
    TRUE ~ species_raw
  ))

n_rows_before_species_consolidation <- nrow(db)

db <- db %>%
  dplyr::group_by(id_trap, date, sex, species_raw) %>%
  dplyr::mutate(value = sum(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id_trap, date, sex, species_raw, .keep_all = TRUE)

n_rows_before_species_consolidation
nrow(db)
n_rows_before_species_consolidation - nrow(db)

species_not_found <- db %>%
  dplyr::filter(!is.na(species_raw)) %>%
  dplyr::anti_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(species_raw) %>%
  dplyr::distinct()

print(species_not_found)

db <- db %>%
  dplyr::mutate(
    unknown_species_note = ifelse(is.na(species_raw),
                                  "Catch was recorded but could not be identified to species.",
                                  NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(unknown_species_note) ~ paste(note, unknown_species_note, sep = " | "),
      is.na(note) & !is.na(unknown_species_note)  ~ unknown_species_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-unknown_species_note)

# join
db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(-species_raw)

# sex standardization
levels(as.factor(db$sex))

db <- db %>%
  dplyr::mutate(
    sex = toupper(trimws(sex)),
    sex = dplyr::case_when(
      sex == "FEMALE" ~ "F",
      sex == "MALE" ~ "M",
      sex == "" ~ NA_character_,
      TRUE ~ sex
    )
  )

levels(as.factor(db$sex))


##### 3.9. only numbers in value #####
REGION_NAME <- "trentino"
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
readr::write_csv(dplyr::bind_rows(unknown_zero_dropped, no_sampling_rows, invalid_value_rows), qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::mutate(value = dplyr::if_else(is_structural | is_uncertain | is_blank | is_unparseable_other,
                                       NA_real_, value_numeric)) %>%
  dplyr::filter(!is.na(value))


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
  "Missing catch value (value) after harmonisation" = all(!is.na(db$value)),
  "Negative catch values detected" = all(db$value >= 0),
  "Year outside expected 2008-2024 range" = all(dplyr::between(db$year, 2008, 2024)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude)),
  "Missing Province remains after lookup" = all(!is.na(db$Province))
)


#### 4. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "trentino_samplings_clean.csv"), row.names = FALSE)
