# =============================================================================
# DATA HARMONISATION — Lombardia mosquito surveillance (2014–2023)
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
#             entomological surveillance dataset for Lombardia (2014–2023).
#             Produces a standardised CSV compatible with the national
#             harmonised mosquito surveillance database.
#
# Input     : ../izs_data/Lombardia/DB_Culex_Coordinate_FD.xlsx
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/lombardia_samplings_clean.csv
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
##### 1.1.2014-2023 #####
db <- readxl::read_excel("../izs_data/Lombardia/lombardia_DB_Culex_Coordinate_FD.xlsx")

##### 1.2. geo data #####
# municipality
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% "Lombardia")

##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. Data cleaning ####
##### 2.1. change colmn names#####
names(db)

names(db)[names(db) == "PROV."] <- "Province"
names(db)[names(db) == "DATA CAMPION."] <- "date"
names(db)[names(db) == "COMUNE"] <- "Municipality"
names(db)[names(db) == "num_darwin"] <- "id_trap"
names(db)[names(db) == "LONG"] <- "longitude"
names(db)[names(db) == "LAT"] <- "latitude"


##### 2.2. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Region <- "Lombardy"
db$Institute <- "Istituto Zooprofilattico Sperimentale Lombardia ed Emilia-Romagna"
db$contact_person <- "Francesco Defilippo"
db$contact_person_email <- "francesco.defilippo@izsler.it"
db$trap_type <- "CDC_LIKE_CO2"
db$sex <- "F"
db$life_stage <- "adults"
db$EPSG <- "4326"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA


##### 2.3. format date ####
db$date <- lubridate::ymd(db$date)

if (!"note" %in% names(db)) db$note <- NA_character_

year_2028_fix <- lubridate::year(db$date) == 2028
date_fix_note <- ifelse(year_2028_fix,
                        "Date year typo corrected from 2028 to the recording year minus 5 years.",
                        NA_character_)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(date_fix_note) ~ paste(db$note, date_fix_note, sep = " | "),
  is.na(db$note) & !is.na(date_fix_note)  ~ date_fix_note,
  TRUE ~ db$note
)
db$date <- dplyr::if_else(year_2028_fix, db$date - lubridate::years(5), db$date)

# columns year and week
db <- db %>%
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  )

# Date validity: NA after parsing, or a plausible-but-out-of-range year
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


##### 2.4. coordinate check #####
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, 44.7, 46.6) | !dplyr::between(longitude, 8.5, 11.5)) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(8.5, 11.5), ylim = c(44.7, 46.6)) +
  ggplot2::theme_bw()

db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality) %>%
  dplyr::distinct()


##### 2.5. municipality name check & centroid imputation #####

# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# corrections
muni_lookup <- tibble::tribble(
  ~raw,                       ~clean,                          ~type,
  "BIGARELLO",                "SAN GIORGIO BIGARELLO",         "merged",
  "SAN MARTINO ALL'ARGINE",   "SAN MARTINO DALL'ARGINE",       "typo",
  "SAN GENESIO E UNITI",      "SAN GENESIO ED UNITI",          "typo",
  "CORTEOLONA",               "CORTEOLONA E GENZONE",          "merged",
  "FELONICA - NIGELLA",       "SERMIDE E FELONICA",            "merged",
  "RIVAROLO DEL RE",          "RIVAROLO DEL RE ED UNITI",      "merged",
  "TORRE DE PICENARDI",       "TORRE DE' PICENARDI",           "typo",
  "CAPPELLA PICENARDI",       "CAPPELLA DE' PICENARDI",        "typo",
  "LONATO",                   "LONATO DEL GARDA",              "merged",
  "TRAVACO SICCOMARIO",       "TRAVACÒ SICCOMARIO",            "typo",
  "AEREOPORTO MALPENSA",      "SANT'ANGELO LOMELLINA",         "mislabelled"
)

db <- db %>%
  dplyr::mutate(Municipality_up = toupper(trimws(Municipality))) %>%
  dplyr::left_join(muni_lookup, by = c("Municipality_up" = "raw")) %>%
  dplyr::mutate(
    muni_note = dplyr::case_when(
      type == "typo" ~ sprintf("Municipality typo corrected from '%s' to '%s'.", Municipality_up, clean),
      type == "merged" ~ sprintf("'%s' is a historical municipality name; merged into '%s'.", Municipality_up, clean),
      type == "mislabelled" ~ sprintf("Municipality '%s' did not match its own coordinates; corrected to '%s' (verified by point-in-polygon check).", Municipality_up, clean),
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


##### 2.5b. coordinate-based corrections (Municipality name trusted, coordinates wrong) #####
# corrections applied after the merged script cheks runned
db <- db %>% dplyr::mutate(.row_id = dplyr::row_number())

db_muni_sf <- db %>%
  # rows with missing coordinates cannot be point-in-polygon checked (and
  # will go through the existing centroid-imputation path regardless, same
  # as before this block existed) -- exclude them here rather than letting
  # st_as_sf() error out on NA coordinates.
  dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

db_muni_match <- sf::st_join(
  db_muni_sf,
  geojson %>% dplyr::transmute(matched_muni = toupper(trimws(name))),
  join = sf::st_within
) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    .row_id,
    declared_muni = toupper(trimws(Municipality)),
    matched_muni
  )

# confirmed coordinate errors (declared name trusted, coordinates discarded)
coord_error_pairs <- tibble::tribble(
  ~declared_muni,       ~matched_muni,
  "COMO",               "COMMESSAGGIO",
  "CASALMAGGIORE",      "TORRE DE' PICENARDI",
  "BAGNOLO SAN VITO",   "MONZAMBANO",
  "SAN BENEDETTO PO",   "TRAVACÒ SICCOMARIO"
)

# genuine border cases (left as declared, note only)
border_case_pairs <- tibble::tribble(
  ~declared_muni,     ~matched_muni,
  "PUSIANO",          "CESANA BRIANZA",
  "NUVOLENTO",        "PREVALLE",
  "CORTE FRANCA",     "ISEO"
)

coord_error_rows <- db_muni_match %>%
  dplyr::semi_join(coord_error_pairs, by = c("declared_muni", "matched_muni")) %>%
  dplyr::pull(.row_id)

border_case_rows <- db_muni_match %>%
  dplyr::semi_join(border_case_pairs, by = c("declared_muni", "matched_muni")) %>%
  dplyr::pull(.row_id)

# sanity check against the counts established in the chat-log investigation:
# expect 25 (Como) + 12 (Casalmaggiore) + 3 (Bagnolo San Vito) = 40
length(coord_error_rows)
length(border_case_rows)

coord_error_note <- "Recorded coordinates fell within a different, non-adjacent municipality's polygon (point-in-polygon check); treated as a coordinate transcription error rather than a naming error -- declared Municipality kept, coordinates discarded and re-derived below from the municipality centroid."
border_case_note <- "Coordinates fall on/near the boundary with an adjacent municipality (point-in-polygon check assigns them to the neighbouring polygon); declared Municipality and coordinates kept as recorded -- plausible genuine edge-of-municipality trap location, not treated as an error."

db <- db %>%
  dplyr::mutate(
    latitude  = ifelse(.row_id %in% coord_error_rows, NA_real_, latitude),
    longitude = ifelse(.row_id %in% coord_error_rows, NA_real_, longitude),
    coord_fix_note = dplyr::case_when(
      .row_id %in% coord_error_rows  ~ coord_error_note,
      .row_id %in% border_case_rows  ~ border_case_note,
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(coord_fix_note) ~ paste(note, coord_fix_note, sep = " | "),
      is.na(note)  & !is.na(coord_fix_note) ~ coord_fix_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-.row_id, -coord_fix_note)

rm(db_muni_sf, db_muni_match, coord_error_pairs, border_case_pairs,
   coord_error_rows, border_case_rows, coord_error_note, border_case_note)

# NOTE: latitude/longitude set to NA just above for the 3 coordinate-error
# groups are picked up automatically by the EXISTING centroid-imputation
# step immediately below (same mechanism already used for genuinely
# missing coordinates): those rows will get municipality_centroid = "yes"
# and the standard centroid note appended, in addition to the
# coord_error_note already attached above.


muni_not_match_after <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match_after)
print(muni_not_match_after)

# compute centroids
geojson_lomb_coords <- geojson |>
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

# fill lat and lon NAs only + adding municipality_centroid column (if
# centroid used for lat and lon) and note column
n_missing_before <- sum(is.na(db$latitude) | is.na(db$longitude))

db <- db %>%
  dplyr::left_join(geojson_lomb_coords, by = c("Municipality" = "name_upper")) %>%
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
  ggplot2::coord_sf(xlim = c(8.5, 11.5), ylim = c(44.7, 46.6)) +
  ggplot2::theme_bw()

str(db)


##### 2.6. province complete names #####
db <- db %>%
  dplyr::mutate(Province = dplyr::recode(Province,
                                         "BG" = "Bergamo",
                                         "BS" = "Brescia",
                                         "CO" = "Como",
                                         "CR" = "Cremona",
                                         "LC" = "Lecco",
                                         "LO" = "Lodi",
                                         "MB" = "Monza and Brianza",
                                         "MI" = "Milan",
                                         "MN" = "Mantua",
                                         "PV" = "Pavia",
                                         "SO" = "Sondrio",
                                         "VA" = "Varese"
  ))


##### 2.6b. province corrections (verified via point-in-polygon, independent of coordinate/municipality errors) #####
# Oggiono and Pusiano are correctly-named, correctly-positioned
# municipalities whose declared Province code was simply mistyped in the
# source for a subset of records (Oggiono: 6/39 rows say "MN" instead of
# "LC"; Pusiano: all 6 rows say "CO" instead of "LC" -- unrelated to
# Pusiano's separate border-case note with Cesana Brianza). Sant'Angelo
# Lomellina inherited its old "Aeroporto Malpensa" label's Province
# (Varese) when the name was corrected above; its own true Province
# (Pavia) was never updated alongside it.
db <- db %>%
  dplyr::mutate(
    Province_before = Province,
    Province = dplyr::case_when(
      Municipality == "Oggiono" ~ "Lecco",
      Municipality == "Pusiano" ~ "Lecco",
      Municipality == "Sant'Angelo Lomellina" ~ "Pavia",
      TRUE ~ Province
    ),
    prov_fix_note = dplyr::if_else(
      Province != Province_before,
      sprintf("Province corrected from '%s' to '%s' (point-in-polygon check; independent of any municipality/coordinate correction above).", Province_before, Province),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(prov_fix_note) ~ paste(note, prov_fix_note, sep = " | "),
      is.na(note) & !is.na(prov_fix_note)  ~ prov_fix_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-Province_before, -prov_fix_note)


##### 2.7. municipality (title case) #####
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


##### 2.8. standardize species #####
levels(as.factor(db$SPECIE))

db <- db %>%
  dplyr::rename(species_raw = SPECIE)

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw")

db <- db %>% dplyr::select(-species_raw)

levels(as.factor(db$species))


##### 2.9. only numbers in value & aggregation #####
REGION_NAME <- "lombardia"
STRUCTURAL_PLACEHOLDERS <- c()
UNCERTAIN_PLACEHOLDERS <- c()
EXTRACT_DIGITS <- FALSE

value_raw_before <- as.character(db$`N  ESEMPLARI`)

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
  dplyr::filter(!(is_structural | is_uncertain | is_blank | is_unparseable_other))

# #save pool data
# db1 <- db
# 
# 
db <- db %>%
  # dplyr::mutate(
  #   WNV_test = case_when(
  #     positivo %in% c("no", "+ USUTU", "+USUTU") ~ "no",
  #     positivo %in% c("+ WN", "+ WN E USUTU", "+wn", "+WN", "+wn, + usutu") ~ "yes",
  #     TRUE ~ NA_character_
  #   )
  # ) %>%
  dplyr::group_by(id_trap, longitude, latitude, date, species) %>%
  dplyr::summarise(
    value = sum(as.numeric(`N  ESEMPLARI`), na.rm = TRUE),
    # n_pool = n(),
    Municipality = first(Municipality),
    Province = first(Province),
    trap_type = first(trap_type),
    # WNV_test = first(WNV_test),
    longitude = first(longitude),
    latitude = first(latitude),
    municipality_centroid = first(municipality_centroid),
    Region = first(Region),
    Country = first(Country),
    Institute = first(Institute),
    contact_person = first(contact_person),
    contact_person_email = first(contact_person_email),
    life_stage = first(life_stage),
    EPSG = first(EPSG),
    # volume = first(volume),
    # substrate = first(substrate),
    # larvicide_presence = first(larvicide_presence),
    # larvicide_type = first(larvicide_type),
    sex = first(sex),
    week = first(week),
    year = first(year),
    Canonical_name = first(Canonical_name),
    kingdom = first(kingdom),
    phylum = first(phylum),
    class = first(class),
    order = first(order),
    family = first(family),
    genus = first(genus),
    species = first(species),
    note = first(note),
    .groups = "drop"
  )


##### 2.10. order columns #####
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
  "Year outside expected 2014-2023 range" = all(dplyr::between(db$year, 2014, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


# ####POOL DATA####
# db2 <- db1
# 
# db2 <- db2 %>%
#   dplyr::mutate(
#     WNV_test = case_when(
#       positivo %in% c("no", "+ USUTU", "+USUTU") ~ "no",
#       positivo %in% c("+ WN", "+ WN E USUTU", "+wn", "+WN", "+wn, + usutu") ~ "yes",
#       TRUE ~ NA_character_
#     )
#   )
# 
# #only tested pools
# db2 <- db2 %>% 
#   dplyr::filter(!is.na(WNV_test))
# 
# #####pool value column#####
# db2 <- db2 %>%
#   rename(pool_value = `N  ESEMPLARI`)
# 
# #####positive test column#####
# db2 <- db2 %>%
#   dplyr::mutate(WNV_positive = if_else(WNV_test == 1, "yes", "no"))
# 
# db2 <- db2 %>%
#   dplyr::mutate(
#     WNV_positive = case_when(
#       positivo %in% c("no", "+ USUTU", "+USUTU") ~ "no",
#       positivo %in% c("+ WN", "+ WN E USUTU", "+wn", "+WN", "+wn, + usutu") ~ "yes",
#       TRUE ~ NA_character_
#     )
#   )
# 
# #####add id pools#####
# db2 <- db2 %>%
#   dplyr::arrange(date) %>%
#   dplyr::mutate(id_pool = row_number())
# 
# 
# #####order columns#####
# db2 <- db2 %>% 
#   dplyr::select(
#     year, week, date, Country, Region, Province, Municipality, longitude, latitude,
#     Institute, contact_person, contact_person_email, id_trap, Canonical_name,
#     kingdom, phylum, class, order, family, genus, species, life_stage, sex, id_pool,
#     pool_value, WNV_positive
#   )


#### 3. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "lombardia_samplings_clean.csv"), row.names = FALSE)

# write.csv(db2, file = paste0(outdir, "lombardia_pools_clean.csv"), row.names = FALSE)
