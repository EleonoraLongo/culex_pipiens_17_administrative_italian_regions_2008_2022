# =============================================================================
# DATA HARMONISATION — Sicily mosquito surveillance (2015–2024)
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
# Purpose   : Cleans and harmonises the IZS Sicilia (Istituto Zooprofilattico
#             Sperimentale della Sicilia A. Mirri) entomological surveillance
#             dataset for Sicily (2015-2024). Produces a standardised CSV
#             compatible with the national harmonised mosquito surveillance
#             database.
#
# Input     : ../izs_data/Sicilia/sicilia_File serie storiche WND.xlsx
#               (sheet 2: one row per trap-session; columns 15-43 are the
#               field catch by species/sex-neutral total; columns 65-92 are
#               the WNV pool-testing workflow -- VIROLOGIA, Num Pool,
#               Positività, and a second species block, confirmed by IZS
#               Sicilia [F. La Russa] to report FEMALE counts specifically,
#               since only females are pooled for arbovirus testing)
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/sicilia_samplings_clean.csv
#             (../main_db/clean_data/sicilia_pools_clean.csv -- not yet built)
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
db <- readxl::read_excel("../izs_data/Sicilia/sicilia_File serie storiche WND.xlsx", sheet = 2)

str(db)


##### 1.2. geo data #####
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% "Sicilia")

geojson_valid <- sf::st_make_valid(geojson)


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. ADD MUNICIPALITY INFORMATION ####

names(db)[names(db) == "Comune"] <- "Municipality"
names(db)[names(db) == "Prov."] <- "Province"

db$Municipality <- toupper(trimws(db$Municipality))

if (!"note" %in% names(db)) db$note <- NA_character_

# Municipality ISTAT match diagnostic (before correction)
muni_not_match <- setdiff(db$Municipality, toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

municipality_before <- db$Municipality
db$Municipality <- dplyr::recode(db$Municipality,
                                 "CASTELLAMARE DEL GOLFO"    = "CASTELLAMMARE DEL GOLFO",
                                 "GANCI"                     = "GANGI",
                                 "MORREALE"                  = "MONREALE",
                                 "MOTTA S.ANASTASIA"         = "MOTTA SANT'ANASTASIA",
                                 "NOTO- PACHINO"             = "NOTO",
                                 "S.MARGHERITA BELICE"       = "SANTA MARGHERITA DI BELICE",
                                 "SANTA MARGHERITA DI BELICE"= "SANTA MARGHERITA DI BELICE",
                                 "SAN PIETRO PATTI"          = "SAN PIERO PATTI",
                                 "SANT'ANGELO"               = "SANT'ANGELO MUXARO",
                                 "SANTO SEFANO DI QUISQUINA" = "SANTO STEFANO QUISQUINA"
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

# Municipality ISTAT match diagnostic
muni_not_match <- setdiff(db$Municipality, toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)

# restore proper Italian capitalisation (geojson is the reference spelling)
geojson_names <- geojson %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(Municipality = toupper(trimws(name)), Municipality_proper = name)

db <- db %>%
  dplyr::left_join(geojson_names, by = "Municipality") %>%
  dplyr::mutate(Municipality = dplyr::coalesce(Municipality_proper, Municipality)) %>%
  dplyr::select(-Municipality_proper)


#### 3. Data cleaning ####
##### 3.1. remove duplicates #####
db %>%
  dplyr::group_by(dplyr::across(everything())) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  print(n = Inf)

db <- db %>%
  dplyr::distinct()


##### 3.2. change remaining column names #####
names(db)

names(db)[names(db) == "Latitudine"] <- "latitude"
names(db)[names(db) == "Longitudine"] <- "longitude"
names(db)[names(db) == "tipo trappola"] <- "trap_type"
names(db)[names(db) == "anno"] <- "year"
names(db)[names(db) == "Num Pool"] <- "n_pool"


##### 3.3. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Region <- "Sicily"
db$Institute <- "Istituto Zooprofilattico Sperimentale della Sicilia A. Mirri"
db$contact_person <- "Francesco La Russa"
db$contact_person_email <- "francesco.larussa@izssicilia.it"
db$life_stage <- "adults"
db$EPSG <- "4326"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA


##### 3.4. format date, year and week #####
# no "date" column in the source: built from gg/mese/anno. gg is stored
# as a double (e.g. 15) and has 1 missing value; mese has one recorded
# typo (15, not a valid month) that lubridate will correctly refuse to
# parse rather than silently wrap
db <- db %>%
  dplyr::mutate(
    date = suppressWarnings(lubridate::ymd(paste(year, mese, round(gg), sep = "-")))
  )

n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2015, 2024)) %>%
  nrow()

n_date_unparsed
n_date_out_of_range

db <- db %>%
  dplyr::mutate(
    date_note = ifelse(is.na(date), "Date could not be parsed from day/month/year (e.g. an out-of-range month); flagged for review.", NA_character_),
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


##### 3.5. province full name #####
levels(as.factor(db$Province))

db <- db %>%
  dplyr::mutate(Province = dplyr::recode(Province,
                                         "PA" = "Palermo",
                                         "CT" = "Catania",
                                         "ME" = "Messina",
                                         "AG" = "Agrigento",
                                         "CL" = "Caltanissetta",
                                         "EN" = "Enna",
                                         "RG" = "Ragusa",
                                         "SR" = "Syracuse",
                                         "TP" = "Trapani"
  ))


##### 3.6. coordinate check & centroid imputation #####
# raw formats mix degree symbols/quotes, comma decimals, stray spaces,
# and a few values missing their decimal point entirely (e.g.
# "37833075" for 37.833075) -- all fixed by the same chain
db <- db %>%
  dplyr::mutate(
    latitude = stringr::str_remove_all(latitude, " "),
    longitude = stringr::str_remove_all(longitude, " "),
    latitude = stringr::str_remove_all(latitude, "[a-zA-Z°'\"\u2019]"),
    longitude = stringr::str_remove_all(longitude, "[a-zA-Z°'\"\u2019]"),
    latitude = stringr::str_remove(latitude, "\\.$"),
    longitude = stringr::str_remove(longitude, "\\.$"),
    latitude = stringr::str_replace_all(latitude, ",", "."),
    longitude = stringr::str_replace_all(longitude, ",", "."),
    latitude = dplyr::if_else(
      !is.na(latitude) & !stringr::str_detect(latitude, "\\.") & nchar(latitude) > 2,
      paste0(substr(latitude, 1, 2), ".", substr(latitude, 3, nchar(latitude))), latitude
    ),
    longitude = dplyr::if_else(
      !is.na(longitude) & !stringr::str_detect(longitude, "\\.") & nchar(longitude) > 2,
      paste0(substr(longitude, 1, 2), ".", substr(longitude, 3, nchar(longitude))), longitude
    ),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

summary(db[, c("latitude", "longitude")])

# a coordinate outside Sicily's bounding box is treated as invalid 
sic_lat_range <- c(36.5, 38.5)
sic_lon_range <- c(12.0, 15.5)

db %>%
  dplyr::filter(!is.na(latitude) &
                  (!dplyr::between(latitude, sic_lat_range[1], sic_lat_range[2]) |
                     !dplyr::between(longitude, sic_lon_range[1], sic_lon_range[2]))) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = sic_lon_range, ylim = sic_lat_range) +
  ggplot2::theme_bw()

# points that parse to a plausible-looking number but still land in the
# sea
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


##### 3.6b. coordinate error + genuine border cases (verified against comuni.geojson) #####

# (a) GENUINE COORDINATE ERROR (not a border case): 4 specific rows share
# an identical coordinate pair (36.88445, 14.88394) that is actually
# Modica's own genuine, repeatedly-used coordinate (code 006RGA28, 5
# occurrences 2019-2020) -- apparently copy-pasted into 4 unrelated
# sites' single-date records in Nov-Dec 2019. Scoped by (Codice, date)
# since three of these four codes (001EN159/Agira in particular) are
# otherwise correctly and consistently positioned on every other date.
glitch_rows <- tibble::tribble(
  ~Codice,     ~glitch_date,
  "001EN159",  as.Date("2019-11-14"),
  "011EN019",  as.Date("2019-11-15"),
  "097ME008",  as.Date("2019-11-21"),
  "057PA089",  as.Date("2019-11-28")
)

db <- db %>%
  dplyr::left_join(glitch_rows, by = "Codice") %>%
  dplyr::mutate(
    is_glitch_row = !is.na(glitch_date) & date == glitch_date,
    glitch_note = ifelse(is_glitch_row,
                         "Recorded coordinates matched a different site's (Modica) genuine coordinate exactly; apparent data-entry copy-paste error isolated to this single date -- discarded and re-derived from municipality centroid.",
                         NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(glitch_note) ~ paste(note, glitch_note, sep = " | "),
      is.na(note) & !is.na(glitch_note)  ~ glitch_note,
      TRUE ~ note
    ),
    latitude  = dplyr::if_else(is_glitch_row, NA_real_, latitude),
    longitude = dplyr::if_else(is_glitch_row, NA_real_, longitude)
  ) %>%
  dplyr::select(-glitch_date, -is_glitch_row, -glitch_note)

# (b) GENUINE BORDER CASES: verified adjacent (touches(), 0 km) against
# comuni.geojson. Scoped by Codice, not by Municipality name, because
# Paternò, Siracusa and Alcamo each have OTHER codes correctly
# positioned within their own polygon that a name-based note would
# wrongly touch.
border_case_codes <- tibble::tribble(
  ~Codice,     ~matched_muni,
  "001EN159",  "Assoro",
  "007CT078",  "Belpasso",
  "017SR121",  "Avola",
  "017SR118",  "Avola",
  "037CT054",  "Ramacca",
  "026AG056",  "Camastra",
  "013PA074",  "Giardinello",
  "001TP070",  "Partinico"
)

db <- db %>%
  dplyr::left_join(border_case_codes, by = "Codice") %>%
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
  dplyr::select(-matched_muni, -border_note)


# extract coordinates of municipality centroids
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
                          !dplyr::between(db$latitude, sic_lat_range[1], sic_lat_range[2]) |
                          !dplyr::between(db$longitude, sic_lon_range[1], sic_lon_range[2]))

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    coord_missing = is.na(latitude) | is.na(longitude),
    coord_invalid = !coord_missing &
      (!dplyr::between(latitude, sic_lat_range[1], sic_lat_range[2]) |
         !dplyr::between(longitude, sic_lon_range[1], sic_lon_range[2])),
    is_bad_coord = coord_missing | coord_invalid,
    used_centroid = is_bad_coord & !is.na(lat_centroid),
    municipality_centroid = ifelse(used_centroid, "yes", "no"),
    centroid_note = dplyr::case_when(
      used_centroid & coord_missing ~ "Coordinates were missing; derived from municipality centroid.",
      used_centroid & coord_invalid ~ "Coordinates were present but implausible (outside Sicily); derived from municipality centroid.",
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

still_missing_records <- db %>%
  dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(Municipality, date, latitude, longitude, note)

print(still_missing_records) # solved after

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = sic_lon_range, ylim = sic_lat_range) +
  ggplot2::theme_bw()

# recover the 16 rows with no Municipality at all via point-in-polygon,
# where a coordinate is available
db <- db %>% dplyr::mutate(.row_id = dplyr::row_number())

needs_reverse_geocode <- db %>%
  dplyr::filter(is.na(Municipality) & !is.na(latitude) & !is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

if (nrow(needs_reverse_geocode) > 0) {
  reverse_matched <- sf::st_join(needs_reverse_geocode, geojson_valid %>% dplyr::select(name)) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.row_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(.row_id, rev_municipality = name)
  
  db <- db %>%
    dplyr::left_join(reverse_matched, by = ".row_id") %>%
    dplyr::mutate(
      reverse_geocode_note = ifelse(is.na(Municipality) & !is.na(rev_municipality),
                                    "Municipality was unknown; derived by reverse-geocoding the recorded coordinate.",
                                    NA_character_),
      note = dplyr::case_when(
        !is.na(note) & !is.na(reverse_geocode_note) ~ paste(note, reverse_geocode_note, sep = " | "),
        is.na(note) & !is.na(reverse_geocode_note)  ~ reverse_geocode_note,
        TRUE ~ note
      ),
      Municipality = dplyr::coalesce(Municipality, rev_municipality)
    ) %>%
    dplyr::select(-rev_municipality, -reverse_geocode_note)
}

db <- db %>% dplyr::select(-.row_id)

sum(is.na(db$Municipality))

# a few coordinates have a plausible reading but still fail strict
# point-in-polygon matching (e.g. a coastal trap whose recorded position
# falls just outside the municipality boundary due to GPS/digitisation
# precision) -- recover these via nearest-polygon distance instead of
# leaving them unresolved
still_needs_municipality <- db %>%
  dplyr::mutate(.row_id2 = dplyr::row_number()) %>%
  dplyr::filter(is.na(Municipality) & !is.na(latitude) & !is.na(longitude))

if (nrow(still_needs_municipality) > 0) {
  still_needs_sf <- still_needs_municipality %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  nearest_idx <- sf::st_nearest_feature(still_needs_sf, geojson_valid)
  
  nearest_matched <- tibble::tibble(
    .row_id2 = still_needs_municipality$.row_id2,
    nearest_municipality = geojson_valid$name[nearest_idx]
  )
  
  db <- db %>%
    dplyr::mutate(.row_id2 = dplyr::row_number()) %>%
    dplyr::left_join(nearest_matched, by = ".row_id2") %>%
    dplyr::mutate(
      nearest_note = ifelse(is.na(Municipality) & !is.na(nearest_municipality),
                            "Municipality was unknown; recorded coordinates fell just outside every municipality polygon (GPS/digitisation precision), so the nearest municipality polygon was used instead.",
                            NA_character_),
      note = dplyr::case_when(
        !is.na(note) & !is.na(nearest_note) ~ paste(note, nearest_note, sep = " | "),
        is.na(note) & !is.na(nearest_note)  ~ nearest_note,
        TRUE ~ note
      ),
      Municipality = dplyr::coalesce(Municipality, nearest_municipality)
    ) %>%
    dplyr::select(-.row_id2, -nearest_note)
}

sum(is.na(db$Municipality))

# last resort: rows with neither a Municipality nor a coordinate to
# recover it from (
geojson_province_centroids <- geojson |>
  sf::st_transform(crs = 3857) |> sf::st_centroid() |> sf::st_transform(crs = 4326) |>
  dplyr::mutate(lon_c = sf::st_coordinates(geometry)[, "X"], lat_c = sf::st_coordinates(geometry)[, "Y"]) |>
  sf::st_drop_geometry() %>%
  dplyr::group_by(prov_name) %>%
  dplyr::summarise(lon_prov_centroid = mean(lon_c, na.rm = TRUE), lat_prov_centroid = mean(lat_c, na.rm = TRUE), .groups = "drop")

db <- db %>%
  dplyr::left_join(geojson_province_centroids, by = c("Province" = "prov_name")) %>%
  dplyr::mutate(
    still_missing = is.na(latitude) | is.na(longitude),
    province_note = ifelse(still_missing,
                           "No municipality or coordinate available; derived from province-level centroid (low confidence).",
                           NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(province_note) ~ paste(note, province_note, sep = " | "),
      is.na(note) & !is.na(province_note)  ~ province_note,
      TRUE ~ note
    ),
    municipality_centroid = dplyr::if_else(still_missing, "yes", municipality_centroid),
    longitude = dplyr::if_else(still_missing, lon_prov_centroid, longitude),
    latitude  = dplyr::if_else(still_missing, lat_prov_centroid, latitude)
  ) %>%
  dplyr::select(-lon_prov_centroid, -lat_prov_centroid, -still_missing, -province_note)

sum(is.na(db$latitude) | is.na(db$longitude))


##### 3.7. trap type & id_trap #####
# MOVED from its original position (immediately after 3.5) to here, after
# Municipality is fully resolved (ISTAT matching, reverse-geocoding, and
# the nearest-polygon/province-centroid fallbacks above): id_trap is
# derived from Municipality below when no trap code is available, and
# building it before Municipality was fully resolved meant it could
# silently bake in a literal "NA" for the handful of sessions recovered
# only by the later steps.
levels(as.factor(db$trap_type))

db <- db %>%
  dplyr::mutate(trap_type = dplyr::case_when(
    trap_type %in% c("B.G. sentinel", "B.G. Sentinel", "Bg sentinel", "BG Sentinel") ~ "BG",
    trap_type %in% c("CDC", "CDC 1", "CDC 2", "CDC CO2") ~ "CDC",
    # confirmed (2026): Sicily has no CO2-baited CDC/BG category at all;
    # "CDC CO2" in the source does not indicate CO2 baiting here
    trap_type %in% c("Gravid Trap", "GT") ~ "GRAVID",
    trap_type %in% c("universal trap", "Universal Trap") ~ "CDC_LIKE",
    # was previously mapped to CDC_LIKE_CO2 -- corrected: Sicily's
    # confirmed levels include CDC-LIKE (no CO2) but no CDC-LIKE-CO2
    TRUE ~ trap_type
  ))

table(db$trap_type, useNA = "always")

sum(is.na(db$Codice))

db <- db %>%
  dplyr::mutate(Codice = toupper(trimws(Codice))) %>%
  dplyr::group_by(Codice, trap_type, date) %>%
  dplyr::arrange(date, Codice, trap_type, .by_group = TRUE) %>%
  dplyr::mutate(
    total = dplyr::n(),
    replicate_num = dplyr::row_number(),
    id_trap = dplyr::if_else(
      total == 1, paste0(Codice, "_", trap_type),
      paste0(Codice, "_", trap_type, "_", replicate_num)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-replicate_num, -total)

db <- db %>%
  dplyr::mutate(
    id_trap_note = ifelse(is.na(Codice),
                          "No trap code recorded in the source; id_trap derived from municipality and trap type instead (coarser, may group distinct physical traps together).",
                          NA_character_),
    note = dplyr::case_when(
      !is.na(note) & !is.na(id_trap_note) ~ paste(note, id_trap_note, sep = " | "),
      is.na(note) & !is.na(id_trap_note)  ~ id_trap_note,
      TRUE ~ note
    ),
    id_trap = dplyr::if_else(is.na(Codice), paste0(Municipality, "_", trap_type), id_trap)
  ) %>%
  dplyr::select(-id_trap_note)

# residual case: no trap code AND no resolvable Municipality (only Province
# known) -- id_trap above would otherwise contain a literal "NA" baked into
# the string. Municipality itself is intentionally left NA: no defensible
# method exists to pick a specific municipality within the province, so it
# is not imputed, only documented.
db <- db %>%
  dplyr::mutate(
    no_recovery_note = ifelse(
      is.na(Codice) & is.na(Municipality),
      "No trap code, coordinates, or resolvable municipality available in the source; only Province is known. Municipality left unresolved -- not imputed, since no defensible method exists to identify the specific municipality within the province.",
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(no_recovery_note) ~ paste(note, no_recovery_note, sep = " | "),
      is.na(note) & !is.na(no_recovery_note)  ~ no_recovery_note,
      TRUE ~ note
    ),
    id_trap = dplyr::case_when(
      !is.na(id_trap) ~ id_trap,
      !is.na(Municipality) ~ paste0(Municipality, "_", trap_type),
      TRUE ~ paste0("UNKNOWN_MUNI_", Province, "_", trap_type)
    )
  ) %>%
  dplyr::select(-no_recovery_note)

sum(is.na(db$id_trap))


##### 3.8. species, value and sex #####
# two independent counts of Cx. pipiens exist per session: the routine
# field total (col 17, both sexes) and, only when a WNV-testing pool was
# made, the female count specifically (col 71)
db$cx_tot <- suppressWarnings(as.numeric(db[[17]]))
db$cx_fem <- suppressWarnings(as.numeric(db[[71]]))

sex_status <- dplyr::case_when(
  !is.na(db$cx_tot) & !is.na(db$cx_fem) & db$cx_fem > db$cx_tot ~ "female_exceeds_total",
  !is.na(db$cx_tot) & !is.na(db$cx_fem)                          ~ "both",
  !is.na(db$cx_tot) & is.na(db$cx_fem)                           ~ "total_only",
  is.na(db$cx_tot) & !is.na(db$cx_fem)                           ~ "female_only",
  TRUE                                                            ~ "neither"
)
table(sex_status)

sex_note <- dplyr::case_when(
  sex_status == "female_exceeds_total" ~ "Reported female count exceeds the reported total catch in the source; male count could not be computed and is left unspecified.",
  sex_status == "total_only" ~ "No WNV-testing pool was recorded for this session; sex breakdown is unavailable, reported as a single unsexed total.",
  sex_status == "female_only" ~ "No field total was recorded for this session; only the female count from the testing pool is available.",
  TRUE ~ NA_character_
)
db$note <- dplyr::case_when(
  !is.na(db$note) & !is.na(sex_note) ~ paste(db$note, sex_note, sep = " | "),
  is.na(db$note) & !is.na(sex_note)  ~ sex_note,
  TRUE ~ db$note
)

db$sex_status <- sex_status

# remove all other species/pool columns -- not part of this pipeline
db <- db %>% dplyr::select(-c(16:92))

db_both <- db %>% dplyr::filter(sex_status == "both") %>%
  dplyr::mutate(value_M = pmax(cx_tot - cx_fem, 0), value_F = cx_fem) %>%
  tidyr::pivot_longer(cols = c(value_M, value_F), names_to = "sex", values_to = "value") %>%
  dplyr::mutate(sex = dplyr::recode(sex, "value_M" = "M", "value_F" = "F"))

db_female_exceeds <- db %>% dplyr::filter(sex_status == "female_exceeds_total") %>%
  dplyr::mutate(sex = "F", value = cx_fem)

db_total_only <- db %>% dplyr::filter(sex_status == "total_only") %>%
  dplyr::mutate(sex = NA_character_, value = cx_tot)

db_female_only <- db %>% dplyr::filter(sex_status == "female_only") %>%
  dplyr::mutate(sex = "F", value = cx_fem)

n_neither_excluded <- sum(sex_status == "neither")
n_neither_excluded

neither_rows <- db %>% dplyr::filter(sex_status == "neither") %>%
  dplyr::transmute(
    region = "sicilia", check = "no_sampling_event",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = NA_character_,
    reason = "Neither a field total nor a tested-pool count was recorded for this session."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "sicilia") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(neither_rows, qc_log_path, append = file.exists(qc_log_path))

db <- dplyr::bind_rows(db_both, db_female_exceeds, db_total_only, db_female_only) %>%
  dplyr::select(-cx_tot, -cx_fem, -sex_status) %>%
  dplyr::mutate(value = as.integer(value)) %>%
  dplyr::filter(!is.na(value))

# check
print(table(db$sex, useNA = "always"))

db %>%
  dplyr::group_by(year, sex) %>%
  dplyr::summarise(n_individui = sum(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = n_individui) %>%
  print()

# species (this pipeline is Culex pipiens only)
db$species_raw <- "Culex pipiens"

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(-species_raw)


##### 3.9. order columns #####
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
  "Year outside expected 2015-2024 range" = all(dplyr::between(db$year, 2015, 2024)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


#### 4. save clean data ####
outdir <- "../main_db/clean_data/"

write.csv(db, file = paste0(outdir, "sicilia_samplings_clean.csv"), row.names = FALSE)