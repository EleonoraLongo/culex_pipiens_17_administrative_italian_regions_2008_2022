# =============================================================================
# DATA HARMONISATION — Veneto & Friuli-Venezia Giulia mosquito surveillance
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
#             for Veneto and Friuli-Venezia Giulia regions (Istituto 
#             Zooprofilattico Sperimentale delle Venezie). Produces a 
#             standardised CSV compatible with the national harmonised 
#             mosquito surveillance database.
#
# Input     : ../izs_data/Veneto-FVG/WEST NILE surveillance 2010-2023.xlsx
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/veneto_friuli_samplings_clean.csv
#             (../main_db/clean_data/veneto_friuli_pools_clean.csv)
#             (../main_db/qc_veneto_pool_sum_mismatches.csv)
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
library(sf)
library(stringr)
library(lubridate)

# working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. DATA ####
##### 1.1. samplings and pools #####
db <- readxl::read_excel("../izs_data/Veneto-FVG/veneto_WEST NILE surveillance 2010-2023.xlsx",
                         sheet = 1)


traps <- readxl::read_excel("../izs_data/Veneto-FVG/veneto_WEST NILE surveillance 2010-2023.xlsx",
                            sheet = 2)

# merge db and traps
db <- merge(db, traps[, c("ID SITE", "LAT", "LON", "REGION (NUT 2)")], by = "ID SITE", all.x = TRUE)


##### 1.2. geo data #####
# municipality
geojson <- sf::st_read("../other_data/comuni.geojson")

geojson <- geojson %>%
  dplyr::filter(reg_name %in% c("Veneto", "Friuli-Venezia Giulia"))


##### 1.3. taxonomy lookup #####
source("../script/0.0_taxonomy_lookup.R")


#### 2. Data cleaning ####
n_before <- nrow(db)

exact_dup <- duplicated(db)
dup_rows <- db[exact_dup, ] %>%
  dplyr::transmute(
    region = "veneto_friuli", check = "exact_duplicate",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality = MUNICIPALITY, 
    date = as.character(`DATE RECOVERY`), 
    value_raw = as.character(`TOTAL MOSQUITOES CAPTURED`),
    reason = "Exact duplicate row (data-entry double-write); collapsed to one observation."
  )

qc_log_path <- "../main_db/qc_exclusions_log.csv"
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(!(region == "veneto_friuli" & check == "exact_duplicate")) %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(dup_rows, qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::distinct()

n_before
nrow(db)
n_before - nrow(db)


##### 2.2. change column names #####
names(db)

names(db)[names(db) == "ID SITE"] <- "id_trap"
names(db)[names(db) == "MUNICIPALITY"] <- "Municipality"
names(db)[names(db) == "PROVINCE (NUT3)"] <- "Province"
names(db)[names(db) == "TRAP TYPE"] <- "trap_type"
names(db)[names(db) == "DATE RECOVERY"] <- "date"
names(db)[names(db) == "LAT"] <- "latitude"
names(db)[names(db) == "LON"] <- "longitude"
names(db)[names(db) == "REGION (NUT 2)"] <- "Region"


##### 2.3. add columns with NA or fixed values #####
db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale delle Venezie"
db$contact_person <- "Fabrizio Montarsi"
db$contact_person_email <- "fmontarsi@izsvenezie.it "
db$life_stage <- "adults"
db$EPSG <- "4326"

# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA


##### 2.4. format date #####
db$date <- lubridate::dmy(db$date)

# columns year and week
db <- db %>%
  dplyr::mutate(
    week = lubridate::isoweek(date),
    year = lubridate::year(date)
  )

# Date validity
n_date_unparsed <- sum(is.na(db$date))
n_date_out_of_range <- db %>%
  dplyr::filter(!is.na(date), !dplyr::between(lubridate::year(date), 2010, 2023)) %>%
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


##### 2.5. municipality correct names #####
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

if (!"note" %in% names(db)) db$note <- NA_character_

muni_not_match_before <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match_before)
print(muni_not_match_before)

muni_not_match_exact <- setdiff(trimws(db$Municipality), trimws(geojson$name))
length(muni_not_match_exact)
print(muni_not_match_exact)


# corrections
municipality_lookup <- c(
  "Quarto D'altino"            = "Quarto d'Altino",
  "Codogne"                    = "Codognè",
  "Ronco All'adige"            = "Ronco all'Adige",
  "Erbe"                       = "Erbè",
  "Valeggio Sul Mincio"        = "Valeggio sul Mincio",
  "San Canzian D'isonzo"       = "San Canzian d'Isonzo",
  "San Vito Al Tagliamento"    = "San Vito al Tagliamento",
  "Ariano Nel Polesine"        = "Ariano nel Polesine",
  "Tezze Sul Brenta"           = "Tezze sul Brenta",
  "San Michele Al Tagliamento" = "San Michele al Tagliamento",
  "Sant'elena"                 = "Sant'Elena",
  "Villafranca Veronese"       = "Villafranca di Verona",
  "Porto Tolle - S. Giorgio"   = "Porto Tolle",
  "Lignano"                    = "Lignano Sabbiadoro",
  "Barbarano mossano"          = "Barbarano Mossano"
)

db <- db %>%
  dplyr::mutate(
    municipality_before = Municipality,
    Municipality = dplyr::recode(Municipality, !!!municipality_lookup),
    muni_note = dplyr::case_when(
      municipality_before != Municipality ~ sprintf("Municipality corrected from '%s' to '%s'.", municipality_before, Municipality),
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(muni_note) ~ paste(note, muni_note, sep = " | "),
      is.na(note) & !is.na(muni_note)  ~ muni_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-municipality_before, -muni_note)


# check
muni_not_match_after <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match_after)
print(muni_not_match_after)


##### 2.5b. municipality correction: point-in-polygon verified errors #####
# id_trap "350" was flagged by the national merge script's point-in-
# polygon check: all 792 of its records have coordinates that fall
# unambiguously within Lendinara, well inside the boundary (visually
# confirmed)
db <- db %>%
  dplyr::mutate(
    municipality_before_ptcheck = Municipality,
    Municipality = dplyr::if_else(id_trap == "350", "Lendinara", Municipality),
    ptcheck_note = dplyr::if_else(
      id_trap == "350",
      sprintf("Municipality corrected from '%s' to 'Lendinara': point-in-polygon check against the recorded coordinates showed the trap falls unambiguously within Lendinara, not the declared municipality.", municipality_before_ptcheck),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(ptcheck_note) ~ paste(note, ptcheck_note, sep = " | "),
      is.na(note) & !is.na(ptcheck_note)  ~ ptcheck_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-municipality_before_ptcheck, -ptcheck_note)


##### 2.5c. municipality boundary note (unresolved cases) #####
# id_trap "186" (declared Zugliano) and "393" (declared Barbarano Mossano)
# were also flagged by the national merge script's point-in-polygon check,
# but unlike id_trap "350" above their coordinates sit right on (or
# immediately next to) the administrative boundary 
db <- db %>%
  dplyr::mutate(
    boundary_note = dplyr::case_when(
      id_trap == "186" ~ "Coordinates sit on the administrative boundary with Fara Vicentino; declared municipality (Zugliano) retained, as coordinates alone cannot determine the correct side.",
      id_trap == "393" ~ "Coordinates sit on the administrative boundary with Albettone; declared municipality (Barbarano Mossano) retained, as coordinates alone cannot determine the correct side.",
      TRUE ~ NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(boundary_note) ~ paste(note, boundary_note, sep = " | "),
      is.na(note) & !is.na(boundary_note)  ~ boundary_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-boundary_note)


##### 2.6. coordinate check and missing imputation #####
db <- db %>%
  dplyr::mutate(
    latitude = as.numeric(stringr::str_replace_all(latitude, ",", ".")),
    longitude = as.numeric(stringr::str_replace_all(longitude, ",", "."))
  )


##### 2.6b. sea point check #####
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

n_missing_before <- sum(is.na(db$latitude) | is.na(db$longitude))

db <- db %>%
  dplyr::mutate(Municipality_upper = toupper(trimws(Municipality))) %>%
  dplyr::left_join(geojson_coords, by = c("Municipality_upper" = "name")) %>%
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
  dplyr::select(-lon_centroid, -lat_centroid, -used_centroid, -centroid_note, -Municipality_upper)

n_centroid_imputed <- sum(db$municipality_centroid == "yes")
n_still_missing <- sum(is.na(db$latitude) | is.na(db$longitude))

n_missing_before
n_centroid_imputed
n_still_missing

# check limits
summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!dplyr::between(latitude, 44.7, 46.7) | !dplyr::between(longitude, 10.6, 13.9)) %>%
  dplyr::select(Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = db, ggplot2::aes(x = longitude, y = latitude),
                      color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = c(10.6, 13.9), ylim = c(44.7, 46.7)) +
  ggplot2::theme_bw()


##### 2.7. trap_type #####
levels(as.factor(db$trap_type))

db <- db %>%
  dplyr::mutate(trap_type = case_when(
    trap_type == "Gravid Trap" ~ "GRAVID",
    trap_type == "CDC-CO2" ~ "CDC_CO2",
    TRUE ~ trap_type
  ))

levels(as.factor(db$trap_type))


##### 2.8. aggregate pool-level records (RESTORED and EXTENDED) #####
# A single ID CAPTURE (field capture event) is split across multiple lab
# pools for testing, and MALE/FEMALE/NOT DETERMINED at this point are
# PARTIAL counts per pool, not the total for the capture
pool_check <- db %>%
  dplyr::group_by(`ID CAPTURE`) %>%
  dplyr::summarise(
    sum_check = sum(MALE, na.rm = TRUE) + sum(FEMALE, na.rm = TRUE) + sum(`NOT DETERMINED`, na.rm = TRUE),
    declared_total = dplyr::first(`TOTAL MOSQUITOES CAPTURED`),
    .groups = "drop"
  ) %>%
  dplyr::filter(sum_check != declared_total)

nrow(pool_check)
readr::write_csv(pool_check, "../main_db/qc_veneto_pool_sum_mismatches.csv")

n_pool_rows_before <- nrow(db)

db <- db %>%
  dplyr::group_by(`ID CAPTURE`, SPECIES) %>%
  dplyr::mutate(
    MALE = sum(MALE, na.rm = TRUE),
    FEMALE = sum(FEMALE, na.rm = TRUE),
    `NOT DETERMINED` = sum(`NOT DETERMINED`, na.rm = TRUE),
    n_pools = dplyr::n()
  ) %>%
  dplyr::ungroup()

table(db$n_pools)

db <- db %>%
  dplyr::distinct(`ID CAPTURE`, SPECIES, .keep_all = TRUE) %>%
  dplyr::select(-n_pools)

n_pool_rows_before
nrow(db)
n_pool_rows_before - nrow(db)


##### 2.9. sex and value (Pivot Longer + Numeric extraction) #####
conflicts <- db %>%
  dplyr::filter((MALE != 0) + (FEMALE != 0) + (`NOT DETERMINED` != 0) > 1)

print(nrow(conflicts))

if(nrow(conflicts) > 0) {
  head(conflicts)
}

db <- db %>%
  tidyr::pivot_longer(
    cols = c(MALE, FEMALE, `NOT DETERMINED`),
    names_to = "sex_raw",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    sex = dplyr::case_when(
      sex_raw == "MALE" ~ "M",
      sex_raw == "FEMALE" ~ "F",
      sex_raw == "NOT DETERMINED" ~ "IND", 
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-sex_raw)

# a zero count for "not determined" (IND) carries no information 
ind_zero_dropped <- db %>%
  dplyr::filter(sex == "IND" & value == 0) %>%
  dplyr::transmute(
    region = "veneto_friuli", check = "unsexed_zero_count",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality, date, value_raw = "0",
    reason = "Zero-count 'not determined' (IND) record; not a genuine sex-specific observation."
  )

db <- db %>%
  dplyr::filter(!(sex == "IND" & value == 0))

# Only numbers in abundance
REGION_NAME <- "veneto_friuli"
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
readr::write_csv(dplyr::bind_rows(ind_zero_dropped, no_sampling_rows, invalid_value_rows), qc_log_path, append = file.exists(qc_log_path))

db <- db %>%
  dplyr::mutate(
    value = dplyr::if_else(is_structural | is_uncertain | is_blank | is_unparseable_other,
                           NA_integer_, as.integer(value_numeric))
  ) %>%
  dplyr::filter(!is.na(value))

sum(db$value)
table(db$sex, useNA = "always")


##### 2.10. mosquito species #####
db <- db %>%
  dplyr::mutate(species_raw = case_when(
    SPECIES == "Aedes/Ochleotatus spp." ~ "Aedes spp.",
    SPECIES == "Culex Hortensis"        ~ "Culex hortensis",
    SPECIES == "Ochlerotatus annulipes"   ~ "Aedes annulipes",
    SPECIES == "Ochlerotatus berlandi"    ~ "Aedes berlandi",
    SPECIES == "Ochlerotatus cantans"     ~ "Aedes cantans",
    SPECIES == "Ochlerotatus caspius"     ~ "Aedes caspius",
    SPECIES == "Ochlerotatus cataphylla"  ~ "Aedes cataphylla",
    SPECIES == "Ochlerotatus detritus"    ~ "Aedes detritus",
    SPECIES == "Ochlerotatus echinus"     ~ "Aedes echinus",
    SPECIES == "Ochlerotatus geniculatus" ~ "Aedes geniculatus",
    SPECIES == "Ochlerotatus rusticus"    ~ "Aedes rusticus",
    SPECIES == "Ochlerotatus sticticus"   ~ "Aedes sticticus",
    SPECIES == "NOT DETERMINED"           ~ NA_character_,   
    TRUE ~ SPECIES
  )) %>%
  dplyr::select(-SPECIES)

species_not_found <- db %>%
  dplyr::anti_join(taxonomy_lookup, by = "species_raw") %>%
  dplyr::select(species_raw) %>%
  dplyr::distinct()

print(species_not_found)

db <- db %>%
  dplyr::left_join(taxonomy_lookup, by = "species_raw")


# #####WNV test and number of pools#####
# db <- db %>%
#   dplyr::group_by(id_trap, trap_type, date) %>%
#   dplyr::mutate(
#     n_pool = n(),  
#     WNV_test = case_when(
#       `POS WEST NILE` == TRUE ~ "yes",
#       `POS WEST NILE` == FALSE ~ "no",
#       is.na(`POS WEST NILE`) ~ NA_character_
#     )
#   ) %>%
#   dplyr::ungroup()
# 
# 
# #####copy for pool data#####
# pools <- db


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
  "Year outside expected 2010-2023 range" = all(dplyr::between(db$year, 2010, 2023)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude))
)


# ####POOL DATA####
# pools <- pools %>% 
#   dplyr::filter(WNV_test == "yes")
# 
# names(pools)[names(pools) == "N. POOL"] <- "id_pool"
# names(pools)[names(pools) == "value"] <- "pool_value"
# names(pools)[names(pools) == "WNV_test"] <- "WNV_positive"
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

write.csv(db, file = paste0(outdir, "veneto_friuli_samplings_clean.csv"), row.names = FALSE)

# write.csv(pools, file = paste0(outdir, "veneto_friuli_pools_clean.csv"), row.names = FALSE)
