# =============================================================================
# DATA HARMONISATION — Liguria mosquito surveillance (2013–2025)
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
# Purpose   : Cleans and harmonises the IZS Piemonte, Liguria e Valle
#             d'Aosta (IZSPLV) Culex pipiens trap-catch dataset for
#             Liguria (2013–2025). Produces a standardised CSV compatible
#             with the national harmonised mosquito surveillance database.
#             New script (no prior version existed for this region).
#
# Input     : ../izs_data/Piemonte_Liguria/CULEX PIPIENS 2_liguria.xlsx
#               (sheets "LIGURIA 2013" ... "LIGURIA 2025")
#             ../other_data/comuni.geojson
#             ../script/0.0_taxonomy_lookup.R
#
# Output    : ../main_db/clean_data/liguria_samplings_clean.csv
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#### 1. Data ####
liguria_path <- "../izs_data/Piemonte_Liguria/CULEX PIPIENS 2_liguria.xlsx"
sheet_names_lig <- readxl::excel_sheets(liguria_path)

# no header: header rows are inconsistent across years
raw_sheets <- vector("list", length(sheet_names_lig))
for (s in seq_along(sheet_names_lig)) {
  raw_sheets[[s]] <- readxl::read_excel(liguria_path, sheet = sheet_names_lig[s], col_names = FALSE)
}
names(raw_sheets) <- sheet_names_lig


#### 2. Data cleaning ####

##### 2.1. Harvest trap coordinate references #####

# scans every row/sheet for a trap code + coordinate pair, wherever it
# falls (position varies by year); also reads municipality/trap-type so
# 2013-2015 records (no location text) get a real municipality via the
# trap-code join, not just a centroid.

# Liguria bounding box, used to validate coordinates throughout
lig_lat_range <- c(43.5, 44.7)
lig_lon_range <- c(7.3, 9.9)

# trap code: short, alphanumeric, >=1 digit
code_regex <- "^[A-Z]{2,4}[0-9]{0,3}[A-Z]{0,6}$"

# known municipalities, longest name first to avoid partial-match shadowing
liguria_municipalities <- c(
  "SESTRI LEVANTE", "VEZZANO LIGURE", "CAIRO MONTENOTTE", "VALLECROSIA",
  "VALBREVENNA", "VENTIMIGLIA", "CAMPOROSSO", "VADO LIGURE", "DOLCEACQUA",
  "LA SPEZIA", "BRUGNATO", "CHIAVARI", "LAVAGNA", "RAPALLO", "ALBENGA",
  "BUSALLA", "AMEGLIA", "ARCOLA", "LEGINO", "ZOAGLI", "TAGGIA", "USCIO",
  "SANREMO", "LOANO", "SAVONA", "GENOVA", "IMPERIA", "VARAZZE"
) %>% .[order(-nchar(.))]

# Legino is a hamlet of Savona, not an independent municipality (no
# polygon of its own in comuni.geojson); corrected wherever matched
hamlet_correction <- c("LEGINO" = "SAVONA")

# landmark/facility names (hospitals, ports, migrant centres) that appear
# instead of a trap code or a municipality name in some years (2018 sheet);
# each verified individually against either external sources or the
# curated trap_reference sheet, not guessed.
facility_correction <- c(
  "FIUMARA" = "GENOVA",
  "PORTO DI PRA" = "GENOVA",
  "H. SAN MARTINO" = "GENOVA",
  "H. GASLINI" = "GENOVA",
  "H. GALLIERA" = "GENOVA",
  "H. VILLA SCASSI" = "GENOVA",
  "CIMITERO DI STAGLIENO" = "GENOVA",
  "CENTRO IMMIGRATI PARCO ROJA" = "VENTIMIGLIA",
  "OSPEDALE CIVILE S.ANDREA INFETTIVI" = "LA SPEZIA",
  # inferred from SV1's own reference description ("Porto - Capitaneria di
  # Porto"); moderate confidence, no other coast-guard site appears in the
  # reference blocks
  "GUARDIA COSTIERA" = "SAVONA",
  # confirmed against the curated trap_reference sheet (trap codes GE18GT /
  # H03, both Genova): a smaller flower market distinct from the famous
  # Sanremo one, not a guess between the two
  "MERCATO DEI FIORI" = "GENOVA"
)

# trap-type vocabulary, standardised to fixed codes
trap_type_map <- c(
  "BG" = "BG", "BG-S" = "BG", "BGG" = "BG",
  "BG SENTINEL" = "BG", "BG-SENTINEL" = "BG",
  "BG - SENTINEL" = "BG",
  "GRAVID" = "GRAVID", "GRAVID TRAP" = "GRAVID", "GT" = "GRAVID",
  "GRAVID/BG" = "GRAVID",
  "CDC" = "CDC", "CDC CO2" = "CDC_CO2",
  "CDC+BG LURE" = "CDC", "1 CDC" = "CDC"
)

harvest_out <- list()
harvest_count <- 0

for (sheet_label in names(raw_sheets)) {
  sheet_df <- raw_sheets[[sheet_label]]
  n_rows <- nrow(sheet_df)
  
  for (i in seq_len(n_rows)) {
    row_vals <- as.list(sheet_df[i, ])
    row_vals <- row_vals[!sapply(row_vals, is.na)]
    if (length(row_vals) == 0) next
    
    # start of a glued-on reference-table fragment (2014/2015: token
    # "LIGURIA", fragment starts 2 fields earlier; 2023: bare 2-letter
    # province code, fragment starts at that field). Duplicated verbatim
    # in Section 2.2 -- see header note.
    liguria_marker <- integer(0)
    bare_province_marker <- integer(0)
    for (k in seq_along(row_vals)) {
      v <- row_vals[[k]]
      if (!is.na(v) && toupper(trimws(as.character(v))) == "LIGURIA") liguria_marker <- c(liguria_marker, k)
      if (!is.na(v) && toupper(trimws(as.character(v))) %in% c("IM", "SV", "GE", "SP")) bare_province_marker <- c(bare_province_marker, k)
    }
    starts <- c(
      if (length(liguria_marker) > 0) liguria_marker[1] - 2 else NA_integer_,
      if (length(bare_province_marker) > 0) bare_province_marker[1] else NA_integer_
    )
    starts <- starts[!is.na(starts)]
    ref_start <- if (length(starts) > 0) max(1, min(starts)) else NA_integer_
    if (!is.na(ref_start)) row_vals <- row_vals[ref_start:length(row_vals)]
    
    # single cell encoding "lat, lon" as free text
    latlon <- NULL
    latlon_pos <- NA_integer_
    for (j in seq_along(row_vals)) {
      x <- row_vals[[j]]
      p <- c(lat = NA_real_, lon = NA_real_)
      if (!is.na(x)) {
        xc <- trimws(as.character(x))
        if (grepl(",", xc)) {
          parts <- trimws(strsplit(xc, ",")[[1]])
          if (length(parts) == 2) {
            p <- c(lat = suppressWarnings(as.numeric(parts[1])),
                   lon = suppressWarnings(as.numeric(parts[2])))
          }
        }
      }
      if (!is.na(p["lat"]) && !is.na(p["lon"]) &&
          dplyr::between(p["lat"], lig_lat_range[1], lig_lat_range[2]) &&
          dplyr::between(p["lon"], lig_lon_range[1], lig_lon_range[2])) {
        latlon <- p; latlon_pos <- j; break
      }
    }
    # fallback: lat and lon in two separate cells
    if (is.null(latlon)) {
      lat_pos <- integer(0); lon_pos <- integer(0)
      for (k in seq_along(row_vals)) {
        n <- suppressWarnings(as.numeric(as.character(row_vals[[k]])))
        if (!is.na(n) && dplyr::between(n, lig_lat_range[1], lig_lat_range[2])) lat_pos <- c(lat_pos, k)
        if (!is.na(n) && dplyr::between(n, lig_lon_range[1], lig_lon_range[2])) lon_pos <- c(lon_pos, k)
      }
      if (length(lat_pos) >= 1 && length(lon_pos) >= 1) {
        latlon <- c(lat = as.numeric(as.character(row_vals[[lat_pos[1]]])),
                    lon = as.numeric(as.character(row_vals[[lon_pos[1]]])))
        latlon_pos <- c(lat_pos[1], lon_pos[1])
      }
    }
    if (is.null(latlon)) next
    
    # trap code: short, alphanumeric, >=1 digit; some 2014 cells glue
    # code and description together (e.g. "IM1GT gravid trap"), hence
    # the word-level fallback. Duplicated in Section 2.2 -- see header.
    code_str <- rep(NA_character_, length(row_vals))
    for (k in seq_along(row_vals)) {
      v <- row_vals[[k]]
      if (is.na(v)) next
      xc <- toupper(trimws(as.character(v)))
      if (grepl(code_regex, xc) && grepl("[0-9]", xc)) {
        code_str[k] <- xc
      } else {
        words <- stringr::str_split(xc, "\\s+")[[1]]
        hit <- words[grepl(code_regex, words) & grepl("[0-9]", words)]
        if (length(hit) > 0) code_str[k] <- hit[1]
      }
    }
    code_pos <- which(!is.na(code_str))
    if (length(code_pos) == 0) next
    trap_code_val <- code_str[code_pos[1]]
    
    remaining_pos <- setdiff(seq_along(row_vals), c(code_pos[1], latlon_pos))
    remaining_text <- paste(sapply(row_vals[remaining_pos], as.character), collapse = " | ")
    
    # municipality match against the remaining free text
    ref_municipality <- NA_character_
    if (!is.na(remaining_text)) {
      txt_upper <- toupper(remaining_text)
      hit_municipality <- liguria_municipalities[stringr::str_detect(txt_upper, liguria_municipalities)]
      if (length(hit_municipality) > 0) {
        ref_municipality <- dplyr::coalesce(hamlet_correction[hit_municipality[1]], hit_municipality[1])
      }
    }
    
    # trap-type match against the remaining free text
    ref_trap_type <- NA_character_
    if (!is.na(remaining_text)) {
      tokens <- toupper(trimws(stringr::str_split(remaining_text, "\\|")[[1]]))
      hit_type <- tokens[tokens %in% names(trap_type_map)]
      if (length(hit_type) > 0) ref_trap_type <- trap_type_map[hit_type[1]]
    }
    
    harvest_count <- harvest_count + 1
    harvest_out[[harvest_count]] <- tibble::tibble(
      sheet = sheet_label, trap_code = trap_code_val,
      ref_lat = as.numeric(latlon["lat"]), ref_lon = as.numeric(latlon["lon"]),
      ref_municipality = ref_municipality, ref_trap_type = ref_trap_type
    )
  }
}

coord_refs_raw <- dplyr::bind_rows(harvest_out) %>%
  dplyr::mutate(source_year = as.integer(stringr::str_extract(sheet, "\\d{4}")))

# every individual reference-block observation, unaggregated -- the raw
# material Section 2.4 disambiguates from when sources disagree
outdir <- "../main_db/clean_data/"
write.csv(coord_refs_raw, file = paste0(outdir, "liguria_traps_all_sources.csv"), row.names = FALSE)

# deduplicate by trap code. Not averaged/most-recent: both are unsafe
# (GE12 case, resolved in 2.4). All candidates stay in
# liguria_traps_all_sources.csv for that resolution step.
province_from_municipality <- c(
  "IMPERIA" = "Imperia", "VENTIMIGLIA" = "Imperia", "SANREMO" = "Imperia",
  "VALLECROSIA" = "Imperia", "CAMPOROSSO" = "Imperia", "DOLCEACQUA" = "Imperia",
  "TAGGIA" = "Imperia",
  "SAVONA" = "Savona", "ALBENGA" = "Savona", "VADO LIGURE" = "Savona",
  "LOANO" = "Savona", "CAIRO MONTENOTTE" = "Savona", "LEGINO" = "Savona",
  "VARAZZE" = "Savona",
  "GENOVA" = "Genova", "CHIAVARI" = "Genova", "LAVAGNA" = "Genova",
  "RAPALLO" = "Genova", "SESTRI LEVANTE" = "Genova", "ZOAGLI" = "Genova",
  "BUSALLA" = "Genova", "VALBREVENNA" = "Genova", "USCIO" = "Genova",
  "LA SPEZIA" = "La Spezia", "ARCOLA" = "La Spezia", "AMEGLIA" = "La Spezia",
  "BRUGNATO" = "La Spezia", "VEZZANO LIGURE" = "La Spezia"
)
code_prefix_province <- c("IM" = "Imperia", "SV" = "Savona", "GE" = "Genova", "SP" = "La Spezia")

# table()-based mode that returns NA_character_ (not NULL) when every
# value in the group is NA, so summarise() never breaks on an empty group
safe_mode <- function(x) {
  tab <- table(x)
  if (length(tab) == 0) return(NA_character_)
  names(sort(tab, decreasing = TRUE))[1]
}

traps <- coord_refs_raw %>%
  dplyr::group_by(trap_code) %>%
  dplyr::mutate(
    lat_spread = max(ref_lat, na.rm = TRUE) - min(ref_lat, na.rm = TRUE),
    lon_spread = max(ref_lon, na.rm = TRUE) - min(ref_lon, na.rm = TRUE)
  ) %>%
  dplyr::arrange(trap_code, dplyr::desc(source_year)) %>%
  dplyr::summarise(
    latitude = dplyr::first(ref_lat), longitude = dplyr::first(ref_lon),
    latest_source_sheet = dplyr::first(sheet),
    lat_spread = dplyr::first(lat_spread), lon_spread = dplyr::first(lon_spread),
    # mode (most frequent value); safe_mode returns NA_character_ when
    # every value in the group is NA (table() would otherwise return a
    # zero-length result, which errors when combined across groups --
    # e.g. spurious "trap codes" like "CO2", a trap-type token that
    # happens to also match the trap-code regex in 2.1)
    Municipality = safe_mode(ref_municipality),
    trap_type = safe_mode(ref_trap_type),
    n_sources = dplyr::n(), .groups = "drop"
  ) %>%
  dplyr::mutate(
    Province = dplyr::coalesce(
      province_from_municipality[toupper(Municipality)],
      code_prefix_province[substr(trap_code, 1, 2)]
    )
  )

nrow(traps)
print(traps, n = Inf)

# "most recent" is only a placeholder where sources agree; codes with
# >0.05 deg (~5 km) spread are flagged here and resolved in 2.4.
traps %>%
  dplyr::filter(n_sources > 1 & (lat_spread > 0.05 | lon_spread > 0.05)) %>%
  dplyr::select(trap_code, latitude, longitude, latest_source_sheet, lat_spread, lon_spread, n_sources) %>%
  print(n = Inf)

traps <- traps %>% dplyr::select(-lat_spread, -lon_spread)


##### 2.1b. trap-code aliases (confirmed by Annalisa Accorsi, IZSPLV, 2026) #####
# These codes are the same physical trap as their alias, recorded under a
# different code in some years/suffix variants. Additive only: if the code
# already resolved its own coordinate from harvesting, that is kept as-is;
# an alias row is added only to fill a gap, never to overwrite an existing
# entry. (GE1BIS / GE1GTBIS are explicitly NOT aliased -- Accorsi confirmed
# these are separate, untraceable emergency catches, not the same trap as
# GE1/GE1GT -- so they are deliberately left out of this table and fall
# through to the normal centroid fallback, same as any other unmapped code.)
trap_code_aliases <- tibble::tribble(
  ~trap_code, ~alias_of,
  "IM2GT",    "IM2",
  "SV1GT",    "SV1",
  "GE1GT",    "GE1"
)

alias_rows <- trap_code_aliases %>%
  dplyr::inner_join(
    traps %>% dplyr::select(alias_of = trap_code, latitude, longitude, Municipality, trap_type, Province, n_sources),
    by = "alias_of"
  ) %>%
  dplyr::filter(!trap_code %in% traps$trap_code) %>%
  dplyr::select(-alias_of)

if (nrow(alias_rows) > 0) {
  message(nrow(alias_rows), " trap-code alias(es) resolved from their target's coordinates: ",
          paste(alias_rows$trap_code, collapse = ", "))
}

traps <- dplyr::bind_rows(traps, alias_rows)

# resolved coordinate/Municipality/Province/trap-type per trap code,
# inspectable and correctable by hand; disagreeing sources still get
# the naive pick here, validated in 2.4.
write.csv(traps, file = paste0(outdir, "liguria_traps_reference.csv"), row.names = FALSE)


##### 2.2. Classify catch records #####

# a genuine record needs a parseable date, a plausible count, and a
# whitelisted Cx. pipiens spelling; rows that fail keep a status flag
# instead of being dropped. No custom functions: every step is inline.

# confirmed Cx. pipiens spelling variants (incl. 2 typos)
pipiens_variants <- c("CULEX PIPIENS", "CULEX PIPENS", "CUIEX PIPIENS")
# other species present in the raw data, out of scope but logged separately
offtarget_species <- c("OCHLEROTATUS CASPIUS", "CULEX SPP", "CULEX SP.")

classify_out <- list()
classify_count <- 0
n_skipped_empty <- 0

for (sheet_label in names(raw_sheets)) {
  sheet_df <- raw_sheets[[sheet_label]]
  n_rows <- nrow(sheet_df)
  
  for (i in seq_len(n_rows)) {
    row_vals <- as.list(sheet_df[i, ])
    
    # strip any glued-on reference-table fragment before scanning fields.
    # Same check as Section 2.1 -- see header note on why it's duplicated.
    liguria_marker <- integer(0)
    bare_province_marker <- integer(0)
    for (k in seq_along(row_vals)) {
      v <- row_vals[[k]]
      if (!is.na(v) && toupper(trimws(as.character(v))) == "LIGURIA") liguria_marker <- c(liguria_marker, k)
      if (!is.na(v) && toupper(trimws(as.character(v))) %in% c("IM", "SV", "GE", "SP")) bare_province_marker <- c(bare_province_marker, k)
    }
    starts <- c(
      if (length(liguria_marker) > 0) liguria_marker[1] - 2 else NA_integer_,
      if (length(bare_province_marker) > 0) bare_province_marker[1] else NA_integer_
    )
    starts <- starts[!is.na(starts)]
    ref_start <- if (length(starts) > 0) max(1, min(starts)) else NA_integer_
    if (!is.na(ref_start)) {
      row_vals <- if (ref_start == 1) list() else row_vals[seq_len(ref_start - 1)]
    }
    if (length(row_vals) == 0) { n_skipped_empty <- n_skipped_empty + 1; next }
    
    present_idx <- which(!sapply(row_vals, is.na))
    if (length(present_idx) == 0) { n_skipped_empty <- n_skipped_empty + 1; next }
    cells <- row_vals[present_idx]
    cells_chr <- toupper(trimws(sapply(cells, as.character)))
    
    species_idx <- which(cells_chr %in% pipiens_variants)
    offtarget_idx <- which(cells_chr %in% offtarget_species)
    
    # plausible date: native Excel date, or dd/mm/yyyy string within
    # 2013-2025; repairs one known truncated-year typo
    date_val <- as.Date(NA); date_pos <- NA_integer_
    for (j in seq_along(cells)) {
      x <- cells[[j]]
      d <- as.Date(NA)
      if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        dd <- as.Date(x)
        if (!is.na(dd) && dplyr::between(lubridate::year(dd), 2013, 2025)) d <- dd
      } else if (!is.na(x)) {
        xc <- trimws(as.character(x))
        if (xc != "") {
          xc <- stringr::str_replace(xc, "^(\\d{1,2}/\\d{1,2}/20)(\\d)$", "\\11\\2")
          dd <- suppressWarnings(lubridate::dmy(xc, quiet = TRUE))
          if (is.na(dd)) dd <- suppressWarnings(lubridate::ymd(xc, quiet = TRUE))
          # bare Excel serial date number stored as text (e.g. "41458", seen
          # whole-sheet in 2013/2018/2020/2022/2024): dmy()/ymd() can't parse
          # a serial number, so it needs an explicit numeric conversion
          if (is.na(dd) && grepl("^[0-9]+$", xc)) {
            serial_candidate <- suppressWarnings(as.numeric(xc))
            if (!is.na(serial_candidate)) dd <- as.Date(serial_candidate, origin = "1899-12-30")
          }
          if (!is.na(dd) && dplyr::between(lubridate::year(dd), 2013, 2025)) d <- dd
        }
      }
      if (!is.na(d)) { date_val <- d; date_pos <- j; break }
    }
    
    # plausible count: non-negative whole number <= 2000
    value_val <- NA_real_; value_pos <- NA_integer_
    for (j in seq_along(cells)) {
      if (!is.na(date_pos) && j == date_pos) next
      x <- cells[[j]]
      v <- NA_real_
      if (!is.na(x)) {
        nn <- suppressWarnings(as.numeric(as.character(x)))
        if (!is.na(nn) && nn >= 0 && nn <= 2000 && abs(nn - round(nn)) <= 1e-9) v <- round(nn)
      }
      if (!is.na(v)) { value_val <- v; value_pos <- j; break }
    }
    
    is_record <- length(species_idx) > 0 && !is.na(date_val) && !is.na(value_val)
    
    if (!is_record) {
      # date checked before value when both fail; reference/fragment
      # rows (no species token at all) are the catch-all
      status <- dplyr::case_when(
        length(offtarget_idx) > 0 & !is.na(date_val) & !is.na(value_val) ~ "off_target_species",
        length(species_idx) > 0 & is.na(date_val) ~ "invalid_date",
        length(species_idx) > 0 & is.na(value_val) ~ "invalid_value",
        length(species_idx) == 0 & length(offtarget_idx) == 0 &
          !is.na(date_val) & !is.na(value_val) ~ "no_species_match",
        TRUE ~ "reference_fragment"
      )
      classify_count <- classify_count + 1
      classify_out[[classify_count]] <- tibble::tibble(
        sheet = sheet_label, row_index = i, status = status,
        raw_row = paste(sapply(cells, as.character), collapse = " | "),
        date = date_val, value = value_val,
        trap_code = NA_character_, location_text = NA_character_
      )
      next
    }
    
    used_pos <- unique(c(date_pos, value_pos, species_idx))
    leftover <- cells[-used_pos]
    leftover_chr <- trimws(sapply(leftover, as.character))
    leftover_chr <- leftover_chr[leftover_chr != ""]
    
    # trap code extraction -- same check as Section 2.1, see header note
    code_candidates <- rep(NA_character_, length(leftover_chr))
    for (k in seq_along(leftover_chr)) {
      xc <- toupper(leftover_chr[k])
      if (grepl(code_regex, xc) && grepl("[0-9]", xc)) {
        code_candidates[k] <- xc
      } else {
        words <- stringr::str_split(xc, "\\s+")[[1]]
        hit <- words[grepl(code_regex, words) & grepl("[0-9]", words)]
        if (length(hit) > 0) code_candidates[k] <- hit[1]
      }
    }
    code_candidates <- code_candidates[!is.na(code_candidates)]
    trap_code_val <- if (length(code_candidates) > 0) code_candidates[1] else NA_character_
    
    classify_count <- classify_count + 1
    classify_out[[classify_count]] <- tibble::tibble(
      sheet = sheet_label, row_index = i, status = "record", raw_row = NA_character_,
      date = date_val, value = value_val, trap_code = trap_code_val,
      location_text = paste(leftover_chr, collapse = " | ")
    )
  }
}

all_classified <- dplyr::bind_rows(classify_out)
table(all_classified$status, useNA = "always")

excluded_review <- all_classified %>%
  dplyr::filter(status != "record") %>%
  dplyr::transmute(
    region = "liguria",
    check = status,
    sheet = sheet,
    row_index = row_index,
    Municipality = NA_character_,   # not yet resolved at this point in the pipeline (see 2.6)
    date = date,
    value_raw = value,
    reason = raw_row
  )

n_skipped_empty

if (n_skipped_empty > 0) {
  skipped_empty_row <- tibble::tibble(
    region = "liguria", check = "empty_or_fragment_row",
    sheet = NA_character_, row_index = NA_integer_,
    Municipality = NA_character_, date = as.Date(NA), value_raw = NA_real_,
    reason = paste0(n_skipped_empty, " row(s) across all sheets became empty after removing ",
                    "a reference-table fragment, or were entirely blank; not individually logged.")
  )
  excluded_review <- dplyr::bind_rows(excluded_review, skipped_empty_row)
}

print(excluded_review %>% dplyr::filter(check == "no_species_match"), n = Inf)

qc_log_path <- "../main_db/qc_exclusions_log.csv"

# idempotent write: drop this region's own prior rows before appending fresh
# ones, so re-running this script (e.g. after a fix) never double-counts
if (file.exists(qc_log_path)) {
  readr::read_csv(qc_log_path, show_col_types = FALSE) %>%
    dplyr::filter(region != "liguria") %>%
    readr::write_csv(qc_log_path)
}
readr::write_csv(excluded_review, qc_log_path, append = file.exists(qc_log_path))

catches <- all_classified %>%
  dplyr::filter(status == "record") %>%
  dplyr::select(-status, -raw_row)

nrow(catches)
write.csv(catches, file = paste0(outdir, "liguria_catches_raw.csv"), row.names = FALSE)


##### 2.3. Re-read extracted tables & load geographic data #####

# reading back from disk decouples this from 2.1/2.2: can be re-run
# alone after manually correcting a coordinate in liguria_traps_reference.csv
traps <- readr::read_csv(paste0(outdir, "liguria_traps_reference.csv"), show_col_types = FALSE)
catches <- readr::read_csv(paste0(outdir, "liguria_catches_raw.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(date = as.Date(date))

db <- catches
db$note <- NA_character_

geojson <- sf::st_read("../other_data/comuni.geojson") %>%
  dplyr::filter(reg_name %in% "Liguria")

# sourced for consistency with the shared script header; species is
# hardcoded to Culex pipiens only, not joined against
source("../script/0.0_taxonomy_lookup.R")


##### 2.4. Resolve disagreeing trap coordinates (land-polygon check) #####

# for codes disagreeing by >~5 km (flagged in 2.1), each candidate is
# tested against the land polygon; the one on-land candidate wins. Zero
# or multiple on-land matches stay at the naive pick, printed for review.
coord_refs_all <- readr::read_csv(paste0(outdir, "liguria_traps_all_sources.csv"), show_col_types = FALSE)

disagreeing_codes <- coord_refs_all %>%
  dplyr::group_by(trap_code) %>%
  dplyr::filter(dplyr::n_distinct(round(ref_lat, 2), round(ref_lon, 2)) > 1) %>%
  dplyr::ungroup()

if (nrow(disagreeing_codes) > 0) {
  disagreeing_sf <- disagreeing_codes %>%
    sf::st_as_sf(coords = c("ref_lon", "ref_lat"), crs = 4326, remove = FALSE)
  disagreeing_codes$on_land <- lengths(sf::st_within(disagreeing_sf, sf::st_union(geojson))) > 0
  
  resolved <- disagreeing_codes %>%
    dplyr::group_by(trap_code) %>%
    dplyr::filter(sum(on_land) == 1, on_land) %>%
    dplyr::ungroup() %>%
    dplyr::select(trap_code, resolved_lat = ref_lat, resolved_lon = ref_lon, resolved_source = sheet)
  
  message(nrow(resolved), " of ", dplyr::n_distinct(disagreeing_codes$trap_code),
          " disagreeing trap code(s) resolved to a single on-land candidate.")
  print(resolved, n = Inf)
  
  unresolved <- setdiff(unique(disagreeing_codes$trap_code), resolved$trap_code)
  if (length(unresolved) > 0) {
    message("NOT auto-resolved (zero/multiple on-land candidates), needs manual review: ",
            paste(unresolved, collapse = ", "))
    disagreeing_codes %>%
      dplyr::filter(trap_code %in% unresolved) %>%
      dplyr::select(trap_code, sheet, ref_lat, ref_lon, on_land) %>%
      print(n = Inf)
  }
  
  traps <- traps %>%
    dplyr::left_join(resolved, by = "trap_code") %>%
    dplyr::mutate(
      latitude = dplyr::coalesce(resolved_lat, latitude),
      longitude = dplyr::coalesce(resolved_lon, longitude),
      trap_note = ifelse(!is.na(resolved_lat),
                         "Coordinates disagreed by more than ~5 km across seasonal reference sheets; resolved using the candidate that falls on land.",
                         NA_character_)
    ) %>%
    dplyr::select(-resolved_lat, -resolved_lon, -resolved_source)
} else {
  traps$trap_note <- NA_character_
}

# manual override: coordinates confirmed by Annalisa Accorsi (IZSPLV, 2026)
# from external/paper sources, taking precedence over whatever coordinate
# (if any) was harvested for these codes from the digitised sheets.
#   - GE12 (2018): Parco Botanico Villa Rocca, Chiavari -- inland, not coastal.
#     Had its own (bad) harvested coordinate, so already had a row in
#     `traps` before this block runs.
#   - GE26 (2023): Villa Rocca, per ASL technicians' verbali -- point-in-
#     polygon confirms this falls in Rapallo, not Chiavari, consistent with
#     the separate municipality correction in Section 2.9b (GE26 declared
#     "Chiavari"/"Lavagna" -> corrected to "Rapallo"); the two fixes are
#     complementary (this one supplies the coordinate, 2.9b covers the
#     case where the source's own municipality label is still wrong).
#     Unlike GE12, GE26 has NO coordinate of its own anywhere in the
#     digitised sheets (only the municipality mention) -- so it has no
#     pre-existing row in `traps` for a left_join to update. full_join is
#     used below (not left_join) so this case still creates a row, exactly
#     like the alias mechanism in 2.1b.
manual_coordinate_overrides <- tibble::tribble(
  ~trap_code, ~override_lat, ~override_lon,
  "GE12",     44.31729,      9.32829,
  "GE26",     44.355450,     9.201874
)

traps <- traps %>%
  dplyr::full_join(manual_coordinate_overrides, by = "trap_code") %>%
  dplyr::mutate(
    latitude = dplyr::coalesce(override_lat, latitude),
    longitude = dplyr::coalesce(override_lon, longitude),
    override_note = dplyr::case_when(
      trap_code == "GE12" ~ "Coordinates corrected from a bad GPS entry (~19 km offshore) to a manually verified location (Parco Botanico Villa Rocca, Chiavari).",
      trap_code == "GE26" ~ "No coordinate was available for this trap code in the digitised sheets (only a municipality mention); coordinates supplied from ASL technicians' on-site verbali (Villa Rocca, Rapallo).",
      !is.na(override_lat) ~ "Coordinates manually verified and corrected (source: Annalisa Accorsi, IZSPLV, 2026).",
      TRUE ~ NA_character_
    ),
    trap_note = dplyr::case_when(
      !is.na(trap_note) & !is.na(override_note) ~ paste(trap_note, override_note, sep = " | "),
      is.na(trap_note) & !is.na(override_note)  ~ override_note,
      TRUE ~ trap_note
    )
  ) %>%
  dplyr::select(-override_lat, -override_lon, -override_note)


##### 2.5. Join coordinates by trap code #####

# trap coordinates are treated as stable across years for any code in
# the reference table (2.1). "H"-prefixed codes (from 2024) have no
# reference and stay NA, resolved by centroid in 2.7.
db <- db %>%
  dplyr::left_join(traps %>% dplyr::select(trap_code, latitude, longitude, trap_note), by = "trap_code") %>%
  dplyr::mutate(
    note = dplyr::case_when(
      !is.na(note) & !is.na(trap_note) ~ paste(note, trap_note, sep = " | "),
      is.na(note) & !is.na(trap_note)  ~ trap_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-trap_note)

db %>%
  dplyr::filter(is.na(latitude)) %>%
  dplyr::count(trap_code, sort = TRUE) %>%
  print(n = Inf)


##### 2.6. Municipality and province #####

# precedence: (1) the record's own location text; (2) trap-code join
# against "traps" (lets 2013-2015 records with no text get a real
# municipality); (3) centroid imputation (2.7).
db <- db %>%
  dplyr::left_join(
    traps %>% dplyr::select(trap_code, ref_municipality = Municipality, ref_province = Province),
    by = "trap_code"
  )

# municipality match against each record's own location text
find_municipality_in_text <- function(loc) {
  if (is.na(loc)) return(NA_character_)
  txt_upper <- toupper(loc)
  hit_municipality <- liguria_municipalities[stringr::str_detect(txt_upper, liguria_municipalities)]
  if (length(hit_municipality) > 0) {
    return(dplyr::coalesce(hamlet_correction[hit_municipality[1]], hit_municipality[1]))
  }
  hit_facility <- names(facility_correction)[stringr::str_detect(txt_upper, names(facility_correction))]
  if (length(hit_facility) > 0) return(unname(facility_correction[hit_facility[1]]))
  NA_character_
}
municipality_from_text <- vapply(db$location_text, find_municipality_in_text, character(1), USE.NAMES = FALSE)

# which facility name (if any) drove the match above, kept separately so a
# specific note can be written rather than silently folding it into the
# same path as a direct municipality match
find_facility_hit <- function(loc) {
  if (is.na(loc)) return(NA_character_)
  txt_upper <- toupper(loc)
  if (length(liguria_municipalities[stringr::str_detect(txt_upper, liguria_municipalities)]) > 0) return(NA_character_)
  hit_facility <- names(facility_correction)[stringr::str_detect(txt_upper, names(facility_correction))]
  if (length(hit_facility) > 0) return(hit_facility[1])
  NA_character_
}
facility_hit_from_text <- vapply(db$location_text, find_facility_hit, character(1), USE.NAMES = FALSE)

db <- db %>%
  dplyr::mutate(
    Municipality_upper = dplyr::coalesce(municipality_from_text, toupper(ref_municipality)),
    Municipality = stringr::str_to_title(Municipality_upper),
    Province = dplyr::coalesce(
      province_from_municipality[Municipality_upper], ref_province, code_prefix_province[substr(trap_code, 1, 2)]
    ),
    Region = "Liguria",
    hamlet_note = ifelse(
      Municipality_upper == "SAVONA" &
        (stringr::str_detect(toupper(dplyr::coalesce(location_text, "")), "LEGINO") |
           toupper(dplyr::coalesce(ref_municipality, "")) == "LEGINO"),
      "Municipality corrected from 'Legino' (a hamlet of Savona with no independent ISTAT polygon) to 'Savona'.",
      NA_character_
    ),
    facility_hit = facility_hit_from_text,
    facility_note = ifelse(
      !is.na(facility_hit),
      sprintf("Municipality derived from facility name '%s' (verified individually), not a direct municipality match.", facility_hit),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(hamlet_note) ~ paste(note, hamlet_note, sep = " | "),
      is.na(note) & !is.na(hamlet_note)  ~ hamlet_note,
      TRUE ~ note
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(facility_note) ~ paste(note, facility_note, sep = " | "),
      is.na(note) & !is.na(facility_note)  ~ facility_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-ref_municipality, -ref_province, -hamlet_note, -facility_hit, -facility_note)

db %>% dplyr::filter(is.na(Municipality)) %>%
  dplyr::select(sheet, trap_code, location_text) %>% dplyr::distinct() %>% print(n = Inf)

db %>% dplyr::filter(is.na(Province)) %>%
  dplyr::select(sheet, trap_code, location_text) %>% dplyr::distinct() %>% print(n = Inf)


##### 2.6b. Sea point check #####

# Liguria's entire regional coastline means a recorded trap coordinate
# can genuinely land offshore. Same row-level check used for
# Sicilia/Sardegna/Emilia-Romagna/Puglia-Basilicata: only the specific
# reading is invalidated (set to NA), not the whole municipality; it is
# then picked up automatically by the existing missing-coordinate ->
# centroid fallback in Section 2.7 (coord_missing already treats
# is.na(latitude) as bad). Runs on the coordinate as resolved by 2.4/2.5
# (disagreeing-source resolution and the GE12/GE26 manual overrides
# already applied), so a correctly-fixed trap like GE12 or GE26 will not
# be re-flagged.
geojson_valid <- sf::st_make_valid(geojson)

db_sf <- db %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

sea_points <- db_sf %>%
  dplyr::filter(lengths(sf::st_intersects(geometry, geojson_valid)) == 0) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(trap_code, Municipality, latitude, longitude) %>%
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


##### 2.7. Coordinate check & centroid imputation #####

summary(db[, c("latitude", "longitude")])

db %>%
  dplyr::filter(!is.na(latitude) &
                  (!dplyr::between(latitude, lig_lat_range[1], lig_lat_range[2]) |
                     !dplyr::between(longitude, lig_lon_range[1], lig_lon_range[2]))) %>%
  dplyr::select(trap_code, Municipality, latitude, longitude) %>%
  dplyr::distinct()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = geojson, fill = "lightgrey", color = "white", linewidth = 0.1) +
  ggplot2::geom_point(data = dplyr::filter(db, !is.na(latitude)),
                      ggplot2::aes(x = longitude, y = latitude), color = "red", size = 1.5, alpha = 0.6) +
  ggplot2::coord_sf(xlim = lig_lon_range, ylim = lig_lat_range) +
  ggplot2::theme_bw()

# municipality lat and lon merge if not lat e lon in original file
geojson_lig_coords <- geojson |>
  sf::st_transform(crs = 3857) |> sf::st_centroid() |> sf::st_transform(crs = 4326) |>
  dplyr::mutate(
    lon_centroid = sf::st_coordinates(geometry)[, "X"],
    lat_centroid = sf::st_coordinates(geometry)[, "Y"],
    name_upper = toupper(name)
  ) |>
  sf::st_drop_geometry() %>%
  dplyr::select(name_upper, lon_centroid, lat_centroid)

if (!"note" %in% names(db)) db$note <- NA_character_

db <- db %>%
  dplyr::left_join(geojson_lig_coords, by = c("Municipality_upper" = "name_upper")) %>%
  dplyr::mutate(
    coord_missing = is.na(latitude) | is.na(longitude),
    coord_invalid = !coord_missing &
      (!dplyr::between(latitude, lig_lat_range[1], lig_lat_range[2]) |
         !dplyr::between(longitude, lig_lon_range[1], lig_lon_range[2])),
    is_bad_coord = coord_missing | coord_invalid,
    centroid_found = is_bad_coord & !is.na(lon_centroid),
    municipality_centroid = ifelse(centroid_found, "yes", "no"),
    centroid_note = dplyr::case_when(
      centroid_found & coord_missing ~ "Coordinates were missing; derived from municipality centroid.",
      centroid_found & coord_invalid ~ "Coordinates were present but outside the plausible Liguria bounding box; derived from municipality centroid.",
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
  dplyr::select(-lon_centroid, -lat_centroid, -is_bad_coord, -coord_missing, -coord_invalid,
                -centroid_note, -centroid_found, -Municipality_upper)

# last resort: rows with a trap code but no municipality text (e.g.
# "GE1BIS" alone) fall back to the province mean centroid -- coarser, flagged
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
                           "No trap-code reference or municipality match available; derived from province-level centroid (low confidence).",
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

db %>% dplyr::filter(is.na(latitude) | is.na(longitude)) %>%
  dplyr::select(sheet, trap_code, Municipality) %>% dplyr::distinct()

table(db$municipality_centroid, useNA = "always")


##### 2.8. Reverse-geocode remaining missing municipality #####

# every row now has a coordinate (trap match, municipality or province
# centroid); any remaining missing Municipality is resolved by checking
# which municipality polygon the point falls inside.
db <- db %>% dplyr::mutate(.row_id = dplyr::row_number())

needs_reverse_geocode <- db %>%
  dplyr::filter(is.na(Municipality) & !is.na(latitude) & !is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

if (nrow(needs_reverse_geocode) > 0) {
  reverse_matched <- sf::st_join(needs_reverse_geocode, geojson %>% dplyr::select(name, prov_name)) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.row_id) %>%
    dplyr::slice(1) %>%   # boundary points can match >1 polygon; keep first
    dplyr::ungroup() %>%
    dplyr::select(.row_id, rev_municipality = name, rev_province = prov_name)
  
  db <- db %>%
    dplyr::left_join(reverse_matched, by = ".row_id") %>%
    dplyr::mutate(
      used_reverse_geocode = is.na(Municipality) & !is.na(rev_municipality),
      reverse_geocode_note = ifelse(used_reverse_geocode,
                                    "Municipality was unknown; derived by reverse-geocoding the resolved coordinate.",
                                    NA_character_),
      note = dplyr::case_when(
        !is.na(note) & !is.na(reverse_geocode_note) ~ paste(note, reverse_geocode_note, sep = " | "),
        is.na(note) & !is.na(reverse_geocode_note)  ~ reverse_geocode_note,
        TRUE ~ note
      ),
      Municipality = dplyr::coalesce(Municipality, stringr::str_to_title(rev_municipality)),
      Province = dplyr::coalesce(Province, rev_province)
    ) %>%
    dplyr::select(-rev_municipality, -rev_province, -used_reverse_geocode, -reverse_geocode_note)
}

db <- db %>% dplyr::select(-.row_id)

# still missing = resolved coordinate falls outside any municipality
# polygon (e.g. a province centroid landing in a gap); should be rare
db %>% dplyr::filter(is.na(Municipality)) %>%
  dplyr::select(sheet, trap_code, latitude, longitude) %>% dplyr::distinct() %>% print(n = Inf)


##### 2.9. Municipality ISTAT match diagnostic #####
muni_not_match <- setdiff(toupper(trimws(db$Municipality)), toupper(trimws(geojson$name)))
length(muni_not_match)
print(muni_not_match)


##### 2.9b. municipality correction: point-in-polygon verified errors #####
# Confirmed by the national merge script's point-in-polygon check and
# visually verified against ISTAT boundaries (see project chat log): in
# every one of these six cases the point falls unambiguously inside the
# corrected municipality, well clear of any shared boundary -- none are
# borderline cases. trap_code "GE26" appears under two different declared
# municipalities over time (Chiavari and Lavagna) at the same coordinate;
# both are corrected to the same value, Rapallo. Scoped by
# (trap_code, declared Municipality) pair, not by Municipality name alone,
# since other Liguria municipalities in this mismatch set (e.g. Imperia)
# have multiple genuinely distinct trap locations, only some of which are
# wrong.
muni_ptcheck_fixes <- tibble::tribble(
  ~trap_code, ~declared,   ~corrected,
  "SV07GT",   "SAVONA",    "Albenga",
  "GE26",     "CHIAVARI",  "Rapallo",
  "GE26",     "LAVAGNA",   "Rapallo",
  "IM15",     "IMPERIA",   "Camporosso",
  "GE7",      "BUSALLA",   "Genova",
  "IM14",     "IMPERIA",   "Taggia"
) %>%
  dplyr::mutate(trap_code = toupper(trimws(trap_code)), declared = toupper(trimws(declared)))

db <- db %>%
  dplyr::mutate(trap_code_upper = toupper(trimws(trap_code)),
                municipality_upper_check = toupper(trimws(Municipality))) %>%
  dplyr::left_join(
    muni_ptcheck_fixes,
    by = c("trap_code_upper" = "trap_code", "municipality_upper_check" = "declared")
  ) %>%
  dplyr::mutate(
    municipality_before_ptcheck = Municipality,
    Municipality = dplyr::coalesce(corrected, Municipality),
    ptcheck_note = dplyr::if_else(
      !is.na(corrected),
      sprintf("Municipality corrected from '%s' to '%s': point-in-polygon check against the recorded coordinates showed the trap falls unambiguously within %s, not the declared municipality (visually verified, not a borderline case).",
              municipality_before_ptcheck, corrected, corrected),
      NA_character_
    ),
    note = dplyr::case_when(
      !is.na(note) & !is.na(ptcheck_note) ~ paste(note, ptcheck_note, sep = " | "),
      is.na(note) & !is.na(ptcheck_note)  ~ ptcheck_note,
      TRUE ~ note
    )
  ) %>%
  dplyr::select(-trap_code_upper, -municipality_upper_check, -corrected,
                -municipality_before_ptcheck, -ptcheck_note)


##### 2.10. Add columns with NA or fixed values #####
db <- db %>%
  dplyr::left_join(traps %>% dplyr::select(trap_code, ref_trap_type = trap_type), by = "trap_code")

# trap-type match against each record's own location text
find_trap_type_in_text <- function(loc) {
  if (is.na(loc)) return(NA_character_)
  tokens <- toupper(trimws(stringr::str_split(loc, "\\|")[[1]]))
  hit_type <- tokens[tokens %in% names(trap_type_map)]
  if (length(hit_type) == 0) return(NA_character_)
  unname(trap_type_map[hit_type[1]])
}
trap_type_from_text <- vapply(db$location_text, find_trap_type_in_text, character(1), USE.NAMES = FALSE)

db <- db %>%
  dplyr::mutate(trap_type = dplyr::coalesce(trap_type_from_text, ref_trap_type)) %>%
  dplyr::select(-ref_trap_type)

table(db$trap_type, useNA = "always")

db$Country <- "Italy"
db$Institute <- "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta"
db$contact_person <- "Annalisa Accorsi"
db$contact_person_email <- "annalisa.accorsi@izsplv.it"
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
db$sex <- "F"   # confirmed by IZSPLV: all Cx. pipiens counts are female

db <- db %>% dplyr::rename(id_trap = trap_code)

db <- db %>%
  dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date))
# db$volume <- NA
# db$substrate <- NA
# db$larvicide_presence <- NA
# db$larvicide_type <- NA
# db$WNV_test <- NA
# db$n_pool <- NA


##### 2.11. Order columns #####

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
  "Year outside expected 2013-2025 range" = all(dplyr::between(db$year, 2013, 2025)),
  "Missing coordinates remain after centroid imputation" = all(!is.na(db$latitude) & !is.na(db$longitude)),
  "Missing Municipality remains after all resolution tiers" = all(!is.na(db$Municipality)),
  "Missing Province remains after all resolution tiers" = all(!is.na(db$Province))
)


#### 3. Save clean data ####
write.csv(db, file = paste0(outdir, "liguria_samplings_clean.csv"), row.names = FALSE)
