# =============================================================================
# DESCRIPTIVE ANALYSIS — Harmonised dataset of Culex pipiens, Italy (2008–2022)
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
# Purpose   : Descriptive statistics, spatial/temporal data visualization, and
#             sampling effort evaluation of the harmonised dataset of adult
#             Culex pipiens mosquitoes from nationwide surveillance in Italy.
#             Produces summary tables, in-text numbers, and figures for the 
#             manuscript (Data Paper).
#
# Sections  :
#    0. Libraries & Working Directory
#    1. Data Loading (Trap records, Aggregated DB, Shapefiles, WNV Risk Zones)
#    2. Configurations (Colorblind palettes, Labels, Risk categorization)
#    3. Descriptive Analysis & Sampling Effort (Summary metrics and Table 1)
#    7. Spatiotemporal Evolution of WNV Risk Zoning (Figure 1)
#    4. Temporal & Geographical Range (Figure 2A and 2B)
#    5. In-Text Numbers (Extracting peaks and seasonal limits for Section 4)
#    6. Seasonal Profiles (Figure 3A and 3C)
#    8. Spatial Distribution of Median Abundance (Figure 3B)
# =============================================================================

#### 0. LIBRARIES & SETTINGS ####
rm(list = ls())

# libraries
library(dplyr)
library(tidyverse)
library(sf)
library(readr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(giscoR)
library(stringi)
library(patchwork)
library(grid)
library(RColorBrewer)
library(ggnewscale)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(tiff)

# working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### 1. DATA ####

##### Non-aggregated Cx. pipiens trap records #####
# Raw trap-level records used to derive sampling effort descriptive statistics
# (number of traps, weeks, grid cells). Abruzzo and Molise excluded as data
# were provided by IZSTERAMO and are not part of the harmonised dataset.
culex_dis <- read.csv("../main_db/db_samplings_clean.csv")

# culex_dis <- culex_dis %>%
#   dplyr::filter(!Region %in% c("Abruzzo", "Molise"))

# culex_dis$Region <- droplevels(as.factor(culex_dis$Region))

culex_dis1 <- culex_dis %>%
  dplyr::filter(year %in% c(2008:2022)) %>% 
  dplyr::filter(species == "pipiens")

##### Spatially aggregated Cx. pipiens dataset (9x9 km grid) #####
# Core analysis-ready dataset: daily median abundance per grid cell,
# stratified by trap type and mosquito sex (2008–2022).
# See Section 2.1.4 for aggregation protocol.
culex <- read.csv("../main_db/db_aggregated.csv", sep = ",")

# culex <- culex %>%
#   dplyr::filter(!Region %in% c("Abruzzo", "Molise"))

# culex$Region <- droplevels(as.factor(culex$Region))

# Filter to study period (2008–2022)
db <- culex %>%
  dplyr::filter(year %in% c(2008:2022))

# Recode region names to English for consistency with manuscript
db <- db %>%
  dplyr::mutate(Region = recode(Region,
                                "Toscana"                = "Tuscany",
                                "Trentino-Alto Adige"    = "Autonomous Province of Trento",
                                "Friuli Venezia Giulia"  = "Friuli-Venezia Giulia",
                                "Piemonte"               = "Piedmont",
                                "Lombardia"              = "Lombardy",
                                "Puglia"                 = "Apulia",
                                "Sicilia"                = "Sicily",
                                "Sardegna"               = "Sardinia"
  ))

##### Italian administrative boundaries (ISTAT shapefiles) #####
# Regional, provincial, and municipal polygons used for spatial joins,
# georeferencing, and map production. Source: ISTAT 2022.
ita_reg  <- sf::st_read("../other_data/italy_boundaries/Reg01012022/Reg01012022_WGS84.shp")
ita_prov <- sf::st_read("../other_data/italy_boundaries/ProvCM01012022/ProvCM01012022_WGS84.shp")
ita_muni <- sf::st_read("../other_data/comuni.geojson") %>%
  sf::st_make_valid()

trento <- ita_prov %>% 
  dplyr::filter(DEN_PROV == "Trento")

##### European country boundaries (background map) #####
# Cropped to the Western Mediterranean and Central Europe extent,
# then reprojected to match the CRS of the Italian shapefiles.
world_raw <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

world_cropped <- sf::st_crop(world_raw,
                             xmin = -10, xmax = 45,
                             ymin =  30, ymax = 60)

world_map_utm <- sf::st_transform(world_cropped, sf::st_crs(ita_reg))

##### WNV risk area classification database #####
# Historical zoning of WNV surveillance areas in Italy (2008–2025),
# as defined by successive Ministerial decrees and National Plans (PNA).
# See Section 2.1.2 for the full description of zoning evolution.
risk_area <- readxl::read_excel("../main_db/db_risk_areas_italy.xlsx")


#### 2. CONFIGURATIONS (colours & labels) ####
# Colorblind-safe palette for contributing institutes (one colour per institute)
# UPDATE: added Liguria, sharing IPLA's colour/label -- the original label
# ("IPLA and IZSPLV") already anticipated grouping Piedmont (IPLA) and
# Liguria (IZSPLV) under one legend entry. Added Puglia & Basilicata,
# reusing the colour that was reserved for Abruzzo & Molise: that region
# stays excluded from these plots (data provided by IZSTERAMO, not part
# of the harmonised dataset -- see Section 1), so its colour slot was
# otherwise sitting unused rather than genuinely free; if Abruzzo & Molise
# is ever reactivated here, IZSPB will need a colour of its own instead.
manual_colors_colorblind <- c(
  "Fondazione Edmund Mach"                                                          = "#E69F00",
  "Istituto per le Piante da Legno e l'Ambiente"                                    = "#56B4E9",
  "Istituto Zooprofilattico Sperimentale del Lazio e della Toscana M. Aleandri"     = "#009E73",
  "Istituto Zooprofilattico Sperimentale del Mezzogiorno"                           = "#F0E442",
  # "Istituto Zooprofilattico Sperimentale dell'Abruzzo e del Molise"                 = "#0072B2",
  "Istituto Zooprofilattico Sperimentale della Puglia e della Basilicata"           = "#0072B2",
  "Istituto Zooprofilattico Sperimentale dell'Umbria e delle Marche Togo Rosati"    = "#D55E00",
  "Istituto Zooprofilattico Sperimentale della Sardegna"                            = "#CC79A7",
  "Istituto Zooprofilattico Sperimentale della Sicilia A. Mirri"                    = "#B042F4",
  "Istituto Zooprofilattico Sperimentale delle Venezie"                             = "#66FF66",
  "Istituto Zooprofilattico Sperimentale Lombardia ed Emilia-Romagna"               = "#C51B8A",
  "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta"     = "#56B4E9",
  "IPLA and IZSPLV"                                                                 = "#56B4E9"
)

# Abbreviated institute labels for figure legends (Table 1 / Figure 2 notation)
institute_labels <- c(
  "Fondazione Edmund Mach"                                                          = "FEM",
  "Istituto per le Piante da Legno e l'Ambiente"                                    = "IPLA and IZSPLV",
  "Istituto Zooprofilattico Sperimentale del Lazio e della Toscana M. Aleandri"     = "IZSLT",
  "Istituto Zooprofilattico Sperimentale del Mezzogiorno"                           = "IZSMPORTICI",
  # "Istituto Zooprofilattico Sperimentale dell'Abruzzo e del Molise"                 = "IZSTERAMO",
  "Istituto Zooprofilattico Sperimentale della Puglia e della Basilicata"           = "IZSPB",
  "Istituto Zooprofilattico Sperimentale dell'Umbria e delle Marche Togo Rosati"    = "IZSUM",
  "Istituto Zooprofilattico Sperimentale della Sardegna"                            = "IZSSARDEGNA",
  "Istituto Zooprofilattico Sperimentale della Sicilia A. Mirri"                    = "IZSSICILIA",
  "Istituto Zooprofilattico Sperimentale delle Venezie"                             = "IZSVE",
  "Istituto Zooprofilattico Sperimentale Lombardia ed Emilia-Romagna"               = "IZSLER",
  "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta"     = "IPLA and IZSPLV"
)

# North-to-south ordering of regions for consistent axis display across figures
region_north_south <- c(
  "Liguria", "Piedmont", "Lombardy", "Autonomous Province of Trento", "Veneto", "Friuli-Venezia Giulia",
  "Emilia-Romagna", "Tuscany", "Umbria", "Marche", "Lazio",
  "Campania", "Calabria", "Apulia", "Basilicata", "Sardinia", "Sicily"
)

# Three-level risk classification scheme used in Figure 1
# (harmonised across the four surveillance periods 2008–2025;
# see Section 2.1.2 for period-specific definitions)
group_colors <- c(
  "High Risk"   = "#a50f15",   # ACV / Endemic area / High risk area
  "Medium Risk" = "#E69F00",   # External surveillance area / Low risk area
  "Low Risk"    = "#009E73"    # Minimum risk area / Risk area
)

#' Assign harmonised three-level risk group from raw risk classification labels
#'
#' Maps the heterogeneous risk labels used across regulatory periods (2008–2025)
#' onto three comparable categories: High Risk, Medium Risk, and Low Risk.
#'
#' @param risk_col Character vector of raw risk labels
#' @return Character vector of harmonised risk group labels
assign_risk_group <- function(risk_col) {
  dplyr::case_when(
    risk_col %in% c("ACV", "Endemic area", "High risk area")                                     ~ "High Risk",
    risk_col %in% c("External surveillance area", "External surveilance area", "Low risk area")  ~ "Medium Risk",
    risk_col %in% c("Minimum risk area", "Risk area")                                            ~ "Low Risk",
    TRUE ~ NA_character_
  )
}


#### 3. DESCRIPTIVE ANALYSIS #####################################################
##### 3.1. Sampling effort: number of traps per region, year, and trap type #####
# Total raw records (pipiens only, 2008-2022)
nrow(culex_dis1)  # 58888

# Unique sampling events (pipiens, lat/lon × week × year × trap_type)
culex_dis1 %>%
  dplyr::summarise(
    n_unique_events = n_distinct(latitude, longitude, week, year, trap_type, na.rm = TRUE)
  )  # 37070

# Final aggregated dataset
nrow(db)  # 50759


# Proxy trap IDs are derived from coordinates where available, or from
# municipality names for regions lacking precise georeferencing (Campania,
# Calabria, Trentino). See Section 2.1.3 for georeferencing procedures.
traps_dis <- culex_dis %>%
  dplyr::mutate(
    proxy_trap_id = dplyr::case_when(
      Region %in% c("Campania", "Calabria", "Trentino-Alto Adige") ~ as.character(Municipality),
      !is.na(latitude) & !is.na(longitude)              ~ paste(latitude, longitude, sep = "_"),
      TRUE                                               ~ NA_character_
    )
  ) %>%
  dplyr::group_by(Region, year, trap_type) %>%
  dplyr::summarise(
    n_traps    = dplyr::n_distinct(proxy_trap_id, na.rm = TRUE),
    n_traps_na = sum(is.na(latitude) | is.na(longitude)),
    .groups = "drop"
  )

print(traps_dis, n = 221)

# Total distinct sampling events by trap type in the aggregated dataset
db %>%
  dplyr::distinct(cell_id, year, week, trap_type) %>%
  dplyr::group_by(trap_type) %>%
  dplyr::summarise(n_events = n(), .groups = "drop") %>%
  dplyr::arrange(desc(n_events))

# Breakdown by trap type and region to verify which regions drive each method
db %>%
  dplyr::distinct(cell_id, year, week, trap_type, Region) %>%
  dplyr::group_by(trap_type, Region) %>%
  dplyr::summarise(
    n_events = n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(trap_type, desc(n_events)) %>% 
  print(n = Inf)


##### 3.2. Number of unique sampling locations in the aggregated dataset #####
# Unique 9x9 km grid cells monitored nationwide
db %>%
  dplyr::summarise(n_cells = n_distinct(cell_id))

# Locations are identified by unique latitude/longitude pairs to avoid
# double-counting traps deployed at the same site across years.
db %>%
  dplyr::group_by(Region, trap_type) %>%
  dplyr::summarise(
    n_traps    = n_distinct(latitude, longitude, na.rm = TRUE),
    n_traps_na = sum(is.na(latitude) | is.na(longitude))
  ) %>% 
  print(n = Inf)

traps <- culex %>%
  dplyr::group_by(Region, year, trap_type) %>%
  dplyr::summarise(
    n_traps    = n_distinct(latitude, longitude, na.rm = TRUE),
    n_traps_na = sum(is.na(latitude) | is.na(longitude))
  )

print(traps, n = Inf)

# Barplot of trap counts by year, region, and trap type (Not in the paper)
ggplot2::ggplot(traps, aes(x = year, y = n_traps, fill = factor(trap_type))) +
  geom_col() +
  scale_fill_brewer(palette = "Set2", na.value = "grey40") +
  facet_wrap(~ Region) +
  labs(x = "Year", y = "Number of traps", fill = "Trap type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )


##### 3.4. average monitoring duration #####
db %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(
    n_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  dplyr::summarise(
    mean_years = mean(n_years),
    sd_years   = sd(n_years)
  )


##### 3.3. Table 1: mean annual sampling effort per region and trap type #####
# For each region, the yearly mean number of active traps is computed over the
# region-specific monitoring period (min–max year), filling unsampled years
# with zero to avoid overestimating effort in discontinuous time series.

# --- diagnostic: quantify what we're excluding, for reporting in the manuscript ---
db %>%
  dplyr::filter(is.na(trap_type)) %>%
  dplyr::group_by(Region, year) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    n_unique_traps = dplyr::n_distinct(latitude, longitude),
    .groups = "drop"
  ) %>%
  print()

# --- exclude NA trap_type from all downstream statistics in this section ---
db_trapstats <- db %>% dplyr::filter(!is.na(trap_type))

# define the monitoring period for each region
region_years <- db_trapstats %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(min_year = min(year), max_year = max(year), .groups = "drop") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(year = list(seq(min_year, max_year))) %>%
  tidyr::unnest(year) %>%
  dplyr::select(Region, year)

region_years

# count unique trap locations per year × region × trap type
trap_counts <- db_trapstats %>%
  dplyr::group_by(year, Region, trap_type) %>%
  dplyr::summarise(
    n_traps = dplyr::n_distinct(latitude, longitude, na.rm = TRUE),
    .groups = "drop"
  )

# expand to the full region-specific year range and fill gaps with zero
trap_counts_complete <- region_years %>%
  tidyr::crossing(trap_type = unique(trap_counts$trap_type)) %>%
  dplyr::left_join(trap_counts, by = c("Region", "year", "trap_type")) %>%
  dplyr::mutate(n_traps = tidyr::replace_na(n_traps, 0))

# compute yearly mean ± SD over the region-specific monitoring period
trap_counts_complete %>%
  dplyr::group_by(Region, trap_type) %>%
  dplyr::summarise(
    yearly_mean_traps = mean(n_traps, na.rm = TRUE),
    sd_traps          = sd(n_traps, na.rm = TRUE),
    n_years           = n_distinct(year),
    year_range        = paste(min(year), max(year), sep = "–"),
    .groups = "drop"
  ) %>%
  dplyr::filter(yearly_mean_traps > 0) %>%
  print(n = 35)

##### 3.4. Table 1: mean annual number of sampled weeks per region #####
# Weeks are counted as distinct ISO weeks with at least one trap active,
# within the region-specific monitoring period.
week_counts <- db %>%
  dplyr::group_by(year, Region) %>%
  dplyr::summarise(n_weeks = n_distinct(week), .groups = "drop")

week_counts_complete <- region_years %>%
  dplyr::left_join(week_counts, by = c("Region", "year")) %>%
  dplyr::mutate(n_weeks = tidyr::replace_na(n_weeks, 0))

week_counts_complete %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(
    yearly_mean_weeks = mean(n_weeks, na.rm = TRUE),
    sd_weeks          = sd(n_weeks, na.rm = TRUE),
    n_years           = n_distinct(year),
    year_range        = paste(min(year), max(year), sep = "–"),
    .groups = "drop"
  ) %>%
  print(n = 30)

##### 3.5. Table 1: mean annual number of sampled 9x9 km grid cells per region #####
# Grid cells are identified by unique cell_id values assigned during spatial
# aggregation onto the ERA5-Land compatible reference grid (Section 2.1.4).
cell_counts <- db %>%
  dplyr::group_by(year, Region) %>%
  dplyr::summarise(n_cells = n_distinct(cell_id), .groups = "drop")

cell_counts_complete <- region_years %>%
  dplyr::left_join(cell_counts, by = c("Region", "year")) %>%
  dplyr::mutate(n_cells = tidyr::replace_na(n_cells, 0))

cell_counts_complete %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(
    yearly_mean_cells = mean(n_cells, na.rm = TRUE),
    sd_cells          = sd(n_cells, na.rm = TRUE),
    .groups = "drop"
  )

##### 3.6. Table 1: temporal range of monitoring per region #####
db %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  ) %>%
  dplyr::arrange(Region)

##### 3.7. Median Cx. pipiens abundance per region, year, and sex (Data overview) #####
# Sex categories are harmonised from heterogeneous raw labels across institutes.
# Records without sex information are retained as "NA" to avoid data loss.
table_sex <- db %>%
  dplyr::mutate(sex_cat = dplyr::case_when(
    tolower(sex) %in% c("female", "females", "f") ~ "F",
    tolower(sex) %in% c("male",   "males",   "m") ~ "M",
    TRUE                                          ~ "NA" # Gestisce sia NA che altri valori
  )) %>%
  dplyr::group_by(Region, year, sex_cat) %>%
  dplyr::summarise(
    # Calcola mediana e percentili (95% CI empirico)
    med = median(value, na.rm = TRUE),
    lci = quantile(value, probs = 0.025, na.rm = TRUE),
    uci = quantile(value, probs = 0.975, na.rm = TRUE),
    
    # Unisce i valori in un formato tipico da paper: "Mediana [LCI - UCI]"
    # Modifica "%.1f" in "%.2f" se vuoi due decimali invece di uno
    formatted_value = sprintf("%.1f [%.1f - %.1f]", med, lci, uci),
    
    .groups = "drop"
  ) %>%
  # Rimuove le righe dove la mediana è NA per evitare stringhe come "NA [NA - NA]"
  dplyr::filter(!is.na(med)) %>% 
  dplyr::arrange(year, sex_cat) %>%
  # Usa solo la colonna formattata per fare la tabella larga
  tidyr::pivot_wider(
    names_from  = c(year, sex_cat),
    values_from = formatted_value,
    values_fill = "-" # Meglio un trattino di "NA" per le tabelle di testo
  )

print(table_sex, n = Inf, width = Inf)

table_sex_ci <- db %>%
  mutate(sex_cat = case_when(
    tolower(sex) %in% c("female","females","f") ~ "F",
    tolower(sex) %in% c("male","males","m")     ~ "M",
    TRUE                                       ~ NA_character_
  )) %>%
  group_by(Region, year, sex_cat) %>%
  summarise(
    n = sum(!is.na(value)),
    med = median(value, na.rm = TRUE),
    lci = quantile(value, probs = 0.025, na.rm = TRUE),
    uci = quantile(value, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Region, year)

table_sex_ci %>% filter(sex_cat == "F") %>% print(n = Inf, width = Inf)


table_sex_iqr <- db %>%
  mutate(sex_cat = case_when(
    tolower(sex) %in% c("female","females","f") ~ "F",
    tolower(sex) %in% c("male","males","m")     ~ "M",
    TRUE                                       ~ NA_character_
  )) %>%
  group_by(Region, year, sex_cat) %>%
  summarise(
    n   = sum(!is.na(value)),
    med = median(value, na.rm = TRUE),
    q1  = quantile(value, probs = 0.25, na.rm = TRUE),
    q3  = quantile(value, probs = 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    p025 = quantile(value, probs = 0.025, na.rm = TRUE),
    p975 = quantile(value, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Region, year)

table_sex_iqr %>% filter(sex_cat == "F") %>% print(n = Inf, width = Inf)


##### 3.8. Per-phase summary of sampling effort (for §4 Data Overview) #####
# In response to Reviewer 1 request for a clearer summary of sampling effort
# (number of traps deployed, spatial distribution, and how effort changed
# over time), we compute per-phase metrics that describe the harmonised
# dataset across the four regulatory phases of the surveillance system.
# The breakdown quantifies data availability in the harmonised dataset;
# it does not aim to represent the full historical extent of the Italian
# surveillance system, since some contributing institutes could not share
# data for all years. Numbers are cited in the Data Overview section (§4).

phase_summary <- culex_dis1 %>%
  dplyr::mutate(
    phase = dplyr::case_when(
      year == 2008        ~ "2008 (emergency)",
      year %in% 2009:2013 ~ "2009-2013 (standardisation)",
      year %in% 2014:2019 ~ "2014-2019 (consolidation)",
      year %in% 2020:2022 ~ "2020-2022 (current)"
    )
  ) %>%
  dplyr::group_by(phase) %>%
  dplyr::summarise(
    n_years          = dplyr::n_distinct(year),
    n_regions        = dplyr::n_distinct(Region),
    n_trap_locations = dplyr::n_distinct(latitude, longitude),
    n_raw_records    = dplyr::n(),
    .groups          = "drop"
  )

# Grid-cell coverage per phase (from the aggregated dataset db)
phase_cells <- db %>%
  dplyr::mutate(
    phase = dplyr::case_when(
      year == 2008        ~ "2008 (emergency)",
      year %in% 2009:2013 ~ "2009-2013 (standardisation)",
      year %in% 2014:2019 ~ "2014-2019 (consolidation)",
      year %in% 2020:2022 ~ "2020-2022 (current)"
    )
  ) %>%
  dplyr::group_by(phase) %>%
  dplyr::summarise(
    n_cells              = dplyr::n_distinct(cell_id),
    n_sampled_weeks      = dplyr::n_distinct(paste(year, week)),
    n_aggregated_records = dplyr::n(),
    .groups              = "drop"
  )

# Merge the two summaries for a single per-phase overview
phase_overview <- phase_summary %>%
  dplyr::left_join(phase_cells, by = "phase")

print(phase_overview, width = Inf)


#### 4. FIGURE 1 – Spatiotemporal evolution of WNV risk zoning (2008–2025) ####
# Four surveillance periods are displayed as facets:
#   2008         → 15 point locations (20×20 km grid buffers)
#   2009–2012/13 → municipal-level polygons (map_muni_plot)
#   2014–2019    → provincial-level polygons (map_prov_plot)
#   2020–2025    → regional-level polygons  (map_reg_plot)
# All risk labels are harmonised to three groups via assign_risk_group().
# See Section 2.1.2 for the full description of each regulatory period.

livelli_anni <- c("Sampled regions", "2008", "2009", "2010", "2011", "2012-2013", "2014-2019", "2020-2025")


##### 4.1 sampled regions #####
# Shows which Italian regions contributed samples to the harmonised
# dataset (dark grey) vs those without any sampling programme in scope
# (white). For Trentino-Alto Adige only the Autonomous Province of Trento
# contributes data; we therefore paint the whole regional polygon as
# "Not sampled" and overlay the Trento-only province polygon in dark
# grey on top of it.

# Mapping from db English labels to ita_reg Italian shapefile labels.
# Note the exact spellings required by the shapefile:
#   - "Trentino-Alto Adige"    (no "/Südtirol" suffix)
#   - "Friuli Venezia Giulia"  (space, not hyphen)
# Also note that "Autonomous Province of Trento" is deliberately
# excluded here — the whole Trentino-Alto Adige is left as
# "Not sampled" and Trento is added back on top separately.
region_en_to_it <- c(
  "Piedmont"                       = "Piemonte",
  "Liguria"                        = "Liguria",
  "Lombardy"                       = "Lombardia",
  "Veneto"                         = "Veneto",
  "Friuli-Venezia Giulia"          = "Friuli Venezia Giulia",
  "Emilia-Romagna"                 = "Emilia-Romagna",
  "Tuscany"                        = "Toscana",
  "Umbria"                         = "Umbria",
  "Marche"                         = "Marche",
  "Lazio"                          = "Lazio",
  "Campania"                       = "Campania",
  "Apulia"                         = "Puglia",
  "Basilicata"                     = "Basilicata",
  "Calabria"                       = "Calabria",
  "Sardinia"                       = "Sardegna",
  "Sicily"                         = "Sicilia"
)

# Exclude Trento from the region-level check — handled by the overlay below
sampled_regions_en <- setdiff(unique(db$Region), "Autonomous Province of Trento")
sampled_regions_it <- unname(region_en_to_it[sampled_regions_en])

# Sanity check: no NAs allowed in the mapping
stopifnot(
  "Some db$Region labels failed to map to Italian shapefile labels" =
    all(!is.na(sampled_regions_it))
)

# Regional-level layer: sampled vs not sampled, all regions painted
sampled_map <- ita_reg %>%
  dplyr::mutate(
    sampled    = ifelse(DEN_REG %in% sampled_regions_it, "Sampled", "Not sampled"),
    anno_label = "Sampled regions"
  )

# Trento overlay: the Autonomous Province of Trento only, marked "Sampled"
# (drawn on top of the "Not sampled" Trentino-Alto Adige region below)
trento_overlay <- trento %>%
  dplyr::mutate(
    sampled    = "Sampled",
    anno_label = "Sampled regions"
  )


##### 4.2 2008: 15 sentinel sites with 20 km buffer grids #####
sites_2008 <- tibble::tribble(
  ~region,          ~site,                       ~Lat_DMS,  ~Long_DMS,  ~lat,     ~lon,
  "Abruzzo",        "Foce fiume Vomano (TE)",    "42°39'",  "14°02'",   42.6500,  14.0333,
  "Basilicata",     "Lago S. Giuliano (MT)",      "40°38'",  "16°30'",   40.6333,  16.5000,
  "Calabria",       "Foce fiume Neto (KR)",       "39°12'",  "17°08'",   39.2000,  17.1333,
  "Campania",       "Serre Persano (SA)",          "40°33'",  "15°08'",   40.5500,  15.1333,
  "Emilia-Romagna", "Valli di Comacchio (FE)",    "44°37'",  "12°08'",   44.6167,  12.1333,
  "Friuli-V.G.",    "Laguna Grado/Marano (GO)",   "45°44'",  "13°14'",   45.7333,  13.2333,
  "Lazio",          "Lago di Sabaudia (LT)",       "41°28'",  "13°02'",   41.4667,  13.0333,
  "Marche",         "Sentina (AN)",                "43°28'",  "13°38'",   43.4667,  13.6333,
  "Molise",         "Foce Biferno (CB)",           "41°58'",  "15°02'",   41.9667,  15.0333,
  "Puglia",         "Manfredonia (FG)",            "41°38'",  "16°02'",   41.6333,  16.0333,
  "Sardinia",       "Stagno S'Ena Arrubia (OR)",  "39°49'",  "08°34'",   39.8167,   8.5667,
  "Sicily",         "Stagni Vendicari (SR)",       "36°47'",  "15°05'",   36.7833,  15.0833,
  "Tuscany",        "Padule Fucecchio (PT)",       "43°49'",  "10°47'",   43.8167,  10.7833,
  "Umbria",         "Lago Trasimeno (PG)",         "43°11'",  "12°08'",   43.1833,  12.1333,
  "Veneto",         "Valle Averto (VE)",           "45°21'",  "12°12'",   45.3500,  12.2000
)

sites_sf <- sf::st_as_sf(sites_2008, coords = c("lon", "lat"), crs = 4326)

# 20 km buffer around each sentinel site → grid cells intersecting the buffer
sites_buffer     <- sites_sf %>%
  sf::st_transform(32632) %>%
  sf::st_buffer(dist = 20000) %>%
  sf::st_transform(4326)

sites_buffer_utm <- sf::st_transform(sites_buffer, 32632)
grid_utm         <- sf::st_make_grid(sites_buffer_utm, cellsize = 20000, square = TRUE)
grid_sf          <- sf::st_as_sf(grid_utm)

final_grid_utm   <- sf::st_filter(grid_sf, sites_buffer_utm, .predicate = st_intersects)
grid_clipped_utm <- sf::st_intersection(final_grid_utm, ita_reg)
final_grid       <- sf::st_transform(grid_clipped_utm, 4326)

sites_08_plot <- sites_sf    %>% mutate(anno_label = "2008")
grid_08_plot  <- final_grid  %>% mutate(anno_label = "2008")

##### 4.3. Municipality and province name standardisation #####
# Required for spatial joins with the risk area database (2009–2019).
# Upper-case conversion ensures case-insensitive matching with ISTAT strings.
ita_muni1 <- ita_muni %>%
  dplyr::mutate(
    municipality_match = stringr::str_to_upper(name),
    province_match     = stringr::str_to_upper(prov_name),
    region_match       = stringr::str_to_upper(reg_name)
  )

ita_prov1 <- ita_prov %>%
  dplyr::mutate(PROV_UPPER = stringr::str_to_upper(DEN_UTS))

##### 4.4. Risk area database cleaning and standardisation #####
# Corrects typographical errors, harmonises place names across regulatory
# periods, and resolves municipality mergers (post-2016 ISTAT reforms).
# See Section 2.1.3 for the complete harmonisation and QC pipeline.
risk_clean <- risk_area %>%
  dplyr::mutate(
    
    # 1. Standardise region names to ISTAT Italian labels
    region = stringr::str_to_upper(stringr::str_trim(region)),
    region = dplyr::case_when(
      region %in% c("VALLE D'AOSTA/VALLÉE D'AOSTE", "VALLE D'AOSTA") ~ "VALLE D'AOSTA",
      region %in% c("FRIULI-VENEZIA GIULIA", "FRIULI VENEZIA GIULIA") ~ "FRIULI VENEZIA GIULIA",
      region %in% c("EMILIA ROMAGNA", "EMILIA-ROMAGNA")               ~ "EMILIA-ROMAGNA",
      region == "TRENTINO ALTO ADIGE"                                 ~ "TRENTINO-ALTO ADIGE",
      region == "SARDINIA"                                            ~ "SARDEGNA",
      region == "SICILY"                                              ~ "SICILIA",
      region == "TUSCANY"                                             ~ "TOSCANA",
      region == "PIEDMONT"                                            ~ "PIEMONTE",
      region == "LOMBARDY"                                            ~ "LOMBARDIA",
      region == "APULIA"                                              ~ "PUGLIA",
      TRUE ~ region
    ),
    
    # 2. Harmonise risk labels across regulatory periods
    risk = stringr::str_trim(risk),
    risk = dplyr::case_when(
      risk %in% c("External surveilance area",
                  "External surveillance area") ~ "External surveillance area",
      TRUE ~ risk
    ),
    
    # 3. Standardise province names to ISTAT coding
    #    (handles Sardinia old-province codes and common typos)
    province = stringr::str_to_upper(stringr::str_trim(province)),
    province = dplyr::case_when(
      province == "RRERAMO"                            ~ "TERAMO",
      province %in% c("PESARO URBINO",
                      "PESARO-URBINO")                ~ "PESARO E URBINO",
      province %in% c("MONZA", "MONZA BRIANZA")       ~ "MONZA E DELLA BRIANZA",
      province %in% c("FORLI CESENA",
                      "FORLI'-CESENA")                ~ "FORLÌ-CESENA",
      province == "REGGIO EMILIA"                     ~ "REGGIO NELL'EMILIA",
      province == "BOLZANO"                           ~ "BOLZANO/BOZEN",
      province == "AOSTA"                             ~ "VALLE D'AOSTA/VALLÉE D'AOSTE",
      province == "OLBIA-TEMPIO"                      ~ "SASSARI",
      province == "OGLIASTRA"                         ~ "NUORO",
      province %in% c("CARBONIA-IGLESIAS",
                      "MEDIO CAMPIDANO")              ~ "SUD SARDEGNA",
      TRUE ~ province
    ),
    
    # 4. Standardise municipality names
    #    Resolves: typographical errors, OCR artefacts, accented characters,
    #    and post-2016 municipal mergers (ISTAT fusioni di comuni).
    municipality = stringr::str_to_upper(stringr::str_trim(municipality)),
    municipality = dplyr::case_when(
      municipality %in% c("DUINO-AURISINA", "DUINO AURISINA", "DUINO AURISINA-DEVIN NABREZINA") ~ "DUINO AURISINA-DEVIN NABRE\u009eINA",
      municipality %in% c("DONORI'", "DONORÌ")                          ~ "DONORI",
      municipality %in% c("LIGOSULLO",
                          "TREPPO CARNICO")                             ~ "TREPPO LIGOSULLO",
      municipality %in% c("CRESPANO DEL GRAPPA",
                          "PADERNO DEL GRAPPA")                         ~ "PIEVE DEL GRAPPA",
      municipality == "ARZENE"                                          ~ "VALVASONE ARZENE",
      municipality == "MONRUPINO"                                       ~ "MONRUPINO-REPENTABOR",
      municipality == "SAN DORLIGO DELLA VALLE"                        ~ "SAN DORLIGO DELLA VALLE-DOLINA",
      municipality == "SGONICO"                                         ~ "SGONICO-ZGONIK",
      municipality == "SAN NICOLO' GERREI"                             ~ "SAN NICOLÒ GERREI",
      municipality == "SENORBI'"                                        ~ "SENORBÌ",
      municipality %in% c("BELVI", "BELVI'")                           ~ "BELVÌ",
      municipality %in% c("GALTELLI", "GALTELLI'")                     ~ "GALTELLÌ",
      municipality %in% c("LODE", "LODE'")                             ~ "LODÈ",
      municipality %in% c("ONANI", "ONANI'")                           ~ "ONANÌ",
      municipality %in% c("TORPE", "TORPE'")                           ~ "TORPÈ",
      municipality == "TORTOLI'"                                        ~ "TORTOLÌ",
      municipality == "ALA' DEI SARDI"                                 ~ "ALÀ DEI SARDI",
      municipality == "BUDDUSO'"                                        ~ "BUDDUSÒ",
      municipality == "TRINITA' D'AGULTU E VIGNOLA"                    ~ "TRINITÀ D'AGULTU E VIGNOLA",
      municipality == "BIDONI'"                                         ~ "BIDONÌ",
      municipality == "GONNOSNO'"                                       ~ "GONNOSNÒ",
      municipality == "SODDI'"                                          ~ "SODDÌ",
      municipality == "ULA' TIRSO"                                     ~ "ULÀ TIRSO",
      municipality == "NUGHEDU SAN NICOLO'"                            ~ "NUGHEDU SAN NICOLÒ",
      municipality == "ALI'"                                           ~ "ALÌ",
      municipality == "ALI' TERME"                                     ~ "ALÌ TERME",
      municipality == "BASICO'"                                        ~ "BASICÒ",
      municipality == "CONDRO'"                                        ~ "CONDRÒ",
      municipality == "FORZA D'AGRO'"                                  ~ "FORZA D'AGRÒ",
      municipality == "GUALTIERI SICAMINO'"                            ~ "GUALTIERI SICAMINÒ",
      municipality == "MAZZARRA' SANT'ANDREA"                          ~ "MAZZARRÀ SANT'ANDREA",
      municipality == "MERI'"                                          ~ "MERÌ",
      municipality == "RODI' MILICI"                                   ~ "RODÌ MILICI",
      municipality == "PORTOBUFFOLE'"                                  ~ "PORTOBUFFOLÈ",
      municipality == "ANZANO"                                         ~ "ANCARANO",
      municipality == "PETRONA'"                                       ~ "PETRONÀ",
      municipality == "CODOGNE'"                                       ~ "CODOGNÈ",
      municipality == "VALVASONE"                                      ~ "VALVASONE ARZENE",
      municipality == "PROVIDENTI"                                     ~ "PROVVIDENTI",
      municipality == "SANNICANDRO GARGANICO"                          ~ "SAN NICANDRO GARGANICO",
      municipality == "VILLA URBANA"                                   ~ "VILLAURBANA",
      municipality == "ANO SUL TORDINO"                                ~ "TERAMO",
      municipality == "MGARANO MAINARDA"                               ~ "VIGARANO MAINARDA",
      municipality == "OGHIERA"                                        ~ "VOGHIERA",
      municipality == "RZENE"                                          ~ "VALVASONE ARZENE",
      municipality == "ZANO DECIMO"                                    ~ "AZZANO DECIMO",
      municipality == "!SORELLA"                                       ~ "ISORELLA",
      municipality == "13ELPASSO"                                      ~ "BELPASSO",
      municipality == "ATANIA"                                         ~ "CATANIA",
      municipality == "AMACCA"                                         ~ "RAMACCA",
      municipality == "F/LZZINI"                                       ~ "VIZZINI",
      municipality == "11\\UGUSTA"                                     ~ "AUGUSTA",
      municipality == "RANCOFONTE"                                     ~ "FRANCOFONTE",
      municipality == "LTOPASCIO"                                      ~ "ALTOPASCIO",
      municipality == "L:>ORCARI"                                      ~ "PORCARI",
      municipality == "L:>ESCIA"                                       ~ "PESCIA",
      municipality == "PUARRATA"                                       ~ "QUARRATA",
      municipality == "NZOLA DELL'EMILIA"                              ~ "ANZOLA DELL'EMILIA",
      municipality == "RGELATO"                                        ~ "ARGELATO",
      municipality == "MONTEGROTTOTERME"                               ~ "MONTEGROTTO TERME",
      municipality == "SAN PIETRO VIMLNARIO"                           ~ "SAN PIETRO VIMINARIO",
      municipality == "BOSCHISANT'ANNA"                                ~ "BOSCHI SANT'ANNA",
      municipality == "CASTELL'ARQ UATO"                               ~ "CASTELL'ARQUATO",
      municipality == "LI\\LELLO DEL FRIULI"                           ~ "LIGNANO SABBIADORO",
      municipality == "PROWIDENTI"                                     ~ "PROVVIDENTI",
      municipality == "PETROSI NO"                                     ~ "PETROSINO",
      municipality == "V\\TRI"                                         ~ "ATRI",
      municipality == "ASTELLALTO"                                     ~ "CASTELLALTO",
      municipality == "ASTILENTI"                                      ~ "CASTILENTI",
      municipality == "ELLINO ATTANASIO"                               ~ "CELLINO ATTANASIO",
      municipality == "K31ULIANOVA"                                    ~ "GIULIANOVA",
      municipality == "ILVI"                                           ~ "SILVI",
      municipality == "ÌRORTORETO"                                     ~ "TORTORETO",
      municipality == "SELLANTE"                                       ~ "BELLANTE",
      municipality == "ARFIZZI"                                        ~ "CARFIZZI",
      municipality == "ASABONA"                                        ~ "CASABONA",
      municipality == "ROTONE"                                         ~ "CROTONE",
      municipality == "UTRO"                                           ~ "CUTRO",
      municipality == "13UCCHERI"                                      ~ "BUCCHERI",
      municipality == "ARLENTINI"                                      ~ "CARLENTINI",
      municipality == "F./LLLA BASILICA"                               ~ "VILLA BASILICA",
      municipality == "GLIANA"                                         ~ "AGLIANA",
      municipality == "13UGGIANO"                                      ~ "BUGGIANO",
      municipality == "-AMPORECCHIO"                                   ~ "LAMPORECCHIO",
      municipality == "L:>IEVE A NIEVOLE"                              ~ "PIEVE A NIEVOLE",
      municipality == "PERRAVALLE PISTOIESE"                           ~ "SERRAVALLE PISTOIESE",
      municipality == "LJZZANO"                                        ~ "UZZANO",
      municipality == "APRAIA E LIMITE"                                ~ "CAPRAIA E LIMITE",
      municipality == "BORGO RATTO ALESSANDRINO"                       ~ "BORGORATTO ALESSANDRINO",
      municipality == "LES" & province == "ORISTANO"                  ~ "ALES",
      municipality == "RBOREA"                                         ~ "ARBOREA",
      municipality == "ERRALBA"                                        ~ "TERRALBA",
      municipality == "MLLAURBANA"                                     ~ "VILLAURBANA",
      municipality == "!ZEDDIANI"                                      ~ "ZEDDIANI",
      municipality %in% c("DOBERDÒ DEL LAGO",
                          "DOBERDO' DEL LAGO")                         ~ "DOBERDÒ DEL LAGO-DOBERDOB",
      municipality == "SAN FLORIANO DEL COLLIO"                        ~ "SAN FLORIANO DEL COLLIO-\u008aTEVERJAN",
      municipality == "SAVOGNA D'ISONZO"                               ~ "SAVOGNA D'ISONZO-SOVODNJE OB SO?I",
      # Post-2016 municipal mergers (ISTAT fusioni di comuni)
      municipality %in% c("BAZZANO", "CRESPELLANO",
                          "CASTELLO DI SERRAVALLE",
                          "MONTEVEGLIO", "SAVIGNO")                    ~ "VALSAMOGGIA",
      municipality %in% c("BERRA", "RO")                              ~ "RIVA DEL PO",
      municipality %in% c("MIRABELLO", "SANT'AGOSTINO")               ~ "TERRE DEL RENO",
      municipality %in% c("MIGLIARO", "MIGLIARINO",
                          "MASSA FISCAGLIA")                           ~ "FISCAGLIA",
      municipality %in% c("FORMIGNANA", "TRESIGALLO")                 ~ "TRESIGNANA",
      municipality %in% c("SORBOLO", "MEZZANI")                       ~ "SORBOLO MEZZANI",
      municipality %in% c("SISSA", "TRECASALI")                       ~ "SISSA TRECASALI",
      municipality %in% c("POLESINE PARMENSE", "ZIBELLO")             ~ "POLESINE ZIBELLO",
      municipality %in% c("BIGARELLO",
                          "SAN GIORGIO DI MANTOVA")                   ~ "SAN GIORGIO BIGARELLO",
      municipality %in% c("FELONICA", "SERMIDE")                      ~ "SERMIDE E FELONICA",
      municipality %in% c("BORGOFRANCO SUL PO",
                          "CARBONARA DI PO")                           ~ "BORGOCARBONARA",
      municipality %in% c("PIEVE DI CORIANO",
                          "REVERE", "VILLA POMA")                      ~ "BORGO MANTOVANO",
      municipality %in% c("BORGOFORTE", "VIRGILIO")                   ~ "BORGO VIRGILIO",
      municipality %in% c("DRIZZONA", "PIADENA")                      ~ "PIADENA DRIZZONA",
      municipality %in% c("GRANCONA",
                          "SAN GERMANO DEI BERICI")                   ~ "VAL LIONA",
      municipality %in% c("BARBARANO VICENTINO",
                          "MOSSANO")                                   ~ "BARBARANO MOSSANO",
      municipality %in% c("MEGLIADINO SAN FIDENZIO",
                          "SALETTO",
                          "SANTA MARGHERITA D'ADIGE")                  ~ "BORGO VENETO",
      municipality %in% c("RIVIGNANO", "TEOR")                        ~ "RIVIGNANO TEOR",
      municipality %in% c("FIUMICELLO",
                          "VILLA VICENTINA")                           ~ "FIUMICELLO VILLA VICENTINA",
      municipality %in% c("CAMPOLONGO AL TORRE",
                          "TAPOGLIANO")                                ~ "CAMPOLONGO TAPOGLIANO",
      municipality == "CASTELLANIA"                                    ~ "CASTELLANIA COPPI",
      municipality == "GAVAZZANA"                                      ~ "CASSANO SPINOLA",
      municipality == "PIOVERA"                                        ~ "ALLUVIONI PIOVERA",
      municipality == "PITEGLIO"                                       ~ "SAN MARCELLO PITEGLIO",
      municipality == "PERGINE VALDARNO"                               ~ "LATERINA PERGINE VALDARNO",
      municipality == "CITTA' DELLA PIEVE"                            ~ "CITTÀ DELLA PIEVE",
      municipality == "CAPACCIO"                                       ~ "CAPACCIO PAESTUM",
      municipality == "LONATO"                                         ~ "LONATO DEL GARDA",
      municipality == "CA' D'ANDREA"                                  ~ "TORRE DE' PICENARDI",
      municipality == "COSTERMANO"                                     ~ "COSTERMANO SUL GARDA",
      municipality == "NEGRAR"                                         ~ "NEGRAR DI VALPOLICELLA",
      municipality == "FOSSO'"                                         ~ "FOSSÒ",
      municipality == "ARQUA' POLESINE"                               ~ "ARQUÀ POLESINE",
      municipality == "ARQUA' PETRARCA"                               ~ "ARQUÀ PETRARCA",
      municipality == "SAN DONA' DI PIAVE"                            ~ "SAN DONÀ DI PIAVE",
      municipality == "SANTO STINO DI LIVENZA"                        ~ "SAN STINO DI LIVENZA",
      municipality == "PONTE SAN NICOLO'"                             ~ "PONTE SAN NICOLÒ",
      municipality %in% c("MASERA' DI PADOVA",
                          "MASERA'DI PADOVA")                          ~ "MASERÀ DI PADOVA",
      municipality == "SCORZE'"                                        ~ "SCORZÈ",
      municipality == "MANSUE'"                                        ~ "MANSUÈ",
      municipality == "IESOLO"                                         ~ "JESOLO",
      municipality == "ERBE'"                                          ~ "ERBÈ",
      municipality == "PALU'"                                          ~ "PALÙ",
      municipality == "RONCA'"                                         ~ "RONCÀ",
      municipality == "ROVEREDO DI GUA'"                              ~ "ROVEREDO DI GUÀ",
      municipality == "SORGA'"                                         ~ "SORGÀ",
      municipality == "PATERNO'"                                       ~ "PATERNÒ",
      municipality %in% c("CIRO'", "IRO'")                            ~ "CIRÒ",
      municipality %in% c("CIRO' MARINA", "IRO' MARINA")              ~ "CIRÒ MARINA",
      municipality == "SAN NICOLO' D'ARCIDANO"                        ~ "SAN NICOLÒ D'ARCIDANO",
      municipality == "CITTA' DI CASTELLO"                            ~ "CITTÀ DI CASTELLO",
      municipality == "CITTA' SANT'ANGELO"                            ~ "CITTÀ SANT'ANGELO",
      municipality == "VO"                                             ~ "VO'",
      municipality == "COLOGNOLA AL COLLI"                             ~ "COLOGNOLA AI COLLI",
      municipality == "SAN GIOVANNI ILARIO NE"                        ~ "SAN GIOVANNI ILARIONE",
      municipality == "TORRIDIQUARTESOLO"                              ~ "TORRI DI QUARTESOLO",
      municipality == "SERRA" & province == "FERRARA"                 ~ "FERRARA",
      TRUE ~ municipality
    )
  ) %>%
  dplyr::filter(!municipality %in% c("COMUNE", "MUNICIPALITY", "MUNICIPALITY_NAME"))

# Smart FVG fix: attempt prefix-based match for Friuli-Venezia Giulia municipalities
# that remain unmatched after the manual corrections above
fvg_munis <- ita_muni1 %>%
  filter(region_match == "FRIULI VENEZIA GIULIA") %>%
  pull(municipality_match)

risk_clean <- risk_clean %>%
  rowwise() %>%
  mutate(
    municipality = if_else(
      region == "FRIULI VENEZIA GIULIA" & !(municipality %in% fvg_munis),
      coalesce(fvg_munis[startsWith(fvg_munis, municipality)][1], municipality),
      municipality
    )
  ) %>%
  ungroup()

# Synchronise province names via municipality lookup
# (resolves mismatches between old and new ISTAT province coding)
muni_lookup <- ita_muni1 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(region_match, municipality_match, province_match) %>%
  dplyr::distinct()

risk_clean <- risk_clean %>%
  dplyr::left_join(muni_lookup,
                   by = c("region" = "region_match",
                          "municipality" = "municipality_match")) %>%
  dplyr::mutate(province = dplyr::coalesce(province_match, province)) %>%
  dplyr::select(-province_match)

##### 4.5. 2009–2012/13: municipal-level risk zoning #####
risk_muni_data <- risk_clean %>%
  filter(year %in% c(2009, 2010, 2011, 2012)) %>%
  mutate(
    anno_label = dplyr::case_when(year == 2012 ~ "2012-2013",
                                  TRUE          ~ as.character(year)),
    risk_group = assign_risk_group(risk)
  )

# Diagnostic: flag municipalities with no match in ISTAT shapefile
orphans <- risk_muni_data %>%
  dplyr::anti_join(ita_muni1,
                   by = c("municipality" = "municipality_match",
                          "province"     = "province_match")) %>%
  dplyr::distinct(region, municipality, province, year)

if (nrow(orphans) > 0) {
  message("WARNING: Unmatched municipalities in the 2009–2012 risk data:")
  print(orphans)
}

map_muni_plot <- ita_muni1 %>%
  inner_join(risk_muni_data,
             by = c("municipality_match" = "municipality",
                    "province_match"     = "province"))

##### 4.6. 2014–2019: provincial-level risk zoning #####
risk_prov_data <- risk_clean %>%
  filter(year == 2014) %>%
  mutate(anno_label = "2014-2019",
         risk_group = assign_risk_group(risk))

orphans_prov <- risk_prov_data %>%
  dplyr::anti_join(ita_prov1, by = c("province" = "PROV_UPPER")) %>%
  dplyr::distinct(region, province)

if (nrow(orphans_prov) > 0) {
  message("WARNING: Unmatched provinces in the 2014 risk data:")
  print(orphans_prov)
}

map_prov_plot <- ita_prov1 %>%
  inner_join(risk_prov_data, by = c("PROV_UPPER" = "province"))

##### 4.7. 2020–2025: regional-level risk zoning (PNA 2020–2025) #####
risk_reg_data <- risk_clean %>%
  filter(year == 2020) %>%
  mutate(anno_label = "2020-2025",
         risk_group = assign_risk_group(risk))

ita_reg1 <- ita_reg %>%
  dplyr::mutate(REG_UPPER = stringr::str_to_upper(DEN_REG))

map_reg_plot <- ita_reg1 %>%
  inner_join(risk_reg_data, by = c("REG_UPPER" = "region"))

##### 4.8. Composite map: all periods as facets (Figure 1) #####
# Province polygons are linked to the responsible institute based on
# COD_PROV codes (ISTAT 2022 coding scheme), enabling colour-fill by institute
# to delineate territorial competence as described in Section 2.1.1.

ita_prov <- ita_prov %>%
  dplyr::mutate(Region_join = case_when(
    COD_PROV == 22                                                        ~ "Autonomous Province of Trento",
    COD_PROV %in% c(1, 2, 3, 4, 5, 6, 96, 103)                            ~ "Piedmont",
    COD_PROV %in% c(8, 9, 10, 11)                                         ~ "Liguria",
    COD_PROV %in% c(12:20, 97, 98, 108)                                   ~ "Lombardy",
    COD_PROV %in% c(23:29)                                                ~ "Veneto",
    COD_PROV %in% c(30, 31, 32, 93)                                       ~ "Friuli-Venezia Giulia",
    COD_PROV %in% c(33:40, 99)                                            ~ "Emilia-Romagna",
    COD_PROV %in% c(45:53, 100)                                           ~ "Tuscany",
    COD_PROV %in% c(54, 55)                                               ~ "Umbria",
    COD_PROV %in% c(41:44, 109)                                           ~ "Marche",
    COD_PROV %in% c(56:60)                                                ~ "Lazio",
    COD_PROV %in% c(66:69)                                                ~ "Abruzzo",
    COD_PROV %in% c(70, 94)                                               ~ "Molise",
    COD_PROV %in% c(61:65)                                                ~ "Campania",
    COD_PROV %in% c(71:75, 110)                                           ~ "Apulia",
    COD_PROV %in% c(76, 77)                                               ~ "Basilicata",
    COD_PROV %in% c(78:80, 101, 102)                                      ~ "Calabria",
    COD_PROV %in% c(81:89)                                                ~ "Sicily",
    COD_PROV %in% c(90:92, 95, 104:107, 111)                              ~ "Sardinia",
    TRUE ~ NA_character_
  ))

region_institute <- db %>%
  dplyr::group_by(Region, Institute, year) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(Region, year) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(
    gap   = c(0, diff(year) != 1),
    block = cumsum(gap)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(Region, Institute)

ita_sf_plot <- ita_prov %>%
  dplyr::left_join(region_institute, by = c("Region_join" = "Region"))

figure_1 <- ggplot2::ggplot() +
  ggspatial::geom_sf(data = world_map_utm, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = ita_reg, fill = "white", color = NA, size = 0.2) +
  # --- Sampled Regions panel (first facet) with its own fill scale ---
  geom_sf(data = sampled_map,    aes(fill = sampled), color = NA) +
  geom_sf(data = trento_overlay, aes(fill = sampled), color = NA) +
  scale_fill_manual(
    values = c("Sampled" = "gray30", "Not sampled" = "white"),
    guide  = "none"
  ) +
  ggnewscale::new_scale_fill() +
  # --- Risk classification panels (2009-2025 facets) ---
  geom_sf(data = map_muni_plot, aes(fill = risk_group), color = NA) +
  geom_sf(data = map_prov_plot, aes(fill = risk_group), color = NA) +
  geom_sf(data = map_reg_plot,  aes(fill = risk_group), color = NA) +
  # --- 2008 sentinel sites and grid buffers ---
  geom_sf(data = grid_08_plot,  fill = "darkred", color = "darkred", alpha = 0.4, size = 0.3) +
  geom_sf(data = sites_08_plot, color = "black", size = 1.5) +
  # --- Administrative boundaries (drawn on top) ---
  geom_sf(data = ita_reg, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = trento,  fill = NA, color = "black", size = 0.2) +
  ggspatial::coord_sf(
    xlim = sf::st_bbox(ita_sf_plot)[c(1, 3)],
    ylim = sf::st_bbox(ita_sf_plot)[c(2, 4)]
  ) +
  scale_fill_manual(
    values = group_colors,
    breaks = c("High Risk", "Medium Risk", "Low Risk"),
    name   = "Risk classification"
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl",
                         style  = north_arrow_fancy_orienteering(),
                         height = unit(0.8, "cm"),
                         width  = unit(0.8, "cm")) +
  facet_wrap(~ factor(anno_label, levels = livelli_anni), nrow = 2) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    text             = element_text(size = 18),
    strip.background = element_rect(fill = "gray95"),
    strip.text       = element_text(face = "bold", size = 11),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed")
  )

figure_1

ggplot2::ggsave("../figures/figure_1.tiff",
                plot = figure_1,
                width = 15, height = 15, dpi = 600)


#### 5.FIGURE 2 – Data processing flow chart ####
# Visual summary of the harmonisation and
# spatial aggregation pipeline from the raw regional datasets to the final
# published dataset. The chart mirrors the narrative in Methods
# (Data harmonisation and quality control) and Data analysis,
# clarifying in particular the aggregation logic where n_records_aggregated
# distinguishes multi-trap aggregation (median) from single-trap pass-through.

##### 5.0.1. Extract QC numbers from every pipeline stage (dynamic) #####
pct <- function(part, whole) round(100 * part / whole, 1)

qc_summary_full <- readr::read_csv("../main_db/qc_summary_report.csv", show_col_types = FALSE) %>%
  dplyr::filter(region == "TOTAL")

n_raw_reconstructed      <- qc_summary_full$n_raw_reconstructed
n_excluded_stage1        <- qc_summary_full$n_excluded_stage1
n_retained_regional      <- qc_summary_full$n_retained_regional
n_excluded_stage2        <- qc_summary_full$n_excluded_stage2
n_retained_final         <- qc_summary_full$n_retained_final
n_duplicates_stage1      <- qc_summary_full$n_duplicates_stage1
n_duplicates_stage2      <- qc_summary_full$n_duplicates_stage2
n_duplicates_removed     <- qc_summary_full$n_duplicates_removed
n_georeferenced_centroid <- qc_summary_full$n_georeferenced_centroid
n_corrected              <- qc_summary_full$n_corrected

qc_log_stage1 <- readr::read_csv("../main_db/qc_exclusions_log.csv", show_col_types = FALSE)
stage1_by_reason <- qc_log_stage1 %>% dplyr::count(check, sort = TRUE)
print(stage1_by_reason, n = Inf)

n_invalid_s1 <- sum(stage1_by_reason$n[stage1_by_reason$check %in%
                                         c("invalid_value", "invalid_date")])
n_structural_s1 <- sum(stage1_by_reason$n[stage1_by_reason$check %in%
                                            c("no_sampling_event", "reference_fragment", "empty_or_fragment_row")])
n_uninformative_s1 <- sum(stage1_by_reason$n[stage1_by_reason$check %in%
                                               c("unsexed_zero_count", "unknown_species_zero_count")])
n_duplicate_s1_reason <- sum(stage1_by_reason$n[stage1_by_reason$check %in%
                                                  c("duplicate_export_artifact", "exact_duplicate")])
n_offtarget_species_s1 <- sum(stage1_by_reason$n[stage1_by_reason$check == "off_target_species"])

n_categorised_s1 <- n_invalid_s1 + n_structural_s1 + n_uninformative_s1 +
  n_duplicate_s1_reason + n_offtarget_species_s1

stopifnot(
  "Stage 1 exclusion categories do not sum to n_excluded_stage1 -- a 'check' value in qc_exclusions_log.csv is not covered above; update the category lists" =
    n_categorised_s1 == n_excluded_stage1
)

qc_log_stage2 <- readr::read_csv("../main_db/qc_exclusions_log_merge_stage.csv", show_col_types = FALSE)

n_national_validation_checked <- n_retained_regional
national_validation_checks    <- c("invalid_date_postmerge", "invalid_value_postmerge",
                                   "invalid_coordinate_postmerge", "coordinate_out_of_italy_bbox")
n_national_validation_failed  <- qc_log_stage2 %>%
  dplyr::filter(check %in% national_validation_checks) %>%
  nrow()

n_dup_candidate_groups <- qc_log_stage2 %>%
  dplyr::filter(check == "exact_duplicate_postmerge") %>%
  nrow()

db_check_note <- readr::read_csv("../main_db/db_samplings_clean.csv", show_col_types = FALSE,
                                 col_select = "note")
n_notrap_ambiguous_kept <- sum(grepl("no stable trap identifier", db_check_note$note), na.rm = TRUE)

qc_muniprov <- readr::read_csv("../main_db/qc_municipality_province_mismatches.csv", show_col_types = FALSE)

db_check_centroid <- readr::read_csv("../main_db/db_samplings_clean.csv", show_col_types = FALSE,
                                     col_select = "municipality_centroid")
n_muniprov_checked  <- n_retained_final - sum(db_check_centroid$municipality_centroid == "yes", na.rm = TRUE)
n_muni_mismatch     <- qc_muniprov %>% dplyr::filter(!coord_muni_match | is.na(matched_muni)) %>% nrow()
n_prov_mismatch     <- qc_muniprov %>% dplyr::filter(!coord_prov_match | (is.na(matched_prov) & !is.na(declared_prov))) %>% nrow()

qc_outliers <- readr::read_csv("../main_db/qc_spatial_outliers.csv", show_col_types = FALSE)
n_outliers_flagged         <- nrow(qc_outliers)
n_outliers_confirmed_error <- sum(qc_outliers$also_fails_polygon_check, na.rm = TRUE)

manual_review <- readr::read_csv("../main_db/manual_review_queue.csv", show_col_types = FALSE)
n_manual_review_total <- nrow(manual_review)

db_notes <- readr::read_csv("../main_db/db_samplings_clean.csv", show_col_types = FALSE,
                            col_select = "note")
n_manual_review_resolved <- sum(grepl("adjacent municipality|coordinate transcription error|point-in-polygon check", db_notes$note), na.rm = TRUE)

qc_summary_agg <- readr::read_csv("../main_db/qc_summary_report_aggregation.csv", show_col_types = FALSE)
agg_val <- function(m) qc_summary_agg$value[qc_summary_agg$metric == m]

n_agg_input          <- agg_val("n_input_records_from_stage2")
n_offtarget_species   <- agg_val("n_excluded_off_target_species")
n_out_of_period       <- agg_val("n_excluded_out_of_study_period")
n_entering_agg        <- agg_val("n_records_entering_aggregation")
n_final_aggregated    <- agg_val("n_final_aggregated_rows")
mean_records_per_row  <- agg_val("mean_raw_records_per_aggregated_row")

n_regions      <- dplyr::n_distinct(culex_dis1$Region)
n_years        <- paste0(min(culex_dis1$year, na.rm = TRUE), "\u2013",
                         max(culex_dis1$year, na.rm = TRUE))


##### 5.0.2. Build the flow chart (concise, publication-ready labels) #####
flowchart_dot <- sprintf('
digraph data_pipeline {

  graph [layout   = dot,
         rankdir  = TB,
         nodesep  = 0.45,
         ranksep  = 0.6,
         bgcolor  = "white",
         fontname = "Helvetica"]

  node  [style     = "filled,rounded",
         fontname  = "Helvetica",
         fontsize  = 20,
         margin    = "0.25,0.16",
         penwidth  = 1.4]

  edge  [fontname  = "Helvetica",
         fontsize  = 16,
         fontcolor = "#4d4d4d",
         arrowsize = 0.9,
         penwidth  = 1.3,
         color     = "#4d4d4d"]

  raw   [shape = box,
         label = "Raw regional datasets\\n11 sources \u00B7 %d regions \u00B7 %s\\nn = %s trap-night records",
         fillcolor = "#f2f2f2", color = "#8c8c8c", width = 5.8]

  s1_checks [shape = box,
         label = "Regional harmonisation & quality control\\nDate, coordinate, value and taxonomy validation\\nWithin-region duplicate identification",
         fillcolor = "#deebf7", color = "#3182bd", width = 6.4, penwidth = 1.6]

  s1_excl [shape = box, style = "filled",
         label = "Excluded: %s%% (n = %s)\\nInvalid value/date (n = %s) \u00B7 No sampling event (n = %s)\\nNon-informative zero count (n = %s)\\nDuplicates (n = %s) \u00B7 Off-target species (n = %s)",
         fillcolor = "#fee6ce", color = "#e6550d", width = 6.4]

  s1_out [shape = box,
         label = "Retained: n = %s",
         fillcolor = "#e5e5e5", color = "#4d4d4d", width = 4.2]

  s2_val [shape = box,
         label = "National re-validation of dates, coordinates\\nand values \u2013 %s%% failed (n = %s)",
         fillcolor = "#fee6ce", color = "#e6550d", width = 6.2]

  s2_dup [shape = box,
         label = "National duplicate detection (exact match, all fields)\\nCandidate groups: n = %s\\nCollapsed, stable trap ID (n = %s) \u00B7 Kept with note, no trap ID (n = %s)",
         fillcolor = "#fee6ce", color = "#e6550d", width = 6.6]

  s2_muni [shape = box,
         label = "Municipality/Province verification\\n(point-in-polygon vs. ISTAT boundaries)\\nMunicipality mismatch: %s%% (n = %s) \u00B7 Province mismatch: %s%% (n = %s)",
         fillcolor = "#fee6ce", color = "#e6550d", width = 6.6]

  s2_out_check [shape = box,
         label = "Spatial outlier detection\\n(trap positional consistency + robust MAD distance)\\nFlagged: n = %s \u00B7 Confirmed error: n = %s",
         fillcolor = "#fee6ce", color = "#e6550d", width = 6.2]

  s2_manual [shape = box, style = "filled",
         label = "Manual inspection: %s%% queued (n = %s)\\nResolved with documented correction/note: n = %s",
         fillcolor = "#fff2cc", color = "#bf9000", width = 6.2]

  s2_out [shape = box,
         label = "Harmonised national dataset: n = %s\\nGeoreferenced via centroid: %s%% (n = %s)\\nCorrected records: %s%% (n = %s)",
         fillcolor = "#e5e5e5", color = "#4d4d4d", width = 5.8]

  s3_filter [shape = box,
         label = "Species & study-period restriction\\nOff-target species (n = %s) \u00B7 Outside 2008\u20132022 (n = %s)\\nEntering aggregation: n = %s",
         fillcolor = "#e5f5e0", color = "#31a354", width = 5.8]

  logic [shape = diamond,
         label = "Multiple trap\\nlocations\\nin stratum?",
         fillcolor = "#fff2cc", color = "#bf9000", width = 2.4, height = 1.9]

  agg_m [shape = box, style = "filled",
         label = "Median\\ncount + IQR",
         fillcolor = "#fff2cc", color = "#bf9000", width = 2.8]

  agg_p [shape = box, style = "filled",
         label = "Original count\\n(IQR = 0)",
         fillcolor = "#fff2cc", color = "#bf9000", width = 2.8]

  out   [shape = box,
         label = "Published harmonised dataset\\n%s%% retained (n = %s aggregated records)\\nMean n = %s raw records per row \u2013 Zenodo",
         fillcolor = "#c6dbef", color = "#08519c",
         penwidth = 1.6, width = 5.8]

  raw       -> s1_checks
  s1_checks -> s1_excl
  s1_checks -> s1_out
  s1_out    -> s2_val
  s2_val    -> s2_dup
  s2_dup    -> s2_muni
  s2_muni   -> s2_out_check
  s2_out_check -> s2_manual
  s2_manual -> s2_out
  s2_out    -> s3_filter
  s3_filter -> logic
  logic -> agg_m [label = "Yes (n > 1)"]
  logic -> agg_p [label = "No (n = 1)"]
  agg_m -> out
  agg_p -> out

  { rank = same; agg_m; agg_p }
  { rank = same; s1_checks; s1_excl }
}
',
n_regions, n_years, format(n_raw_reconstructed, big.mark = ","),
sprintf("%.1f", pct(n_excluded_stage1, n_raw_reconstructed)), format(n_excluded_stage1, big.mark = ","),
format(n_invalid_s1, big.mark=","), format(n_structural_s1, big.mark=","),
format(n_uninformative_s1, big.mark=","), format(n_duplicate_s1_reason, big.mark=","),
format(n_offtarget_species_s1, big.mark=","),
format(n_retained_regional, big.mark = ","),
sprintf("%.1f", pct(n_national_validation_failed, n_national_validation_checked)), n_national_validation_failed,
format(n_dup_candidate_groups, big.mark=","), format(n_duplicates_stage2, big.mark=","),
format(n_notrap_ambiguous_kept, big.mark=","),
sprintf("%.1f", pct(n_muni_mismatch, n_muniprov_checked)), format(n_muni_mismatch, big.mark=","),
sprintf("%.1f", pct(n_prov_mismatch, n_muniprov_checked)), format(n_prov_mismatch, big.mark=","),
format(n_outliers_flagged, big.mark=","), format(n_outliers_confirmed_error, big.mark=","),
sprintf("%.1f", pct(n_manual_review_total, n_retained_regional)), format(n_manual_review_total, big.mark=","),
format(n_manual_review_resolved, big.mark=","),
format(n_retained_final, big.mark = ","),
sprintf("%.1f", pct(n_georeferenced_centroid, n_retained_final)), format(n_georeferenced_centroid, big.mark=","),
sprintf("%.1f", pct(n_corrected, n_retained_final)), format(n_corrected, big.mark=","),
format(n_offtarget_species, big.mark=","), format(n_out_of_period, big.mark=","),
format(n_entering_agg, big.mark=","),
sprintf("%.1f", pct(n_final_aggregated, n_agg_input)), format(n_final_aggregated, big.mark = ","),
mean_records_per_row
)

figure_2_flowchart <- DiagrammeR::grViz(flowchart_dot)
figure_2_flowchart


##### 5.3. Export to publication-quality formats #####
svg_str <- DiagrammeRsvg::export_svg(figure_2_flowchart)

png_tmp <- tempfile(fileext = ".png")
rsvg::rsvg_png(charToRaw(svg_str),
               file  = png_tmp,
               width = 4250)   # ~180 mm at 600 dpi

png_matrix <- png::readPNG(png_tmp)
tiff::writeTIFF(png_matrix,
                where       = "../figures/figure_2.tiff",
                compression = "LZW")

file.remove(png_tmp)


#### 6. FIGURE 3 ####
##### 6.1. FIGURE 3A – Temporal range of sampling by region and institute #####
# Line segments represent continuous monitoring periods within each region.
# Gaps (years with no data) are detected via consecutive year differences and
# used to break segments, preventing spurious connections across discontinuities.
# Regions are ordered from north to south following region_north_south.

db_fig3 <- db %>%
  dplyr::mutate(
    Institute = dplyr::recode(
      Institute,
      "Istituto per le Piante da Legno e l'Ambiente"                                = "IPLA and IZSPLV",
      "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta" = "IPLA and IZSPLV"
    )
  )

db_lines <- db_fig3 %>%
  dplyr::group_by(Region, Institute, year) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(Region, year) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(
    gap   = c(0, diff(year) != 1),
    block = cumsum(gap)
  ) %>%
  dplyr::ungroup()

figure_3a <- ggplot2::ggplot(db_lines,
                             aes(x = year, y = Region,
                                 group = interaction(Region, block),
                                 color = Institute)) +
  geom_line(linewidth = 3, na.rm = TRUE) +
  geom_point(size = 4, na.rm = TRUE) +
  scale_y_discrete(limits = rev(region_north_south)) +
  scale_x_continuous(breaks = seq(2008, 2025, 1)) +
  scale_color_manual(
    values = manual_colors_colorblind, 
    labels = institute_labels,
    guide = "none"
  ) +
  labs(x = "Year", y = "Region", color = "Institute") +
  theme_bw() +
  theme(
    legend.position = "none",
    text            = element_text(size = 22),
    axis.text.x     = element_text(size = 22, angle = 45, hjust = 1, vjust = 1.1)
  )

figure_3a

# ggplot2::ggsave("../figures/temporal_range_data_paper.tiff",
#                 plot = figure_3a,
#                 width = 14, height = 8, dpi = 600, compression = "lzw")

##### 6.2. FIGURE 3B – Spatial distribution of sampling sites #####
# Sampling locations: each point is the centroid of a sampled 9×9 km grid cell
# (WGS84 coordinates from the aggregated dataset, reprojected to match ita_prov CRS)
grid_sampled_centroids <- db_fig3 %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_transform(st_crs(ita_prov))

ita_sf_plot_fig3 <- ita_sf_plot %>%
  dplyr::mutate(
    Institute = dplyr::recode(
      Institute,
      "Istituto per le Piante da Legno e l'Ambiente"                                = "IPLA and IZSPLV",
      "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta" = "IPLA and IZSPLV"
    ),
    Institute = dplyr::case_when(
      COD_PROV %in% c(7, 21) ~ "No sampling program", 
      is.na(Institute)       ~ "NA",                  
      TRUE                   ~ Institute
    )
  )

figure_3b <- ggplot2::ggplot() +
  ggspatial::geom_sf(data = world_map_utm, fill = "gray90", color = "black", size = 0.1) +
  ggspatial::geom_sf(data = ita_reg, fill = "white", color = NA) +
  ggspatial::geom_sf(data = ita_sf_plot_fig3, aes(fill = Institute), color = NA) +
  ggspatial::geom_sf(data = ita_reg, fill = NA, color = "black", size = 0.2) +
  ggspatial::geom_sf(data = trento, fill = NA, color = "black", size = 0.2) +
  ggspatial::geom_sf(data = grid_sampled_centroids, color = "black", alpha = 0.5, size = 0.8) +
  ggspatial::coord_sf(
    xlim = c(sf::st_bbox(ita_sf_plot_fig3)[1],
             sf::st_bbox(ita_sf_plot_fig3)[3] * 0.997), 
    ylim = sf::st_bbox(ita_sf_plot_fig3)[c(2, 4)],
    clip = "on"
  ) +
  scale_fill_manual(
    values = c(manual_colors_colorblind,
               "No sampling program" = "#999999", 
               "NA" = "#FFFFFF"),
    labels = c(institute_labels,
               "No sampling program" = "No sampling program", 
               "NA" = "NA")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  labs(x = "Longitude", y = "Latitude", fill = "Institute") +
  theme_bw() +
  theme(
    text = element_text(size = 22),
    plot.margin = margin(t = 5, r = 5, b = 20, l = 5),
    plot.tag.position = c(0.02, 0.98)
  )

figure_3b

# ggplot2::ggsave("../figures/geographical_range_9km_grid_data_paper.tiff",
#                 plot = figure_3b,
#                 width = 12, height = 12, dpi = 600, compression = "lzw")


##### 6.3. COMPOSITE #####
# figure_3 <- (figure_3a + figure_3b + patchwork::guide_area()) +
#   patchwork::plot_layout(
#     design = "
#       AB
#       CB
#     ",
#     widths = c(1.2, 1.2),   
#     heights = c(6, 1.5),
#     guides = "collect"
#   ) +
#   patchwork::plot_annotation(tag_levels = 'A') &
#   ggplot2::theme(
#     text = ggplot2::element_text(size = 16),
#     axis.text = ggplot2::element_text(size = 14),
#     axis.title = ggplot2::element_text(size = 16),
#     axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1, vjust = 1.1),
#     plot.tag = ggplot2::element_text(face = "bold", size = 20),
#     plot.tag.position = c(0.01, 0.98),   
#     legend.position = "bottom",
#     legend.justification = c(0.5, 0.5),
#     legend.box = "horizontal",
#     legend.direction = "horizontal",
#     legend.title = ggplot2::element_text(face = "bold", size = 16),
#     legend.text = ggplot2::element_text(size = 14),
#     legend.key.size = grid::unit(0.6, "cm"),
#     legend.spacing.x = grid::unit(0.3, "cm"),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.box.margin = margin(0, 0, 0, 0)
#   )

figure_3_theme <- ggplot2::theme(
  text  = element_text(size = 22),
  axis.text = element_text(size = 22),
  axis.title = element_text(size = 22),
  plot.tag = element_text(face = "bold", size = 22),
  plot.tag.position = c(0.01, 1)
)

fig_3a_theme <- figure_3a +
  figure_3_theme +
  ggplot2::theme(legend.position = "none")

fig_3b_theme <- figure_3b +
  figure_3_theme +
  ggplot2::theme(
    legend.position    = "bottom",
    legend.justification = c(0, 0.5),
    legend.box         = "horizontal",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 22, face = "bold"),
    legend.text        = element_text(size = 18),
    legend.key.size    = grid::unit(0.6, "cm"),
    legend.spacing.x   = grid::unit(0.3, "cm"),
    legend.margin      = margin(0, 0, 0, 0),
    legend.box.margin  = margin(t = 5, r = 0, b = 0, l = 0)
  )

legend_grob <- cowplot::get_legend(fig_3b_theme)

legend_panel <- patchwork::wrap_elements(full = legend_grob) +
  ggplot2::theme(plot.tag = element_blank())

fig_3b_no_legend <- fig_3b_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = "black",
                                                                   linewidth = 0.2))) +
  ggplot2::theme(
    legend.position       = "bottom",
    legend.title.position = "top",       
    legend.justification  = c(0, 0.5),
    legend.box            = "vertical",  
    legend.direction      = "horizontal",
    legend.title          = element_text(size = 22, face = "bold", hjust = 0),
    legend.text           = element_text(size = 18),
    legend.key.size       = grid::unit(0.6, "cm"),
    legend.spacing.x      = grid::unit(0.3, "cm"),
    legend.margin         = margin(0, 0, 0, 0),
    legend.box.margin     = margin(0, 0, 0, 0)
  )

legend_grob <- cowplot::get_legend(fig_3b_no_legend)

fig_3b_no_legend <- fig_3b_no_legend +
  ggplot2::theme(legend.position = "none")

legend_panel <- patchwork::wrap_elements(full = legend_grob) +
  ggplot2::theme(plot.tag = element_blank())

figure_3 <- (fig_3a_theme + fig_3b_no_legend + legend_panel) +
  patchwork::plot_layout(
    design = "
      AB
      CB
    ",
    widths  = c(1.2, 1.2),
    heights = c(6, 1.5)
  ) +
  patchwork::plot_annotation(tag_levels = "A")

# figure_3

ggplot2::ggsave("../figures/figure_3_composite.tiff",
                plot        = figure_3,
                width       = 26,
                height      = 14,
                dpi         = 600,
                compression = "lzw")


#### 7. IN-TEXT NUMBERS (Section 4 – Data overview) ####
# only females 
db1 <- db %>% 
  dplyr::filter(sex == "F")

levels(as.factor(db1$sex))

##### 7.1. National median peak abundance (week of maximum national median) #####
# Corresponds to the sentence: "maximum national median values reaching
# 66 mosquitoes in week 27" (Section 4).
national_weekly <- db1 %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(median_national = median(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(week)

print(national_weekly, n = Inf)

# A. "maximum national median values reaching 66 mosquitoes in week 27"
national_weekly %>%
  dplyr::filter(median_national == max(median_national, na.rm = TRUE)) %>%
  print()

# B. "detections beginning to rise consistently from late spring (around week 20)"
national_weekly %>%
  dplyr::filter(week >= 16 & week <= 23) %>%
  print()

# C. "pronounced peak between late June (week 26) and late July (week 30)"
national_weekly %>%
  dplyr::filter(week >= 26 & week <= 30) %>%
  print()

# D. "approaching zero after week 42 (mid-October)"
national_weekly %>%
  dplyr::filter(week >= 40 & week <= 45) %>%
  print()

##### 7.2. Maximum median cell-level abundance #####
# Corresponds to the sentence: "local median values peak at 200 female Cx. pipiens
# mosquitoes" (Section 4).

# peak_emr <- db1 %>%
#   filter(Region == "Emilia-Romagna") %>%
#   group_by(cell_id) %>%
#   summarise(median_cell = median(value, na.rm = TRUE)) %>%
#   summarise(max_cell = max(median_cell, na.rm = TRUE))
# 
# peak_emr

peak_max <- db1 %>%
  group_by(Region, cell_id) %>%
  summarise(median_cell = median(value, na.rm = TRUE), .groups = "drop") %>%
  slice_max(median_cell, n = 1)

print(peak_max)

##### 7.3. Seasonal onset and end by macro-region #####
# Activity threshold set at median >= 1 mosquito per trap per sampling event.
# Macro-regions follow the grouping described in Section 4 (Figure 3C).
# Campania and Calabria excluded due to single-year data (2022).
# NOTE: Puglia & Basilicata (2020-2022, 3 years) is NOT added to any
# macro-region below -- the existing Campania/Calabria exclusion is
# explicitly about short time series distorting a seasonal-onset
# estimate, and Puglia & Basilicata's coverage is similarly short. Left
# for you to decide rather than assumed either way.
north   <- c("Liguria", "Piedmont", "Lombardy", "Veneto",
             "Friuli-Venezia Giulia", "Emilia-Romagna", "Autonomous Province of Trento")
centre  <- c("Tuscany", "Lazio", "Umbria", "Marche")
# south   <- c("Campania", "Calabria")
islands <- c("Sardinia", "Sicily")

threshold <- 1

# seasonal <- db %>%
#   mutate(macro = case_when(
#     Region %in% north   ~ "North",
#     Region %in% centre  ~ "Centre",
#     # Region %in% south   ~ "South",
#     Region %in% islands ~ "Islands"
#   )) %>%
#   filter(!is.na(macro)) %>%
#   group_by(macro, week) %>%
#   summarise(med = median(value, na.rm = TRUE), .groups = "drop")

seasonal <- db1 %>%
  mutate(macro = case_when(
    Region %in% north   ~ "North",
    Region %in% centre  ~ "Centre",
    Region %in% islands ~ "Islands"
  )) %>%
  filter(!is.na(macro)) %>%
  group_by(macro, week) %>%
  # Usa il 90° percentile (vede cosa succede nel 10% dei posti più infestati)
  summarise(metric = quantile(value, probs = 0.90, na.rm = TRUE), .groups = "drop")

# Onset: first week where median abundance >= threshold
seasonal %>%
  filter(metric >= threshold) %>%
  group_by(macro) %>%
  summarise(onset = min(week))

# End of season: last week where median abundance >= threshold
seasonal %>%
  filter(metric >= threshold) %>%
  group_by(macro) %>%
  summarise(end = max(week))


#### 8. FIGURE 4 ####
##### 8.1. FIGURE 4A – National seasonal profile of female Cx. pipiens median abundance ####
# Median observed abundance per week pooled across all years and regions,
# with IQR (25th–75th percentiles) shown as vertical error bars.
# Aggregation: for each year × week, the median across grid cells is first
# computed, then the across-year median and IQR are derived (two-stage median).
# This approach reduces the influence of highly sampled regions (e.g.,
# Emilia-Romagna) while preserving the overall seasonal signal.

year_weekly <- db1 %>%
  dplyr::group_by(year, week, cell_id) %>%
  dplyr::summarise(median_site = median(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(year, week) %>%
  dplyr::summarise(median_week_year = median(median_site, na.rm = TRUE), .groups = "drop")

db1_median_year <- year_weekly %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    years      = n(),
    median_value = median(median_week_year, na.rm = TRUE),
    iqr_lower  = quantile(median_week_year, 0.25, na.rm = TRUE),
    iqr_upper  = quantile(median_week_year, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

figure_4a <- ggplot2::ggplot(db1_median_year, aes(x = week, y = median_value)) +
  geom_errorbar(aes(ymin = iqr_lower, ymax = iqr_upper),
                width = 0.4, color = "darkgrey") +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = seq(0, 52, 4), 
    limits = c(1, 52),
    labels = c(
      "0\n", 
      "4\nJan", "8\nFeb", "12\nMar", "16\nApr", 
      "20\nMay", "24\nJun", "28\nJul", "32\nAug", 
      "36\nSep", "40\nOct", "44\nNov", "48\nDec", 
      "52\n"
    )
  ) +
  labs(
    x = "Week",
    y = expression("Median observed " * italic("Culex pipiens") * " abundance")
  ) +
  theme_bw() +
  theme(
    text       = element_text(size = 20),
    axis.text  = element_text(size = 18),
    axis.title = element_text(size = 20)
  )

figure_4a

# ggplot2::ggsave("../figures/average_year_data_paper.tiff",
#                 plot = figure_4a,
#                 width = 13, height = 8, dpi = 600)

##### 8.3. FIGURE 4C – Seasonal profiles stratified by region #####
# Region-level seasonal profiles computed as the across-year median of weekly
# grid-cell medians, with IQR. Campania and Calabria excluded due to
# insufficient temporal replication (single year of data, 2022).

db1_median_year_reg <- db1 %>%
  dplyr::filter(!Region %in% c("Calabria", "Campania")) %>%
  dplyr::mutate(
    Institute = dplyr::recode(
      Institute,
      "Istituto per le Piante da Legno e l'Ambiente"                                = "IPLA and IZSPLV",
      "Istituto Zooprofilattico Sperimentale del Piemonte, Liguria e Valle d'Aosta" = "IPLA and IZSPLV"
    )
  ) %>%
  dplyr::group_by(Region, Institute, year, week, cell_id) %>%
  dplyr::summarise(median_site = median(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(Region, Institute, week) %>%
  dplyr::summarise(
    years        = n_distinct(year),
    median_value = median(median_site, na.rm = TRUE),
    iqr_lower    = quantile(median_site, 0.25, na.rm = TRUE),
    iqr_upper    = quantile(median_site, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

figure_4c <- ggplot2::ggplot(
  db1_median_year_reg %>% mutate(Region = factor(Region, levels = region_north_south)),
  aes(x = week, y = median_value, color = Institute)
) +
  geom_errorbar(aes(ymin = pmax(iqr_lower, 0), ymax = iqr_upper), width = 0.4) +
  geom_point(size = 3) +
  facet_wrap(~ Region, scales = "free_y", ncol = 3) +
  scale_color_manual(values = manual_colors_colorblind, labels = institute_labels) +
  scale_x_continuous(
    breaks = seq(0, 52, 4), 
    limits = c(1, 52),
    labels = c(
      "0\n", 
      "4\nJan", "8\nFeb", "12\nMar", "16\nApr", 
      "20\nMay", "24\nJun", "28\nJul", "32\nAug", 
      "36\nSep", "40\nOct", "44\nNov", "48\nDec", 
      "52\n"
    )
  ) +
  guides(color = guide_legend(nrow = 1)) +        
  labs(
    x = "Week",
    y = expression("Median observed " * italic("Culex pipiens") * " abundance")
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    text             = element_text(size = 20),
    axis.text        = element_text(size = 18),
    axis.title       = element_text(size = 20),
    strip.background = element_rect(fill = "gray95"),
    strip.text       = element_text(face = "bold", size = 18)
  )

figure_4c

# ggplot2::ggsave("../figures/average_year_regions_data_paper.tiff",
#                 plot = figure_4c,
#                 width = 15, height = 15, dpi = 600)


##### 8.3. FIGURE 4B – Spatial distribution of median abundance (9x9 km grid) #####
# Bubble map of grid-cell median abundance aggregated across all years.
# Point size and colour intensity are proportional to the spatial median of
# daily median counts, as described in Section 4.

spatial_abundance_year <- db1 %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  dplyr::group_by(year, longitude, latitude) %>%
  dplyr::summarise(median_value = median(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    abundance_class = dplyr::case_when(
      median_value <= 10 ~ "0-10",
      median_value > 10  & median_value <= 100 ~ "11-100",
      median_value > 100 & median_value <= 500 ~ "101-500",
      median_value > 500 ~ "> 500"
    ),
    abundance_class = factor(abundance_class, levels = c("0-10", "11-100", "101-500", "> 500"))
  ) %>%
  dplyr::arrange(median_value)

spatial_abundance_sf <- sf::st_as_sf(spatial_abundance_year,
                                     coords = c("longitude", "latitude"),
                                     crs = 4326)

figure_4b <- ggplot2::ggplot() +
  ggspatial::geom_sf(data = world_map_utm, fill = "gray90", color = "black", linewidth = 0.1) +
  geom_sf(data = ita_reg, fill = "white", color = "black", linewidth = 0.2) +
  geom_sf(data = spatial_abundance_sf,
          aes(size = abundance_class, fill = abundance_class),
          shape = 21, color = "gray30", stroke = 0.3, alpha = 0.4) +
  facet_wrap(~ year) +
  scale_size_manual(name   = "Median abundance",
                    values = c("0-10" = 1.5, "11-100" = 3, "101-500" = 5, "> 500" = 7)) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.15,
                       name = "Median abundance") +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 5)),
         size = guide_legend()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tl",
                                    style  = north_arrow_fancy_orienteering(),
                                    height = unit(1, "cm"), width = unit(1, "cm")) +
  ggspatial::coord_sf(xlim = c(6, 19), ylim = c(35, 48), expand = FALSE, crs = 4326) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    legend.position      = c(0.88, 0.12),
    legend.justification = c(0.5, 0.5),
    legend.background    = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
    legend.key.size      = unit(0.5, "cm"),
    legend.title         = element_text(size = 14, face = "bold"),
    legend.text          = element_text(size = 12),
    text                 = element_text(size = 16),
    axis.text            = element_text(size = 14),
    strip.background     = element_rect(fill = "gray95"),
    strip.text           = element_text(face = "bold", size = 13),
    panel.grid.major     = element_line(color = "gray90", linetype = "dashed"),
    plot.margin       = margin(t = 5, r = 0, b = 5, l = 5),  # r = 0 elimina margine destro
    plot.tag.position = c(0.005, 0.99)
  )

figure_4b

# ggplot2::ggsave("../figures/spatial_abundance_data_paper.tiff",
#                 plot = figure_4b,
#                 width = 17, height = 19, dpi = 600)


##### 8.4. COMPOSITE FIGURE 4 #####
figure_4a_tag <- figure_4a +
  ggplot2::theme(
    plot.tag          = element_text(face = "bold", size = 28),
    plot.tag.position = c(0.01, 0.98),
    text              = element_text(size = 28),
    axis.text         = element_text(size = 26),
    axis.title        = element_text(size = 28)
  )

figure_4b_tag <- figure_4b +
  ggplot2::theme(
    plot.tag          = element_text(face = "bold", size = 28),
    plot.tag.position = c(0.01, 0.99),  # B più vicino al bordo del pannello
    text              = element_text(size = 26),
    axis.text         = element_text(size = 24),
    strip.text        = element_text(face = "bold", size = 22),
    legend.title      = element_text(size = 22, face = "bold"),
    legend.text       = element_text(size = 20)
  )

figure_4c_tag <- figure_4c +
  ggplot2::theme(
    plot.tag          = element_text(face = "bold", size = 28),
    plot.tag.position = c(0.01, 0.98),
    text              = element_text(size = 28),
    axis.text         = element_text(size = 26),
    strip.text        = element_text(face = "bold", size = 22),
    legend.text       = element_text(size = 20),
    legend.title      = element_text(size = 22, face = "bold")
  )

layout <- c(
  patchwork::area(t = 1, l = 1, b = 3, r = 6),   
  patchwork::area(t = 1, l = 7, b = 5, r = 12),  
  patchwork::area(t = 6, l = 1, b = 10, r = 12)  
)

figure_4 <- (figure_4a_tag + figure_4b_tag + figure_4c_tag) +
  patchwork::plot_layout(
    design = layout,
    guides = "keep"
  ) +
  patchwork::plot_annotation(tag_levels = "A")

# figure_4

ggplot2::ggsave("../figures/figure_4_composite.tiff",
                plot        = figure_4,
                width       = 32,
                height      = 40,
                dpi         = 600,
                compression = "lzw")

