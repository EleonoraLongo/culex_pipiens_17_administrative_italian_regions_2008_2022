# =============================================================================
# REFERENCE GRID — ERA5-Land global grid + Italy coastal gap-fill (Zenodo)
# =============================================================================
# Author     : Eleonora Longo
#              PhD Student in Agrifood and Environmental Science
#              Center of Agriculture, Food and Environment (C3A)
#              University of Trento
#              Via Edmund Mach, 1 - 38098 San Michele all'Adige (TN)
#              eleonora.longo@unitn.it
#
# Purpose    : Produces the reference grid for the aggregation.
#
# Reference  : Hijmans, R.J., et al. (2005). Very high resolution interpolated
#              climate surfaces for global land areas. Int. J. Climatol., 25:
#              1965-1978.
#
# Input      : ../other_data/weather/temperature/daniele/era5land_tp_t2m_2022_01.nc
#              ../other_data/comuni.geojson (Italy boundary, via union)
#
# Output     : ../other_data/reference_grid/global/grid_cellid_global.tif
#              ../other_data/reference_grid/global/grid_country_continent_global.tif
#              ../other_data/reference_grid/global/grid_cells_global.csv
#              ../other_data/reference_grid/italy/<var>_gapfilled_italy.tif
#              ../other_data/reference_grid/italy/grid_cellid_italy.tif
#              ../other_data/reference_grid/italy/grid_provenance_italy.tif
#              ../other_data/reference_grid/italy/grid_country_continent_italy.tif
#              ../other_data/reference_grid/italy/grid_cells_italy.csv
#              ../other_data/reference_grid/italy/grid_cells_italy.gpkg
#
# Sections   :
#    0. Libraries & Parameters
#    1. Load & Inspect ERA5-Land
#    2. Global Grid (raw, unfilled -- published as-is)
#    3. Italy Grid (cropped, 3x3 focal gap-fill on the coast)
#       3.1. Crop & gap-fill
#       3.2. Provenance (filled flag + distance to nearest real cell)
#       3.3. cell_id -- native to this raster, exactly as in VectAbundance_Grid.R
#       3.4. Round-trip verification
#    4. Save Outputs
#    5. Point -> Cell Lookup (usage template)
# =============================================================================

rm(list = ls())

#### 0. LIBRARIES & PARAMETERS ####
library(terra)
library(sf)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

ERA5_FILE <- "../other_data/weather/temperature/daniele/era5land_tp_t2m_2022_01.nc"
VAR       <- "t2m"     # sub-dataset defining the land/sea mask -- same variable 1_2 uses
LAYER     <- 1         # time step defining the mask
W         <- 3         # focal window (odd); 3 = one ring, ~10 km at 0.1 deg

ITALY_BOUNDARY_FILE <- "../other_data/comuni.geojson"
ITALY_CROP_BUFFER_DEG <- 0.5  # generous buffer around Italy for the crop,
# so near-shore cells outside the strict
# boundary (open sea just off the coast) are
# still available as fill donors/candidates

MAKE_POLYGONS_ITALY <- TRUE   # Italy grid is small enough to polygonise
MAKE_POLYGONS_GLOBAL <- FALSE # NEVER TRUE: a global land grid at ERA5-Land
# resolution is on the order of ~10^6 cells;
# polygonising it is not memory-realistic and
# is not needed for a raster + lookup-table
# deposit

OUTDIR_GLOBAL <- "../other_data/reference_grid/global"
OUTDIR_ITALY  <- "../other_data/reference_grid/italy"
dir.create(OUTDIR_GLOBAL, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTDIR_ITALY,  showWarnings = FALSE, recursive = TRUE)

# Country/continent attribution uses Natural Earth 1:50m country polygons
# (rnaturalearthdata package). 
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  stop("Package 'rnaturalearthdata' is required for country/continent attribution on the reference grid -- install it first (available via CRAN or, on Debian/Ubuntu, the r-cran-rnaturalearthdata apt package).")
}
data("countries50", package = "rnaturalearthdata", envir = environment())
countries_v <- terra::vect(countries50)


#### 1. LOAD & INSPECT ERA5-LAND ####
stopifnot(file.exists(ERA5_FILE))
describe(ERA5_FILE)

era5_full <- terra::rast(ERA5_FILE, subds = VAR)
era5_layer <- era5_full[[LAYER]]

cat("variable :", varnames(era5_full), "\n")
cat("time     :", format(time(era5_layer)), "\n")
cat("res      :", res(era5_layer), "\n")
cat("ncell    :", ncell(era5_layer), "\n")
cat("crs      :", crs(era5_layer, describe = TRUE)$name, "\n")

na0_global <- global(is.na(era5_layer), "sum", na.rm = TRUE)[[1]]
cat("NA cells (global, sea/no-data):", na0_global,
    sprintf("(%.1f%%)\n", 100 * na0_global / ncell(era5_layer)))


#### 2. GLOBAL GRID (raw, unfilled) ####
# Published as-is: this is the canonical, un-modified ERA5-Land land mask
# and cell numbering. No gap-filling here 
cellid_global <- init(era5_layer, "cell")
cellid_global <- mask(cellid_global, era5_layer)   # NA outside observed land cells
names(cellid_global) <- "cell_id"

# Round-trip check (as in VectAbundance_Grid.R): every published cell_id
# must map back to its own coordinates.
chk_global <- as.data.frame(cellid_global, xy = TRUE, na.rm = TRUE)
id_lookup_global <- cellFromXY(era5_layer, as.matrix(chk_global[, c("x", "y")]))
bad_global <- which(id_lookup_global != chk_global$cell_id | is.na(id_lookup_global))
if (length(bad_global)) {
  cat("GLOBAL cell_id round-trip FAILED for", length(bad_global), "of", nrow(chk_global), "cells\n")
  stop("Global cell_id round-trip failed -- inspect before continuing")
}
cat("\nGlobal cell_id round-trip OK,", nrow(chk_global), "land cells\n")

# Country / continent attribution (see Section 0 note on the boundary
# layer used and its limitations). rasterize(), not a per-point spatial
# join, because a vector point-in-polygon join against ~10^6 global land
# cells is not practical -- rasterize() burns each polygon's attribute
# directly onto the raster template and scales to the whole grid at once.
country_global_r <- terra::rasterize(countries_v, era5_layer, field = "name")
names(country_global_r) <- "country"
continent_global_r <- terra::rasterize(countries_v, era5_layer, field = "continent")
names(continent_global_r) <- "continent"

country_global_r    <- mask(country_global_r, era5_layer)
continent_global_r  <- mask(continent_global_r, era5_layer)

n_global_no_country <- global(is.na(country_global_r) & !is.na(era5_layer), "sum", na.rm = TRUE)[[1]]
cat("Global land cells with no country attribution (small islands / 1:50m gaps):",
    n_global_no_country, sprintf("(%.2f%% of land cells)\n", 100 * n_global_no_country / nrow(chk_global)))


#### 3. ITALY GRID (cropped, 3x3 focal gap-fill on the coast) ####

##### 3.1. Crop & gap-fill #####
italy_boundary <- sf::st_read(ITALY_BOUNDARY_FILE, quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_union()

italy_ext <- sf::st_bbox(italy_boundary)
italy_ext_buffered <- terra::ext(
  italy_ext["xmin"] - ITALY_CROP_BUFFER_DEG, italy_ext["xmax"] + ITALY_CROP_BUFFER_DEG,
  italy_ext["ymin"] - ITALY_CROP_BUFFER_DEG, italy_ext["ymax"] + ITALY_CROP_BUFFER_DEG
)

# Crop from the ORIGINAL raster (era5_layer), purely for computational
# efficiency 
era5_italy_crop <- terra::crop(era5_layer, italy_ext_buffered)

na0_italy <- global(is.na(era5_italy_crop), "sum", na.rm = TRUE)[[1]]
cat("\nItaly crop: NA cells before gap-fill:", na0_italy,
    sprintf("(%.1f%% of the crop)\n", 100 * na0_italy / ncell(era5_italy_crop)))

# Same three settings VectAbundance_Grid.R documents as easy to get wrong:
# fun="mean" (focal() defaults to "sum"), na.rm=TRUE (otherwise any NA in
# the window poisons the result and na.policy="only" becomes a no-op),
# expand=TRUE (pads the crop edge by replication instead of NA).
era5_italy_filled <- focal(era5_italy_crop,
                           w = W, fun = "mean",
                           na.policy = "only", na.rm = TRUE, expand = TRUE)
names(era5_italy_filled) <- paste0(VAR, "_gapfilled")

na1_italy <- global(is.na(era5_italy_filled), "sum", na.rm = TRUE)[[1]]
cat(sprintf("Italy crop: NA %d -> %d (filled %d cells)\n", na0_italy, na1_italy, na0_italy - na1_italy))
if (na0_italy == na1_italy) warning("Gap-fill changed nothing on the Italy crop -- check ITALY_CROP_BUFFER_DEG and the land-sea mask")

##### 3.2. Provenance #####
filled_italy <- (!is.na(era5_italy_filled)) & is.na(era5_italy_crop)
names(filled_italy) <- "filled"

# distance() errors ("no locations to compute distance from") when there
# are zero NA cells in the crop 
if (na0_italy > 0) {
  fill_dist_italy <- distance(era5_italy_crop)
  fill_dist_italy <- mask(fill_dist_italy, era5_italy_filled)
  fill_dist_italy[!filled_italy] <- 0
} else {
  fill_dist_italy <- era5_italy_crop * 0  # all-zero raster, same geometry
}
names(fill_dist_italy) <- "fill_dist_m"

cat("Italy filled cells:", global(filled_italy, "sum", na.rm = TRUE)[[1]], "\n")
if (na0_italy > 0) {
  cat("Italy max fill dist:", round(global(fill_dist_italy, "max", na.rm = TRUE)[[1]] / 1000, 1), "km\n")
} else {
  cat("Italy max fill dist: n/a (no NA cells in the crop)\n")
}

##### 3.3. cell_id -- native to this raster, exactly as in VectAbundance_Grid.R #####
# cell_id here is init(x, "cell") on THIS script's own (cropped, filled)
# Italy raster 
cellid_italy <- init(era5_italy_filled, "cell")
cellid_italy <- mask(cellid_italy, era5_italy_filled)
names(cellid_italy) <- "cell_id"

##### 3.4. Round-trip verification #####
# Self-consistency against THIS raster only (no cross-check against the
# global grid 
chk_italy <- as.data.frame(cellid_italy, xy = TRUE, na.rm = TRUE)
id_lookup_italy <- cellFromXY(era5_italy_filled, as.matrix(chk_italy[, c("x", "y")]))
bad_italy <- which(id_lookup_italy != chk_italy$cell_id | is.na(id_lookup_italy))
if (length(bad_italy)) {
  cat("ITALY cell_id round-trip FAILED for", length(bad_italy), "of", nrow(chk_italy), "cells\n")
  print(utils::head(data.frame(chk_italy[bad_italy, c("x", "y", "cell_id")],
                               lookup = id_lookup_italy[bad_italy])))
  stop("Italy cell_id round-trip failed -- inspect `bad_italy` before continuing")
}
cat("\nItaly cell_id round-trip OK,", nrow(chk_italy), "cells (",
    global(filled_italy, "sum", na.rm = TRUE)[[1]], "gap-filled )\n")

# Country / continent attribution
country_italy_r <- terra::rasterize(countries_v, era5_italy_filled, field = "name")
names(country_italy_r) <- "country"
continent_italy_r <- terra::rasterize(countries_v, era5_italy_filled, field = "continent")
names(continent_italy_r) <- "continent"

country_italy_r   <- mask(country_italy_r, era5_italy_filled)
continent_italy_r <- mask(continent_italy_r, era5_italy_filled)

n_italy_no_country <- global(is.na(country_italy_r) & !is.na(era5_italy_filled), "sum", na.rm = TRUE)[[1]]
cat("Italy grid cells with no country attribution:", n_italy_no_country, "\n")

country_breakdown_italy <- as.data.frame(country_italy_r, na.rm = TRUE) %>%
  dplyr::count(country, sort = TRUE)
cat("\nItaly grid -- country breakdown (non-'Italy' rows are expected at the crop edge, see note above):\n")
print(country_breakdown_italy)


#### 4. SAVE OUTPUTS ####

## Global
grid_stack_global <- c(cellid_global, country_global_r, continent_global_r)

writeRaster(cellid_global, file.path(OUTDIR_GLOBAL, "grid_cellid_global.tif"),
            overwrite = TRUE, datatype = "INT4U")
writeRaster(c(country_global_r, continent_global_r), file.path(OUTDIR_GLOBAL, "grid_country_continent_global.tif"),
            overwrite = TRUE)
tab_global <- as.data.frame(grid_stack_global, xy = TRUE, na.rm = FALSE) %>%
  dplyr::filter(!is.na(cell_id))
write.csv(tab_global, file.path(OUTDIR_GLOBAL, "grid_cells_global.csv"), row.names = FALSE)
cat("\nGlobal grid saved:", nrow(tab_global), "land cells.\n")

if (MAKE_POLYGONS_GLOBAL) {
  # Deliberately gated behind a constant that defaults to FALSE -- see the
  # Section 0 comment on why this is not memory-realistic at global scale.
  grd_global <- as.polygons(cellid_global, aggregate = FALSE, na.rm = TRUE, values = TRUE)
  writeVector(grd_global, file.path(OUTDIR_GLOBAL, "grid_cells_global.gpkg"), overwrite = TRUE)
}

## Italy
grid_stack_italy <- c(cellid_italy, filled_italy, fill_dist_italy, country_italy_r, continent_italy_r)

writeRaster(era5_italy_filled, file.path(OUTDIR_ITALY, paste0(VAR, "_gapfilled_italy.tif")),
            overwrite = TRUE)
writeRaster(cellid_italy, file.path(OUTDIR_ITALY, "grid_cellid_italy.tif"),
            overwrite = TRUE, datatype = "INT4U")
writeRaster(c(filled_italy, fill_dist_italy), file.path(OUTDIR_ITALY, "grid_provenance_italy.tif"),
            overwrite = TRUE)
writeRaster(c(country_italy_r, continent_italy_r), file.path(OUTDIR_ITALY, "grid_country_continent_italy.tif"),
            overwrite = TRUE)

# na.rm = FALSE, then filter on cell_id specifically: with several layers
# combined, as.data.frame(..., na.rm = TRUE) drops a row if ANY layer is
# NA, which would silently discard valid, correctly gap-filled cells
# whenever their country/continent attribution happens to be NA 
tab_italy <- as.data.frame(grid_stack_italy, xy = TRUE, na.rm = FALSE) %>%
  dplyr::filter(!is.na(cell_id))
tab_italy$filled <- as.logical(tab_italy$filled)
write.csv(tab_italy, file.path(OUTDIR_ITALY, "grid_cells_italy.csv"), row.names = FALSE)
cat("Italy grid saved:", nrow(tab_italy), "cells (", sum(tab_italy$filled), "gap-filled ).\n")

if (MAKE_POLYGONS_ITALY) {
  grd_italy <- as.polygons(grid_stack_italy, aggregate = FALSE, na.rm = TRUE, values = TRUE)
  writeVector(grd_italy, file.path(OUTDIR_ITALY, "grid_cells_italy.gpkg"), overwrite = TRUE)
}
