# A harmonised dataset of adult *Culex pipiens* mosquitoes from 17 administrative Italian regions, 2008–2022

[![Preprint DOI](https://img.shields.io/badge/preprint-10.32942%2FX2RM3S-blue)](https://doi.org/10.32942/X2RM3S)
[![Dataset DOI](https://img.shields.io/badge/dataset-10.5281%2Fzenodo.19494972-blue)](https://doi.org/10.5281/zenodo.19494972)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This repository contains the analysis scripts accompanying the preprint and dataset below. Raw and processed data are archived separately on Zenodo (DOI-versioned, not duplicated here).

## 📄 Preprint
**Longo, E., Blaha, M., Accorsi, A., et al.** *A harmonised dataset of adult Culex pipiens mosquitoes from 17 administrative Italian regions, 2008–2022.* EcoEvoRxiv. https://doi.org/10.32942/X2RM3S

## 📦 Dataset
Full dataset (v1.0.0, CC BY 4.0) hosted on Zenodo: https://doi.org/10.5281/zenodo.19494972

| File | Description | Size |
|---|---|---|
| `longo_et_al_db_aggregated.csv` | Harmonised observational records | 16.6 MB |
| `longo_et_al_data_dictionary.csv` | Variable descriptions | 4.5 kB |
| `grid_cellid_global.tif` / `grid_cellid_italy.tif` | Spatial grid rasters (9×9 km, ERA5-Land compatible) | — |
| `grid_cells_italy.gpkg` | Grid cells as vector geopackage | 2.0 MB |
| `grid_provenance_italy.tif` | Grid provenance raster | — |
| `longo_et_al_culex_scripts.zip` | Analysis scripts (mirrored in this repo) | 137 kB |
| `longo_et_al_dummy_data.zip` | Dummy/example data for testing scripts | 93.1 kB |

## 📁 This repository
Contains the analysis scripts (`scripts/`) used to process and harmonise the raw trap records into the analysis-ready dataset, plus the data dictionary for quick reference.

## Citation
If you use this dataset or code, please cite:
> Longo, E. et al. (2026). *A harmonised dataset of adult Culex pipiens mosquitoes from 17 administrative Italian regions, 2008–2022.* EcoEvoRxiv. https://doi.org/10.32942/X2RM3S

## License
- Code in this repository: [choose: MIT / GPL-3.0]
- Data (Zenodo): CC BY 4.0
