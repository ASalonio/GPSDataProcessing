# GPSDataProcessing

A fully automated R workflow for processing livestock GPS data with GitHub Actions CI/CD. Clean, analyse, and visualize GPS tracking data with zero manual interventions.

## Overview

This repository contains sanitized R scripts designed to demonstrate a complete workflow for working with GPS data. The scripts cover:
- **Data Cleaning**: Preprocessing and structuring GPS datasets.
- **Spatial Analysis**: Integrating and analyzing spatial attributes (e.g., altitude, slope, land cover).
- **Exploratory Data Analysis (EDA)**: Generating visualizations to explore patterns and trends.

## Scripts

- `clean_I.R`: Initial data cleaning and filtering of GPS observations.
- `clean_II.R`: Calculation of grazing seasons and pasture days with spatial integration.
- `clean_III.R`: Incorporation of landscape variables (e.g., altitude, slope) into GPS data.
- `eda.R`: Exploratory data analysis with visualizations (e.g., bar plots, boxplots, heatmaps).

**GitHub Actions handles everything automatically:**
- ✅ R 4.3.0 + 50+ packages (`sf`, `terra`, `tidyverse`, `ggplot2`, etc.)
- ✅ GDAL/GEOS/PROJ spatial libraries
- ✅ Directory creation
- ✅Triggered by git push data/* (Auto-run pipeline) or manually GitHub → Actions → "Run workflow"

**Pipeline Flow:**
````mermaid
graph LR
    A[Push data/] --> B[clean_I.R<br/>ZEC filtering]
    B --> C[clean_II.R<br/>Grazing seasons]
    C --> D[clean_III.R<br/>Landscape vars]
    D --> E[eda.R<br/>Visualizations]
    E --> F[output/*.png<br/>output/*.xlsx]
````
## Requirements

- R environment (version 4.0 or higher recommended).
- Required R packages (install via `install.packages()`):
  - `sf`
  - `terra`
  - `tidyr`
  - `dplyr`
  - `tidyterra`
  - `stringr`
  - `tibble`
  - `openxlsx`
  - `lubridate`
  - `scales`
  - `ggplot2`
  - `tidyverse`

## Usage

1. Clone the repo:
   ```bash
   git clone <https://github.com/ASalonio>
   cd GPSDataProcessing

2. Add your GPS CSV files to data/
   ```bash
   git add data/*
   git commit -m "Add GPS data"
   git push

3. Watch it Run
  - GitHub → Actions tab → See live progress
  - ~10-15 minutes → Complete pipeline
  - Results auto-committed to output/ & data/output/

4. Download Results
  - Artifacts available for 30 days
  - Or directly from repo: output/*.png, output/*.xlsx

## Directory Structure

GPSDataProcessing/
├── data/                    # ← ADD YOUR RAW DATA HERE
│   ├── zone1_data.csv
│   ├── zone2_data.csv
│   ├── traceability_data.csv
│   ├── country_boundaries.shp
│   ├── zec_boundaries.shp
│   ├── management_units.shp
│   ├── slope_data.shp
│   ├── orientation_data.shp
│   ├── habitat_data.shp
│   ├── forest_inventory.shp
│   ├── water_points.shp
│   ├── path_data.shp
│   ├── clearance_data.shp
│   └── dem_data.tif                
├── data/output/            # ← AUTO-GENERATED
│   └── processed_data.csv
├── output/                 # ← AUTO-GENERATED FINAL RESULTS
│   ├── demand_producer.xlsx
│   ├── season_unit.csv
│   ├── landscape_data.csv
│   └── gps_obs_*.png
├── clean_I.R, clean_II.R... # ← AUTOMATED SCRIPTS
└── process.yml             # ← GITHUB ACTIONS WORKFLOW

## Example Outputs

## Example Outputs

| **Type** | **File** | **Content** |
|----------|----------|-------------|
| **Excel** | `demand_producer.xlsx` | Producer demand summaries |
| **CSV** | `season_unit.csv` | Management unit grazing seasons |
| **PNG** | `gps_obs_by_livestock_type.png` | Observations by livestock type |
| **PNG** | `total_grazing_days_by_ug.png` | Grazing days by management unit |

## License

This project is licensed under the [MIT License](LICENSE).
