![Docker](https://img.shields.io/badge/docker-ready-blue)
![GitHub Actions](https://img.shields.io/badge/CI-GitHub_Actions-green)
![License](https://img.shields.io/badge/license-MIT-lightgrey)

# GPSDataProcessing

A fully automated **R-based pipeline for processing livestock GPS data**, built with **Docker**, **Google Cloud Storage**, and **GitHub Actions CI/CD**.  
The workflow cleans, analyzes, and visualizes GPS tracking data **without any manual intervention**.

This project is intended for researchers, data scientists, and analysts working with large-scale GPS or spatial tracking data.

---

## Overview

This repository demonstrates a real-world, production-style data processing workflow for livestock GPS data.

Key features:

- 📦 **Raw data stored in Google Cloud Storage (GCS)**
- 🐳 **Reproducible Docker environment**
- ⚙️ **Fully automated GitHub Actions pipeline**
- 📊 **Spatial analysis and exploratory data analysis (EDA)**
- 🚫 **No raw data committed to GitHub**

All processing runs inside a Docker container, ensuring **fully reproducible results** across machines and environments.

Google Cloud Storage is used to decouple large spatial datasets from code, keeping the repository lightweight and secure.

---

## What This Pipeline Does

1. Downloads raw GPS and spatial data from Google Cloud Storage  
2. Cleans and filters GPS observations  
3. Computes grazing seasons and pasture use  
4. Integrates landscape variables (altitude, slope, habitat, etc.)  
5. Generates summary tables and visualizations  
6. Uploads outputs as GitHub Actions artifacts  

---

## Scripts

| **Script** | **Purpose** |
|------|--------|
| `clean_I.R` | Initial GPS data cleaning and spatial filtering |
| `clean_II.R` | Grazing season and pasture day calculations |
| `clean_III.R` | Integration of landscape variables |
| `eda.R` | Exploratory data analysis and visualization |

---

## Automated Workflow

The entire pipeline is executed by **GitHub Actions**.

### Execution Environment

- Docker image:
````bash
ghcr.io/asalonio/gps-processing:1.0
````

- Includes:
 - R 4.4.3
 - sf, terra, tidyverse, and related packages
 - GDAL/PROJ/GEOS system libraries

### Triggers

- Automatically on push to main
- Manually via **GitHub → Actions → Run workflow**

---

## Pipeline Flow:

````mermaid
graph LR
    A[Google Cloud Storage<br/>Raw Data ZIP] --> B[clean_I.R<br/>Data cleaning]
    B --> C[clean_II.R<br/>Grazing seasons]
    C --> D[clean_III.R<br/>Landscape variables]
    D --> E[eda.R<br/>Visualizations]
    E --> F[Artifacts<br/>CSV · XLSX · PNG]
````

---

## Data Management (Google Cloud Storage)

### Where the data lives

Raw GPS and spatial data are stored as a ZIP archive in **Google Cloud Storage (GCS)**.

**Current dataset:**

gps_data_processing/data-v1.zip

You can view and download the current input data here:
https://console.cloud.google.com/storage/browser/_details/gps_data_processing/data-v1.zip;tab=live_object?project=even-trainer-481417-i1

> The repository itself does **not** contain raw data.

---

### Updating the input data

To provide new data to the pipeline:

1. Prepare a ZIP file with the expected internal structure:

````kotlin
data/
├── zone1_data.csv
├── zone2_data.csv
├── traceability_data.csv
├── *.shp
├── *.tif
````

2. Upload the ZIP file to the GCS bucket:

    - Replace data-v1.zip, **or**
    - Upload a new version (e.g. data-v2.zip) and update the workflow download URL

Once uploaded, the next workflow run will automatically use the new data.

---

### What happens during a run 

On each GitHub Actions execution:

1. The ZIP file is downloaded from GCS

2. Data are extracted into the data/ directory

3. All R scripts run sequentially inside Docker

4. Results are saved and uploaded as artifacts

---

## Outputs

Results are generated automatically and uploaded as **GitHub Actions artifacts**, available for **30 days**.

### Example outputs

| **Type** | **File** | **Description** |
|----|----|----|
| Excel | `demand_producer.xlsx` | Producer demand summaries |
| CSV | `season_unit.csv` | Grazing seasons by management unit |
| CSV | `landscape_data.csv` | GPS points with landscape attributes |
| PNG | `gps_obs_by_livestock_type.png` | Observations by livestock type |
| PNG | `total_grazing_days_by_ug.png` | Grazing days by unit |

---

## Repository Structure

```text
GPSDataProcessing
├── .github/
│   └── workflows/process.yml
├── .gitignore
├── clean_I.R
├── clean_II.R
├── clean_III.R
├── Dockerfile
├── eda.R
├── LICENSE
└── README.md
```

---


## Running Locally (Optional)

Advanced users can run the pipeline locally using Docker:

```bash
docker run --rm \
  -v $(pwd):/workspace \
  ghcr.io/asalonio/gps-processing:1.0 \
  Rscript clean_I.R
```

---

## Why this Architecture?

- 🐳 Reproducible environment
- ☁️ Scalable cloud storage
- 🔐 Secure data handling
- 🚀 Zero-touch automation
- 📉 No large files in GitHub

---

## License

This project is licensed under the [MIT License](LICENSE).
