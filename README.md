# GPSDataProcessing

A collection of R scripts for processing and analyzing GPS data, including data cleaning, spatial analysis, and exploratory visualizations. This repository showcases workflows for handling geospatial datasets, suitable for environmental or ecological studies.

## Overview

This repository contains sanitized R scripts designed to demonstrate a complete workflow for working with GPS data. The scripts cover:
- **Data Cleaning**: Preprocessing and structuring GPS datasets.
- **Spatial Analysis**: Integrating and analyzing spatial attributes (e.g., altitude, slope, land cover).
- **Exploratory Data Analysis (EDA)**: Generating visualizations to explore patterns and trends.

These scripts are intended as a portfolio piece to highlight data science and geospatial analysis skills.

## Scripts

- `clean_I.R`: Initial data cleaning and filtering of GPS observations.
- `clean_II.R`: Calculation of grazing seasons and pasture days with spatial integration.
- `clean_III.R`: Incorporation of landscape variables (e.g., altitude, slope) into GPS data.
- `eda.R`: Exploratory data analysis with visualizations (e.g., bar plots, boxplots, heatmaps).

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

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd GPSDataProcessing

## License

This project is licensed under the [MIT License](LICENSE).
