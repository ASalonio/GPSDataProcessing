#### Landscape Analysis ####

# Purpose: Incorporate temporal (month, season) and landscape variables (altitude, slope, orientation, etc.) into GPS data.

# Fail Fast
options(
  warn = 1,  # warnings se muestran inmediatamente
  error = function(e) {
    traceback(2)
    quit(status = 1)
  }
)

# Libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

# Import data (placeholder for processed data)
raw_data_path <- Sys.getenv("RAW_DATA_PATH", "data")
processed_data_path <- Sys.getenv("PROCESSED_DATA_PATH", "data/processed")
input_data <- read_csv2(file.path(processed_data_path, "ug_processed_data.csv"))
output_path <- Sys.getenv("OUTPUT_PATH", "output")

# Select relevant columns and add temporal variables
clean_data <- input_data %>%
  select(ID,
         user_id,
         device_id,
         origin,
         livestock,
         UG,
         timestamp,
         x,
         y) %>%
  mutate(UG = str_replace(UG, "/ Unit", "- Unit")) %>%
  mutate(UG = str_replace(UG, "_", " - ")) %>%
  mutate(UG = str_replace(UG, "/", "-")) %>%
  mutate(UG = str_replace(UG, "No Unit", "No - Unit")) %>%
  mutate(
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en"),
    season = case_when(
      month == "january" ~ "winter",
      month == "february" ~ "winter",
      month == "march" ~ "spring",
      month == "april" ~ "spring",
      month == "may" ~ "spring",
      month == "june" ~ "summer",
      month == "july" ~ "summer",
      month == "august" ~ "summer",
      month == "september" ~ "fall",
      month == "october" ~ "fall",
      month == "november" ~ "fall",
      month == "december" ~ "winter"
    )
  )

# Import Traceability (placeholder)
trace_data <- read_csv2(
  file.path(raw_data_path, "tabular/traceability_data.csv")
)

# Remove origin and livestock from Traceability
trace <- trace_data %>%
  select(device_id, herd_size, herd_id)

remove(trace_data)

# Vectorize
data_vector <- terra::vect(clean_data,
  geom = c("x", "y"), crs = "EPSG:25830"
)

# Import Slope Data
slope_data <- terra::vect(file.path(raw_data_path, "vector/slope_data.shp"))

# Import Aspect Data
aspect_data <- terra::vect(file.path(raw_data_path, "vector/aspect_data.shp"))

# Reproject Slope
slope_data <- project(slope_data, "EPSG:25830")

# Intersect with Slope and Orientation
slope_intersect <- terra::intersect(data_vector, slope_data)
aspect_intersect <- terra::intersect(data_vector, aspect_data)

# Convert to Data Frames
slope_df <- terra::as.data.frame(slope_intersect)
aspect_df <- terra::as.data.frame(aspect_intersect)

# Remove Duplicates
slope_df_clean <- slope_df %>%
  select(ID, slope) %>%
  distinct(.keep_all = TRUE)

aspect_df_clean <- aspect_df %>%
  select(ID, aspect) %>%
  distinct(.keep_all = TRUE)

# Import DEM Data
altitude_data <- terra::rast(
  file.path(raw_data_path, "raster/altitude_data.tif")
)

# Extract Altitude
altitude_extract <- terra::extract(altitude_data, data_vector)

# Rename Altitude
altitude_df <- altitude_extract %>%
  rename(altitude = altitude_data)

remove(altitude_extract)

# Convert to Data Frame and Remove Duplicates
altitude_df_clean <- terra::as.data.frame(altitude_df) %>%
  select(ID, altitude) %>%
  distinct(.keep_all = TRUE)

# Import Habitat Data (placeholder)
habitat_data <- terra::vect(file.path(raw_data_path, "vector/habitat_data.shp"))

# Intersect with Habitat
habitat_intersect <- terra::intersect(data_vector, habitat_data)

# Convert to Data Frame and Remove Duplicates
habitat_df_clean <- terra::as.data.frame(habitat_intersect) %>%
  select(
    ID,
    eu1,
    eu1_es,
    eu2,
    eu2_es,
    cob1,
    cob2
  ) %>%
  distinct(.keep_all = TRUE)

# Import Forest Inventory Data (placeholder)
forest_data <- terra::vect(
  file.path(raw_data_path, "vector/forest_inventory.shp")
)

# Intersect with Forest Inventory
forest_intersect <- terra::intersect(data_vector, forest_data)

# Convert to Data Frame and Remove Duplicates
forest_df_clean <- terra::as.data.frame(forest_intersect) %>%
  select(
    ID,
    fccher,
    fccmat,
    fccarb,
    tipes10,
    tipes_es,
    sp1_es,
    sp2_es
  ) %>%
  distinct(.keep_all = TRUE)

# Import Water Points Data (placeholder)
water_data <- terra::vect(
  file.path(raw_data_path, "vector/water_data.shp"), crs = "EPSG:25830"
)

# Intersect with Water Points
water_intersect <- terra::intersect(data_vector, water_data)

# Convert to Data Frame and Remove Duplicates
water_df_clean <- terra::as.data.frame(water_intersect) %>%
  select(ID, water, water_obs, water_LIFE) %>%
  distinct(ID, water, water_obs, water_LIFE, .keep_all = TRUE)

# Import Paths Data
path_data <- terra::vect(
  file.path(raw_data_path, "vector/path_data.shp"), crs = "EPSG:25830"
)

# Buffer Paths (1m)
buffered_paths <- terra::buffer(path_data, width = 1)

# Intersect with Paths
path_intersect <- terra::intersect(data_vector, buffered_paths)

# Convert to Data Frame and Remove Duplicates
path_df_clean <- terra::as.data.frame(path_intersect) %>%
  select(ID, path_id, path) %>%
  distinct(ID, .keep_all = TRUE)

# Import Shrub Clearance Data
clearance_data <- terra::vect(
  file.path(raw_data_path, "vector/clearance_data.shp"), crs = "EPSG:25830"
)

# Intersect with Shrub Clearance Data
clearance_intersect <- terra::intersect(data_vector, clearance_data)

# Convert to Data Frame
clearance_df <- terra::as.data.frame(clearance_intersect)

#Group by point, Select columns and Remove Duplicates
clearance_df_clean <- clearance_df |>
  group_by(ID) %>%
  summarise(
    clear_id = paste(clear_id, collapse = ", "),
    clear = paste(clear, collapse = ", "),
    clear_LIFE = paste(clear_LIFE, collapse = ", "),
    clear_year = paste(clear_year, collapse = ", "),
    .groups = "drop"
  )

# Combine all Landscape Data
final_landscape <- clean_data %>%
  left_join(altitude_df_clean) %>%
  left_join(slope_df_clean) %>%
  left_join(aspect_df_clean) %>%
  left_join(habitat_df_clean) %>%
  left_join(forest_df_clean) %>%
  left_join(water_df_clean) %>%
  left_join(path_df_clean) %>%
  left_join(clearance_df_clean) %>% #by = "ID", keep = FALSE
  left_join(trace, by = "device_id") |>
  select(
    ID,
    user_id,
    device_id,
    origin,
    livestock,
    herd_size,
    herd_id,
    UG,
    timestamp,
    month,
    season,
    altitude,
    slope,
    aspect,
    eu1,
    eu1_es,
    eu2,
    eu2_es,
    cob1,
    cob2,
    fccher,
    fccmat,
    fccarb,
    tipes10,
    tipes_es,
    sp1_es,
    sp2_es,
    water,
    water_obs,
    water_LIFE,
    path_id,
    path,
    clear_id,
    clear,
    clear_LIFE,
    clear_year,
    x,
    y
  ) %>%
  distinct(ID, .keep_all = TRUE)

# Vectorize
final_landscape_vector <- terra::vect(
  final_landscape, geom = c("x", "y"), crs = "EPSG:25830"
)

# Export (generic output path)
readr::write_csv2(final_landscape, file.path(output_path, "landscape_data.csv"))