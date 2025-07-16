#### Landscape Analysis ####

# Purpose: Incorporate temporal (month, season) and landscape variables (altitude, slope, orientation, etc.) into GPS data.

# Libraries
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(tidyterra)
library(stringr)

# Import data (placeholder for processed data)
data_path <- "path/to/data/output"  # Generic path to output directory
input_data <- read_csv2(file.path(data_path, "processed_data.csv"))  # Placeholder for input data

# Select relevant columns and add temporal variables
clean_data <- input_data %>% select(ID, user_id, device_id, origin, livestock_type, management_unit, timestamp, x, y) %>%
  mutate(management_unit = str_replace(management_unit, "/ Unit", "- Unit")) %>%
  mutate(management_unit = str_replace(management_unit, "_", " - ")) %>%
  mutate(management_unit = str_replace(management_unit, "/", "-")) %>%
  mutate(management_unit = str_replace(management_unit, "No Unit", "No - Unit")) %>%
  mutate(Month = months(timestamp), Season = case_when(
    Month == "January" ~ "Winter",
    Month == "February" ~ "Winter",
    Month == "March" ~ "Spring",
    Month == "April" ~ "Spring",
    Month == "May" ~ "Spring",
    Month == "June" ~ "Summer",
    Month == "July" ~ "Summer",
    Month == "August" ~ "Summer",
    Month == "September" ~ "Autumn",
    Month == "October" ~ "Autumn",
    Month == "November" ~ "Autumn",
    Month == "December" ~ "Winter"))

# Import Traceability (placeholder)
trace_path <- "path/to/data/input"  # Generic path to input directory
trace_data <- read_csv2(file.path(trace_path, "traceability_data.csv"))

# Rename Device ID
name1 <- "device_id"
trace <- trace_data %>% rename_with(~c(name1), c(GPS)) %>% select(device_id, herd_size, herd_id)
remove(trace_data)

# Combine with Traceability
data_with_trace <- clean_data %>% left_join(trace)

# Vectorize
data_vector <- terra::vect(data_with_trace, geom = c("x", "y"), crs = "EPSG:25830")

# Import Slope Data
slope_path <- "path/to/data/landscape/slope"
slope_data <- terra::vect(file.path(slope_path, "slope_data.shp"))

# Import Orientation Data
orient_path <- "path/to/data/landscape/orientation"
orient_data <- terra::vect(file.path(orient_path, "orientation_data.shp"))

# Reproject Slope
slope_data <- project(slope_data, "EPSG:25830")

# Rename IDs
name1 <- "ID2"
slope_renamed <- slope_data %>% tidyterra::rename_with(~c(name1), c(ID))
orient_renamed <- orient_data %>% tidyterra::rename_with(~c(name1), c(ID))

# Intersect with Slope and Orientation
slope_intersect <- terra::intersect(data_vector, slope_renamed)
orient_intersect <- terra::intersect(data_vector, orient_renamed)

# Convert to Data Frames
slope_df <- terra::as.data.frame(slope_intersect)
orient_df <- terra::as.data.frame(orient_intersect)

# Remove Duplicates
slope_df_clean <- slope_df %>% select(ID, slope_value) %>% distinct(.keep_all = TRUE)
orient_df_clean <- orient_df %>% select(ID, orientation_value) %>% distinct(.keep_all = TRUE)

# Import DEM Data
dem_path <- "path/to/data/landscape/dem"
dem_data <- terra::rast(file.path(dem_path, "dem_data.tif"))

# Extract Altitude
altitude_extract <- terra::extract(dem_data, data_vector)

# Rename Altitude
name1 <- "Altitude"
altitude_df <- altitude_extract %>% rename_with(~c(name1), c(dem_data))
remove(altitude_extract)

# Convert to Data Frame and Remove Duplicates
altitude_df_clean <- terra::as.data.frame(altitude_df) %>% select(ID, Altitude) %>% distinct(.keep_all = TRUE)

# Import Habitat Data (placeholder)
habitat_path <- "path/to/data/landscape/habitat"
habitat_data <- terra::vect(file.path(habitat_path, "habitat_data.shp"))

# Intersect with Habitat
habitat_intersect <- terra::intersect(data_vector, habitat_data)

# Convert to Data Frame and Remove Duplicates
habitat_df_clean <- terra::as.data.frame(habitat_intersect) %>% select(ID, habitat_type1, habitat_type1_desc, habitat_type2, habitat_type2_desc, cover_type1, cover_type2) %>% distinct(.keep_all = TRUE)

# Import Forest Inventory Data (placeholder)
forest_path <- "path/to/data/landscape/forest"
forest_data <- terra::vect(file.path(forest_path, "forest_inventory.shp"))

# Intersect with Forest Inventory
forest_intersect <- terra::intersect(data_vector, forest_data)

# Convert to Data Frame and Remove Duplicates
forest_df_clean <- terra::as.data.frame(forest_intersect) %>% select(ID, forest_cover1, forest_cover2, forest_cover3, species_type, species_type_desc, species1_desc, species2_desc) %>% distinct(.keep_all = TRUE)

# Import Water Points Data (placeholder)
water_path <- "path/to/data/landscape/water"
water_data <- terra::vect(file.path(water_path, "water_points.shp"), crs = "EPSG:25830")

# Rename Fields
name5 <- "Water"
name6 <- "Water_LIFE"
name23 <- "Water_Obs"
water_renamed <- water_data %>% rename_with(~c(name5), c(water_status)) %>% rename_with(~c(name6), c(water_life)) %>% rename_with(~c(name23), c(water_obs))
remove(water_data)

# Intersect with Water Points
water_intersect <- terra::intersect(data_vector, water_renamed)

# Convert to Data Frame and Remove Duplicates
water_df_clean <- terra::as.data.frame(water_intersect) %>% select(ID, Water, Water_Obs, Water_LIFE) %>% distinct(ID, Water, Water_Obs, Water_LIFE, .keep_all = TRUE)

# Import Paths Data
path_path <- "path/to/data/landscape/paths"
path_data <- terra::vect(file.path(path_path, "path_data.shp"), crs = "EPSG:25830")

# Rename Fields
name7 <- "Paths"
name8 <- "Path_ID"
path_renamed <- path_data %>% rename_with(~c(name7), c(path_desc)) %>% rename_with(~c(name8), c(path_id))

# Buffer Paths (1m)
buffered_paths <- terra::buffer(path_renamed, width = 1)

# Intersect with Paths
path_intersect <- terra::intersect(data_vector, buffered_paths)

# Convert to Data Frame and Remove Duplicates
path_df_clean <- terra::as.data.frame(path_intersect) %>% select(ID, Path_ID, Paths) %>% distinct(ID, .keep_all = TRUE)

# Import Clearance Data
clearance_path <- "path/to/data/landscape/clearance"
clearance_data <- terra::vect(file.path(clearance_path, "clearance_data.shp"), crs = "EPSG:25830")

# Assign Unique IDs
clearance_data$ID_clear <- seq.int(nrow(clearance_data))

# Select Parts
part1 <- clearance_data %>% select(ID_clear, clearance_date, clearance_status1, clearance_life1)
part2 <- clearance_data %>% select(ID_clear, clearance_status2, clearance_date2, clearance_life2)

# Rename Fields
name9 <- "Clearance"
name10 <- "Clearance_LIFE"
name11 <- "Clearance_Date"
part1_renamed <- part1 %>% rename_with(~c(name9), c(clearance_status1)) %>% rename_with(~c(name10), c(clearance_life1)) %>% rename_with(~c(name11), c(clearance_date))
part2_renamed <- part2 %>% rename_with(~c(name9), c(clearance_status2)) %>% rename_with(~c(name10), c(clearance_life2)) %>% rename_with(~c(name11), c(clearance_date2))

# Combine Parts
clearance_combined <- tidyterra::bind_spat_rows(part1_renamed, part2_renamed)

# Extract Year and Filter (e.g., 2022)
clearance_year <- clearance_combined %>% mutate(Year = str_sub(Clearance_Date, 1, 4)) %>% select(ID_clear, Clearance, Clearance_LIFE, Year)

# Intersect with Clearance Data (example for 2022)
clearance_intersect <- terra::intersect(data_vector, clearance_year)

# Convert to Data Frame and Remove Duplicates
clearance_df_clean <- terra::as.data.frame(clearance_intersect) %>% select(ID, Clearance, Clearance_LIFE, Year) %>% distinct(.keep_all = TRUE)

# Combine all Landscape Data
final_landscape <- clean_data %>% 
  left_join(altitude_df_clean) %>% 
  left_join(slope_df_clean) %>% 
  left_join(orient_df_clean) %>% 
  left_join(habitat_df_clean) %>% 
  left_join(forest_df_clean) %>% 
  left_join(water_df_clean) %>% 
  left_join(path_df_clean) %>% 
  left_join(clearance_df_clean, by = "ID", keep = FALSE) %>% 
  select(ID, user_id, device_id, origin, livestock_type, herd_size, herd_id, management_unit, timestamp, Month, Season, Altitude, slope_value, orientation_value,
         habitat_type1, habitat_type1_desc, habitat_type2, habitat_type2_desc, cover_type1, cover_type2, forest_cover1, forest_cover2, forest_cover3, species_type, species_type_desc,
         species1_desc, species2_desc, Water, Water_Obs, Water_LIFE, Path_ID, Paths, Clearance, Clearance_LIFE, Year, x, y) %>% 
  distinct(ID, .keep_all = TRUE)

# Vectorize
final_landscape_vector <- terra::vect(final_landscape, geom = c("x", "y"), crs = "EPSG:25830")

# Export (generic output path)
readr::write_csv2(final_landscape, "path/to/output/landscape_data.csv")