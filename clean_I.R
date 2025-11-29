#### Spatial Data Cleaning Step I ####

# Purpose: Clean spatial data by removing duplicates, filtering coordinates, and assigning zone and ZEC indicators.
# Create columns: "Origin" (zone indicator), "ZEC" (within ZEC boundary), "Livestock" (type, NA as "No_Livestock").
# Generate summaries: Percentage of ZEC observations per collar, lead collars by ZEC and UG.

# Libraries
library(sp)
library(terra)
library(sf)
library(tidyverse)
library(openxlsx)

# Clear paths
raw_data_path <- Sys.getenv("RAW_DATA_PATH", "data")
processed_path <- Sys.getenv("PROCESSED_DATA_PATH", "data/output")
output_path <- Sys.getenv("OUTPUT_PATH", "output")

# Create processed data directory
dir.create(processed_path, showWarnings = FALSE, recursive = TRUE)

# Import data (placeholders for raw data files)
zone1_raw <- read_csv(file.path(raw_data_path, "zone1_data.csv"), col_types = "cddddddddddTccddcdd")  # Placeholder for zone 1 data
zone2_raw <- read_csv(file.path(raw_data_path, "zone2_data.csv"), col_types = "cddddddddddTccddcdd")  # Placeholder for zone 2 data
lost_data <- read_csv(file.path(raw_data_path, "lost_data.csv"), col_types = "cddddddddddTccddcdd")  # Placeholder for lost data

# Combine zone 2 with lost data
total_zone2 <- bind_rows(zone2_raw, lost_data)

# Import generic boundaries (placeholders)
country_bound <- terra::vect(file.path(raw_data_path, "country_boundaries.shp"), crs = "EPSG:25830")
zec_bound <- terra::vect(file.path(raw_data_path, "zec_boundaries.shp"), crs = "EPSG:25830")
ug_bound <- terra::vect(file.path(raw_data_path, "management_units.shp"), crs = "EPSG:25830")

# Import generic traceability data
traceability <- read_csv2(file.path(raw_data_path, "traceability_data.csv"))

# Rename GPS traceability (generic column name)
name1 <- "device_id"
trace <- traceability %>% rename_with(~c(name1), c(GPS)) |> select(device_id, Livestock, Herd_Size, Herd_ID)
remove(traceability)

# Exploration (generic checks)
glimpse(zone1_raw)
glimpse(total_zone2)

# Remove observations with zero longitude
zone1_clean <- zone1_raw %>% select(device_id, user_id, device_id, timestamp, longitude, latitude) %>% 
  filter(!longitude == 0) %>% distinct(device_id, timestamp, user_id, .keep_all = TRUE)
zone2_clean <- total_zone2 %>% select(device_id, user_id, device_id, timestamp, longitude, latitude) %>% 
  filter(!longitude == 0) %>% distinct(device_id, timestamp, user_id, .keep_all = TRUE)

# Assign unique IDs
zone1_clean$ID <- seq.int(nrow(zone1_clean))
zone2_clean$ID <- seq.int(nrow(zone2_clean))

# Spatial filtering
zone1_spdf <- zone1_clean %>% select(ID, user_id, device_id, timestamp, longitude, latitude)
coordinates(zone1_spdf) <- ~longitude + latitude
zone2_spdf <- zone2_clean %>% select(ID, user_id, device_id, timestamp, longitude, latitude)
coordinates(zone2_spdf) <- ~longitude + latitude

zone1_v <- terra::vect(zone1_spdf)
zone2_v <- terra::vect(zone2_spdf)

# Project to target CRS (generic EPSG code)
terra::crs(zone1_v) <- "+proj=longlat +zone=30 +datum=WGS84 +no_defs"
terra::crs(zone2_v) <- "+proj=longlat +zone=30 +datum=WGS84 +no_defs"
zone1_proj <- project(zone1_v, "EPSG:25830")
zone2_proj <- project(zone2_v, "EPSG:25830")

# Clip to country boundaries
clip_zone1 <- terra::intersect(zone1_proj, country_bound) |> tidyterra::select(ID, user_id, device_id, timestamp)
clip_zone2 <- terra::intersect(zone2_proj, country_bound) |> tidyterra::select(ID, user_id, device_id, timestamp)

# Convert to data frames
list_zone1 <- terra::as.data.frame(clip_zone1, geom = "XY")
list_zone2 <- terra::as.data.frame(clip_zone2, geom = "XY")

# Clip to ZEC boundaries
clip_zec_zone1 <- terra::intersect(clip_zone1, zec_bound) |> tidyterra::select(ID, user_id, device_id, timestamp)
clip_zec_zone2 <- terra::intersect(clip_zone2, zec_bound) |> tidyterra::select(ID, user_id, device_id, timestamp)

# Convert to data frames
list_zec_zone1 <- terra::as.data.frame(clip_zec_zone1, geom = "XY")
list_zec_zone2 <- terra::as.data.frame(clip_zec_zone2, geom = "XY")

# Add ZEC column (1 for within ZEC, 0 otherwise)
list_zec_zone1 <- list_zec_zone1 %>% select(ID) %>% mutate("ZEC" = 1)
list_zec_zone2 <- list_zec_zone2 %>% select(ID) %>% mutate("ZEC" = 1)

# Join with country data and fill NA with 0
join_zec_zone1 <- left_join(list_zone1, list_zec_zone1, by = "ID", keep = FALSE) %>% replace_na(list(ZEC = 0))
join_zec_zone2 <- left_join(list_zone2, list_zec_zone2, by = "ID", keep = FALSE) %>% replace_na(list(ZEC = 0))

# Add Origin column
join_zec_zone1 <- join_zec_zone1 %>% mutate(Origin = "zone1") %>% relocate(c(Origin, ZEC), .after = device_id)
join_zec_zone2 <- join_zec_zone2 %>% mutate(Origin = "zone2") %>% relocate(c(Origin, ZEC), .after = device_id)

# Select relevant columns
list_zone1_final <- join_zec_zone1 %>% select(user_id, device_id, Origin, ZEC, timestamp, x, y)
list_zone2_final <- join_zec_zone2 %>% select(user_id, device_id, Origin, ZEC, timestamp, x, y)

# Combine zones
total_data <- bind_rows(list_zone1_final, list_zone2_final)

# Incorporate traceability
total_with_trace <- total_data %>% left_join(trace, by = "device_id", keep = FALSE) %>% replace_na(list(Livestock = "No_Livestock"))
total_with_trace$ID <- seq.int(nrow(total_with_trace))
total_with_trace <- total_with_trace %>% select(ID, user_id, device_id, Livestock, Origin, ZEC, timestamp, x, y)
remove(total_data)

# Lead collars within each livestock type by ZEC
# Filter collars with minimum observations in ZEC (e.g., 48 for one day)
id_filter <- total_with_trace %>% group_by(device_id) %>% summarise(ZEC = sum(ZEC)) %>% filter(ZEC >= 48)
filtered_data <- semi_join(total_with_trace, id_filter, by = "device_id")

# Percentage of ZEC observations by collar
resume_I <- filtered_data %>% group_by(user_id, device_id, Origin, Livestock) %>%
  summarise(N = n(), ZEC = sum(ZEC), perc_ZEC = (ZEC/N)*100) %>% ungroup() %>%
  group_by(user_id, Livestock) %>% arrange(desc(perc_ZEC), .by_group = TRUE)

# Lead collar per user and livestock
ref <- resume_I %>% group_by(user_id, Livestock) %>% arrange(desc(perc_ZEC), .by_group = TRUE) %>% slice_max(order_by = perc_ZEC)

# Percentage of ZEC observations by user and livestock
resume_II <- filtered_data %>% group_by(user_id, Livestock) %>% summarise(N = n(), ZEC = sum(ZEC), perc_ZEC = (ZEC/N)*100)
agrupe <- resume_II %>% left_join(ref %>% select(user_id, device_id, Origin, Livestock), by = c("user_id", "Livestock"))

# Analysis at Management Unit (UG) scale
total_v <- terra::vect(total_with_trace, geom = c("x", "y"), crs = "EPSG:25830")
total_ug <- terra::intersect(total_v, ug_bound)
list_total_ug <- terra::as.data.frame(total_ug, geom = "XY")
nom_ug <- list_total_ug %>% distinct(ID, user_id, device_id, ZEC, .keep_all = TRUE) %>% select(ID, management_unit)
ug_data <- total_with_trace %>% left_join(nom_ug, by = "ID") %>% replace_na(list(management_unit = "No Unit"))
ug_filtered <- ug_data %>% filter(management_unit != "No Unit")

# Filter collars with minimum observations in ZEC
id_ug_filter <- ug_filtered %>% group_by(device_id) %>% summarise(ZEC = sum(ZEC)) %>% filter(ZEC >= 48)
ug_data_filtered <- semi_join(ug_filtered, id_ug_filter, by = "device_id")

# Percentage of UG observations by collar (generic UG names)
resume_ug <- ug_data_filtered %>% group_by(device_id, Livestock) %>%
  summarise(ZEC = sum(ZEC), UG_1 = sum(management_unit == "Unit_1"), perc_UG_1 = (UG_1/ZEC)*100,
            UG_2 = sum(management_unit == "Unit_2"), perc_UG_2 = (UG_2/ZEC)*100,
            .groups = "keep") %>% ungroup() %>% group_by(Livestock) %>% arrange(desc(perc_UG_1), .by_group = TRUE)

# Lead collar per UG and livestock (example for UG_1, extend as needed)
ref_ug_1 <- resume_ug %>% select(device_id, Livestock, ZEC, UG_1, perc_UG_1) %>%
  group_by(Livestock) %>% arrange(desc(perc_UG_1)) %>% slice_max(order_by = perc_UG_1) %>% filter(!perc_UG_1 == 0)

# Save (generic output paths)
write_csv2(filtered_data, file.path(processed_path), "processed_data.csv")
write_csv2(ug_data_filtered, file.path(processed_path), "ug_processed_data.csv")
write_csv2(agrupe, file.path(processed_path), "lead_zec.csv")
write_csv2(resume_ug, file.path(processed_path), "lead_ug.csv")