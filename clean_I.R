#### Spatial Data Cleaning Step I ####

# Purpose: Clean spatial data by removing duplicates, filtering coordinates, and assigning zone and ZEC indicators.
# Create columns: "Origin" (zone indicator), "ZEC" (within ZEC boundary), "Livestock" (type, NA as "No_Livestock").
# Generate summaries: Percentage of ZEC observations per collar, lead collars by ZEC and UG.

# Fail fast
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
library(sp)

# Create the outpout folder (silently, even if it exists)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Verify it exists
if (file.exists("output") && dir.exists("output")) {
  print("Folder 'output' created or already exists!")
}

# Clear paths
raw_data_path <- Sys.getenv("RAW_DATA_PATH", "data")
processed_path <- Sys.getenv("PROCESSED_DATA_PATH", "data/processed")
output_path <- Sys.getenv("OUTPUT_PATH", "output")

# Import data (placeholders for raw data files)
zone1_raw <- read_csv2(file.path(raw_data_path, "tabular/zone1_data.csv"),
                       col_types = "Tddccc")
zone2_raw <- read_csv2(file.path(raw_data_path, "tabular/zone2_data.csv"),
                       col_types = "Tddccc")
lost_data <- read_csv2(file.path(raw_data_path, "tabular/lost_data.csv"),
                       col_types = "Tddccc")

# Combine zone 2 with lost data
total_zone2 <- bind_rows(zone2_raw, lost_data)

# Import generic boundaries (placeholders)
country_bound <- terra::vect(
  file.path(raw_data_path, "vector/country_boundaries.shp"),
  crs = "EPSG:25830"
)
zec_bound <- terra::vect(
  file.path(raw_data_path, "vector/zec_boundaries.shp"),
  crs = "EPSG:25830"
)
ug_bound <- terra::vect(
  file.path(raw_data_path, "vector/management_units.shp"),
  crs = "EPSG:25830"
)

# Import generic traceability data
trace <- read_csv2(file.path(raw_data_path, "tabular/traceability_data.csv"))
trace <- trace |> select(device_id, livestock, herd_size, herd_id)

# Exploration (generic checks)
glimpse(zone1_raw)
glimpse(total_zone2)

# Remove observations with zero longitude
zone1_clean <- zone1_raw %>%
  select(ID, device_id, user_id, timestamp, x, y) %>%
  filter(!x == 0) %>%
  distinct(device_id, timestamp, user_id, .keep_all = TRUE)

zone2_clean <- total_zone2 %>%
  select(ID, device_id, user_id, timestamp, x, y) %>%
  filter(!x == 0) %>%
  distinct(device_id, timestamp, user_id, .keep_all = TRUE)

# Assign unique IDs
zone1_clean$ID <- seq.int(nrow(zone1_clean))
zone2_clean$ID <- seq.int(nrow(zone2_clean))

# Create SpatVector
zone1_v <- terra::vect(zone1_clean, geom = c("x", "y"), crs = "EPSG:4326")
zone2_v <- terra::vect(zone2_clean, geom = c("x", "y"), crs = "EPSG:4326")

# Project
zone1_proj <- terra::project(zone1_v, "EPSG:25830")
zone2_proj <- terra::project(zone2_v, "EPSG:25830")

# Clip to country boundaries keeping geometry as points
inside1 <- is.related(zone1_proj, country_bound, "within")
message(
  sum(inside1),
  " / ",
  length(inside1),
  " zone1 points inside country boundaries"
)

clip_zone1 <- zone1_proj[inside1, ] %>%
  tidyterra::select(ID, user_id, device_id, timestamp)
message(
  "Extracted points: ", nrow(clip_zone1),
  " | Expected: ", sum(inside1)
)

inside2 <- is.related(zone2_proj, country_bound, "within")
message(
  sum(inside2),
  " / ",
  length(inside2),
  " zone2 points inside country boundaries"
)

clip_zone2 <- zone2_proj[inside2, ] %>%
  tidyterra::select(ID, user_id, device_id, timestamp)
message(
  "Extracted points: ", nrow(clip_zone2),
  " | Expected: ", sum(inside2)
)

# Double-check geometry type before converting
if (!terra::geomtype(clip_zone1) == "points")
  stop("clip_zone1 is not points!")
if (!terra::geomtype(clip_zone2) == "points")
  stop("clip_zone2 is not points!")

# Convert to data frames
list_zone1 <- terra::as.data.frame(clip_zone1, geom = "XY")
list_zone2 <- terra::as.data.frame(clip_zone2, geom = "XY")

# Clip to ZEC boundaries keeping geometry as points
inside_zec1 <- is.related(clip_zone1, zec_bound, "within")
message(
  sum(inside_zec1),
  " / ",
  length(inside_zec1),
  " zone1 points inside zec boundaries"
)

clip_zec_zone1 <- clip_zone1[inside_zec1, ] %>%
  tidyterra::select(ID, user_id, device_id, timestamp)
message(
  "Extracted points: ", nrow(clip_zec_zone1),
  " | Expected: ", sum(inside_zec1)
)

inside_zec2 <- is.related(clip_zone2, zec_bound, "within")
message(
  sum(inside_zec2),
  " / ",
  length(inside_zec2),
  " zone2 points inside zec boundaries"
)

clip_zec_zone2 <- clip_zone2[inside_zec2, ] %>%
  tidyterra::select(ID, user_id, device_id, timestamp)
message(
  "Extracted points: ", nrow(clip_zec_zone2),
  " | Expected: ", sum(inside_zec2)
)

# Double-check geometry type before converting
if (!terra::geomtype(clip_zec_zone1) == "points")
  stop("clip_zec_zone1 is not points!")
if (!terra::geomtype(clip_zec_zone2) == "points")
  stop("clip_zec_zone2 is not points!")

# Convert to data frames
list_zec_zone1 <- terra::as.data.frame(clip_zec_zone1, geom = "XY")
list_zec_zone2 <- terra::as.data.frame(clip_zec_zone2, geom = "XY")

# Add ZEC column (1 for within ZEC, 0 otherwise)
list_zec_zone1 <- list_zec_zone1 %>%
  select(ID) %>%
  mutate("ZEC" = 1)

list_zec_zone2 <- list_zec_zone2 %>%
  select(ID) %>%
  mutate("ZEC" = 1)

# Join with country data and fill NA with 0
join_zec_zone1 <- left_join(list_zone1,
                            list_zec_zone1, by = "ID", keep = FALSE) %>%
  replace_na(list(ZEC = 0))

join_zec_zone2 <- left_join(list_zone2,
                            list_zec_zone2, by = "ID", keep = FALSE) %>%
  replace_na(list(ZEC = 0))

# Add Origin column
join_zec_zone1 <- join_zec_zone1 %>%
  mutate(origin = "zone1") %>%
  relocate(c(origin, ZEC), .after = device_id)

join_zec_zone2 <- join_zec_zone2 %>%
  mutate(origin = "zone2") %>%
  relocate(c(origin, ZEC), .after = device_id)

# Select relevant columns
list_zone1_final <- join_zec_zone1 %>%
  select(user_id, device_id, origin, ZEC, timestamp, x, y)

list_zone2_final <- join_zec_zone2 %>%
  select(user_id, device_id, origin, ZEC, timestamp, x, y)

# Combine zones
total_data <- bind_rows(list_zone1_final, list_zone2_final)

# Incorporate traceability
total_with_trace <- total_data %>%
  left_join(trace, by = "device_id", keep = FALSE) %>%
  replace_na(list(livestock = "No_Livestock"))

total_with_trace$ID <- seq.int(nrow(total_with_trace))

total_with_trace <- total_with_trace %>%
  select(ID, user_id, device_id, livestock, origin, ZEC, timestamp, x, y)

remove(total_data)

# Percentage of total points with livestock assigned
assigned_livestock <- sum(total_with_trace$livestock != "No_Livestock")
total_points <- nrow(total_with_trace)

message(
  assigned_livestock,
  " / ",
  total_points,
  " points (",
  round((assigned_livestock / total_with_trace) * 100, 2),
  "%) total points are assigned to a livestock"
)

# Lead collars within each livestock type by ZEC
# Filter collars with minimum observations in ZEC (e.g., 48 for one day)
id_filter <- total_with_trace %>%
  group_by(device_id) %>%
  summarise(ZEC = sum(ZEC)) #%>%
  #filter(ZEC >= 48)

filtered_data <- semi_join(total_with_trace, id_filter, by = "device_id")

# Percentage of ZEC observations by collar
perc_zec_device <- filtered_data %>%
  group_by(user_id, device_id, origin, livestock) %>%
  summarise(
    N = n(),
    ZEC = sum(ZEC),
    perc_ZEC = 100 * ZEC / N,
    .groups = "drop_last"
  ) %>%
  arrange(desc(perc_ZEC), .by_group = TRUE)

# Lead collar per user × livestock
ref <- perc_zec_device %>%
  group_by(user_id, livestock) %>%
  slice_max(order_by = perc_ZEC, n = 1, with_ties = FALSE) %>%
  ungroup()

# Percentage of ZEC per user × livestock
perc_zec_user <- filtered_data %>%
  group_by(user_id, livestock) %>%
  summarise(
    N = n(),
    ZEC = sum(ZEC),
    perc_ZEC = 100 * ZEC / N,
    .groups = "drop"
  )

# Add lead collar info
lead_zec <- perc_zec_user %>%
  left_join(
    ref %>% select(user_id, livestock, device_id, origin),
    by = c("user_id", "livestock")
  )

# Analysis at Management Unit (UG) scale
total_v <- terra::vect(total_with_trace, geom = c("x", "y"), crs = "EPSG:25830")

# Extract points within UG (use extract to preserve the UG column)
inside_ug <- terra::extract(ug_bound["UG"], total_v)
total_v$UG <- inside_ug$UG
n_inside <- sum(!is.na(total_v$UG))
message(
  n_inside,
  " / ",
  nrow(total_v),
  " total points inside UG boundaries"
)

clip_ug <- total_v[!is.na(total_v$UG), ]
message(
  "Extracted points: ", nrow(clip_ug),
  " | Expected: ", n_inside
)

# Sanity check (should always match)
stopifnot(nrow(clip_ug) == n_inside)

# Convert to data frames
list_clip_ug <- terra::as.data.frame(clip_ug, geom = "XY")

# Unit assignment
nom_ug <- list_clip_ug %>%
  distinct(ID, user_id, device_id, ZEC, .keep_all = TRUE) %>%
  select(ID, UG)

ug_data <- total_with_trace %>%
  left_join(nom_ug, by = "ID") %>%
  replace_na(list(UG = "No_Unit"))

ug_filtered <- ug_data %>%
  filter(UG != "No_Unit")

# Percentage of zec points with ug assigned
inside_zec <- sum(ug_data$ZEC == 1)
assigned_ug <- sum(ug_data$UG != "No_Unit")

message(
  assigned_ug,
  " / ",
  inside_zec,
  " points (",
  round((assigned_ug / inside_zec) * 100, 2),
  "%) inside ZEC are assigned to a UG"
)

# No point outside ZEC should have a UG
stopifnot(all(ug_data$UG[ug_data$ZEC == 0] == "No_Unit"))

# Assigned UG cannot exceed points inside ZEC
stopifnot(assigned_ug <= inside_zec)

# Filter collars with minimum observations in ZEC (e.g., 48 for one day)
id_ug_filter <- ug_filtered %>%
  group_by(device_id) %>%
  summarise(ZEC = sum(ZEC)) #%>%
  #filter(ZEC >= 48)

ug_data_filtered <- semi_join(ug_filtered, id_ug_filter, by = "device_id")

# Percentage of UG observations by collar (generic UG names)
resume_ug <- ug_data_filtered %>%
  group_by(device_id, livestock, UG) %>%
  summarise(
    N_UG = n(),
    .groups = "drop"
  ) %>%
  group_by(device_id, livestock) %>%
  mutate(
    ZEC = sum(N_UG),
    perc_UG = (N_UG / ZEC) * 100
  ) %>%
  arrange(device_id, livestock, desc(perc_UG))

# Lead collar per UG and livestock (example for UG_1, extend as needed)
lead_ug <- resume_ug %>%
  group_by(livestock, UG) %>%
  slice_max(order_by = perc_UG, n = 1, with_ties = FALSE) %>%
  ungroup()

# Save (generic output paths)
write_csv2(filtered_data, file.path(processed_path, "processed_data.csv"))
write_csv2(ug_data_filtered, file.path(processed_path, "ug_processed_data.csv"))
write_csv2(lead_zec, file.path(processed_path, "lead_zec.csv"))
write_csv2(lead_ug, file.path(processed_path, "lead_ug.csv"))