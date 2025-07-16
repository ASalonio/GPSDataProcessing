#### Grazing Demand Analysis ####

# Purpose: Calculate grazing season start/end dates, pasture days, and demand metrics for producers and management units.
# Generate summaries: Season duration, pasture days by producer/unit, and lead collars.

# Libraries
library(sp)
library(terra)
library(sf)
library(readr)
library(tidyr)
library(dplyr)  # Use terra:: where needed to avoid conflicts with dplyr
library(stringr)
library(tibble)
library(openxlsx)
library(lubridate)

# Import data (placeholders for processed data files)
data_path <- "path/to/data/output"  # Generic path to output directory
input_data <- read_csv2(file.path(data_path, "processed_data.csv"))  # Placeholder for input data

# Select relevant columns and filter ZEC observations
clean_data <- input_data %>% filter(!ZEC == 0) %>% select(user_id, device_id, livestock_type, timestamp)

# Season Start (Entry) by User
name19 <- "Start_Date"
name20 <- "Lead_Device"
name21 <- "Rank_Start"
entry_data <- clean_data %>% 
  group_by(user_id) %>% 
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_head() %>% 
  rename_with(~c(name19), c(timestamp)) %>%
  rename_with(~c(name20), c(device_id)) %>%
  rename_with(~c(name21), c(ranking))

# Season End (Exit) by User
name22 <- "End_Date"
name23 <- "Lead_Device_End"
name24 <- "Rank_End"
exit_data <- clean_data %>% 
  group_by(user_id) %>% 
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_tail() %>%
  rename_with(~c(name22), c(timestamp)) %>%
  rename_with(~c(name23), c(device_id)) %>%
  rename_with(~c(name24), c(ranking))

# Combine Entry and Exit
season_union <- entry_data %>% 
  left_join(exit_data, by = "user_id", keep = FALSE) %>% 
  select(!c(Rank_Start, Rank_End))

# Calculate Season Duration
season_summary <- season_union %>% 
  mutate(Season_Duration = as.period(interval(start = Start_Date, end = End_Date))) %>% 
  relocate(c(user_id, Season_Duration), .before = Lead_Device)

# Pasture Days by User (Monthly and Annual)
pasture_days_month_user <- clean_data %>% 
  group_by(user_id) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(user_id, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_user <- pasture_days_month_user %>% 
  group_by(user_id) %>%
  summarise(Total_Pasture_Days = sum(Pasture_Days_Month))

# Combine with Season Summary
season_summary <- season_summary %>% 
  left_join(pasture_days_year_user) %>% 
  relocate(c(Total_Pasture_Days, Season_Duration), .before = Lead_Device)

# Pasture Days by Livestock Type (Grouped by User)
pasture_days_month_livestock <- clean_data %>% 
  group_by(user_id, livestock_type) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(user_id, livestock_type, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_livestock <- pasture_days_month_livestock %>% 
  group_by(user_id, livestock_type) %>%
  summarise(Total_Pasture_Days_Livestock = sum(Pasture_Days_Month), .groups = "keep")

# Pasture Days by Device (Grouped by User)
pasture_days_month_device <- clean_data %>% 
  group_by(user_id, device_id) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(user_id, device_id, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_device <- pasture_days_month_device %>% 
  group_by(user_id, device_id) %>%
  summarise(Total_Pasture_Days_Device = sum(Pasture_Days_Month), .groups = "keep")

# Import Reference Data (Placeholder)
reference_data <- read_csv2(file.path(data_path, "reference_data.csv"))

# Combine Demand Metrics
demand_summary <- reference_data %>% 
  left_join(pasture_days_year_livestock, by = c("user_id", "livestock_type")) %>% 
  left_join(pasture_days_year_device, by = c("user_id", "device_id")) %>% 
  mutate(perc_ZEC = round(perc_ZEC, 0)) %>% 
  group_by(user_id) %>% 
  arrange(desc(perc_ZEC), .by_group = TRUE)

# Import Traceability (Placeholder)
trace_data <- read_csv2(file.path(data_path, "traceability_data.csv"))

# Rename Device ID
name1 <- "device_id"
trace <- trace_data %>% rename_with(~c(name1), c(GPS)) %>% select(device_id, herd_size, herd_id)
remove(trace_data)

# Combine with Traceability
final_producer <- demand_summary %>% 
  left_join(trace) %>% 
  select(user_id, origin, livestock_type, Total_Pasture_Days_Livestock, device_id, N, ZEC, perc_ZEC, Total_Pasture_Days_Device, herd_size, herd_id)

# Season Start/End by Management Unit
unit_data <- read_csv2(file.path(data_path, "unit_data.csv")) %>% 
  select(device_id, livestock_type, management_unit, timestamp) %>% 
  mutate(management_unit = str_replace(management_unit, "/ Unit", "- Unit")) %>% 
  mutate(management_unit = str_replace(management_unit, "_", " - ")) %>% 
  mutate(management_unit = str_replace(management_unit, "/", "-")) %>% 
  mutate(management_unit = str_replace(management_unit, "No Unit", "No - Unit"))

# Entry by Management Unit
entry_unit <- unit_data %>% 
  group_by(management_unit) %>% 
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_head() %>%
  rename_with(~c(name19), c(timestamp)) %>%
  rename_with(~c(name20), c(device_id)) %>%
  select(Lead_Device, management_unit, Start_Date)

# Exit by Management Unit
exit_unit <- unit_data %>% 
  group_by(management_unit) %>% 
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_tail() %>%
  rename_with(~c(name22), c(timestamp)) %>%
  rename_with(~c(name23), c(device_id)) %>%
  select(Lead_Device_End, management_unit, End_Date)

# Combine Entry and Exit for Units
unit_union <- entry_unit %>% left_join(exit_unit, keep = FALSE)

# Calculate Season Duration for Units
unit_season <- unit_union %>% 
  mutate(Season_Duration = as.period(interval(start = Start_Date, end = End_Date))) %>% 
  relocate(c(management_unit, Season_Duration), .before = Lead_Device)

# Pasture Days by Management Unit
pasture_days_month_unit <- unit_data %>% 
  group_by(management_unit) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(management_unit, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_unit <- pasture_days_month_unit %>% 
  group_by(management_unit) %>%
  summarise(Total_Pasture_Days = sum(Pasture_Days_Month), .groups = "keep")

# Combine with Unit Season
unit_season <- unit_season %>% 
  left_join(pasture_days_year_unit) %>% 
  relocate(management_unit, Total_Pasture_Days, Season_Duration, .before = Lead_Device)

# Pasture Days by Livestock Type per Unit
pasture_days_month_livestock_unit <- unit_data %>% 
  group_by(management_unit, livestock_type) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(management_unit, livestock_type, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_livestock_unit <- pasture_days_month_livestock_unit %>% 
  group_by(management_unit, livestock_type) %>%
  summarise(Total_Pasture_Days_Livestock = sum(Pasture_Days_Month), .groups = "keep")

# Pasture Days by Device per Unit
pasture_days_month_device_unit <- unit_data %>% 
  group_by(management_unit, device_id) %>%
  mutate(Day = day(timestamp), Month = month(timestamp)) %>%
  group_by(management_unit, device_id, Month) %>%
  summarise(Pasture_Days_Month = n_distinct(Day), .groups = "keep")

pasture_days_year_device_unit <- pasture_days_month_device_unit %>% 
  group_by(management_unit, device_id) %>%
  summarise(Total_Pasture_Days_Device = sum(Pasture_Days_Month), .groups = "keep")

# Import Unit Reference Data (Placeholder)
unit_reference <- read_csv2(file.path(data_path, "unit_reference_data.csv"))

# Rename Management Unit for Consistency
name25 <- "management_unit"
pasture_days_year_livestock_unit <- pasture_days_year_livestock_unit %>% rename_with(~c(name25), c(management_unit))
pasture_days_year_device_unit <- pasture_days_year_device_unit %>% rename_with(~c(name25), c(management_unit))

# Combine Demand Metrics for Units
final_unit <- unit_reference %>% 
  left_join(pasture_days_year_livestock_unit, by = c("management_unit", "livestock_type")) %>% 
  left_join(pasture_days_year_device_unit, by = c("management_unit", "device_id")) %>% 
  left_join(trace) %>% 
  mutate(Perc_Occupancy = round(Perc_Occupancy, 0)) %>% 
  group_by(management_unit) %>% 
  arrange(desc(Perc_Occupancy), .by_group = TRUE) %>% 
  select(management_unit, livestock_type, Total_Pasture_Days_Livestock, device_id, ZEC, N_UG, Perc_Occupancy, Total_Pasture_Days_Device, herd_size, herd_id)

# Export (generic output paths)
write_csv2(pasture_days_month_livestock_unit, "path/to/output/pasture_days_month_unit.csv")
write_csv2(season_summary, "path/to/output/season_producer.csv")
openxlsx::write.xlsx(season_summary, "path/to/output/season_producer.xlsx")
openxlsx::write.xlsx(final_producer, "path/to/output/demand_producer.xlsx")
write_csv2(unit_season, "path/to/output/season_unit.csv")
write_csv2(final_unit, "path/to/output/demand_unit.csv")