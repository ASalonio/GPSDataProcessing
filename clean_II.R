#### Grazing Demand Analysis ####

# Purpose: Calculate grazing season start/end dates, pasture days, and demand metrics for producers and management units.
# Generate summaries: Season duration, pasture days by producer/unit, and lead collars.

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
library(lubridate)
library(sf)
library(terra)
library(sp)

# Import data (placeholders for processed data files)
raw_data_path <- Sys.getenv("RAW_DATA_PATH", "data")
processed_data_path <- Sys.getenv("PROCESSED_DATA_PATH", "data/processed")
input_data <- read_csv2(file.path(processed_data_path, "processed_data.csv"))
output_path <- Sys.getenv("OUTPUT_PATH", "output")

# Select relevant columns and filter ZEC observations
clean_data <- input_data %>%
  filter(!ZEC == 0) %>%
  select(user_id, device_id, livestock, timestamp)

# Season Start (Entry) by User
entry_data <- clean_data %>%
  group_by(user_id) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_head() %>%
  rename(Start_Date = timestamp) %>%
  rename(Lead_Device_Start = device_id) %>%
  rename(Rank_Start = ranking)

# Season End (Exit) by User
exit_data <- clean_data %>%
  group_by(user_id) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_tail() %>%
  rename(End_Date = timestamp) %>%
  rename(Lead_Device_End = device_id) %>%
  rename(Rank_End = ranking)

# Combine Entry and Exit
season_union <- entry_data %>%
  left_join(exit_data, by = "user_id", keep = FALSE) %>%
  select(!c(Rank_Start, Rank_End))

# Calculate Season Duration
season_summary <- season_union %>%
  mutate(
    Season_Duration = as.period(interval(start = Start_Date, end = End_Date))
  ) %>%
  relocate(c(user_id, Season_Duration), .before = Lead_Device_Start)

# Pasture Days by User (Monthly and Annual)
pasture_days_month_user <- clean_data %>%
  group_by(user_id) %>%
  mutate(
    day = day(timestamp),
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(user_id, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_user <- pasture_days_month_user %>%
  group_by(user_id) %>%
  summarise(Total_Pasture_Days = sum(Pasture_Days_Month))

# Combine with Season Summary
season_summary <- season_summary %>%
  left_join(pasture_days_year_user) %>%
  relocate(
    c(Total_Pasture_Days, Season_Duration),
    .before = Lead_Device_Start
  ) %>%
  select(-livestock.x, -livestock.y)


# Pasture Days by Livestock Type (Grouped by User)
pasture_days_month_livestock <- clean_data %>%
  group_by(user_id, livestock) %>%
  mutate(
    day = day(timestamp),
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(user_id, livestock, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_livestock <- pasture_days_month_livestock %>%
  group_by(user_id, livestock) %>%
  summarise(
    Total_Pasture_Days_Livestock = sum(Pasture_Days_Month),
    .groups = "keep"
  )

# Pasture Days by Device (Grouped by User)
pasture_days_month_device <- clean_data %>%
  group_by(user_id, device_id) %>%
  mutate(
    day = day(timestamp),
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(user_id, device_id, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_device <- pasture_days_month_device %>%
  group_by(user_id, device_id) %>%
  summarise(
    Total_Pasture_Days_Device = sum(Pasture_Days_Month),
    .groups = "keep"
  )

# Import Reference Data (Placeholder)
reference_data <- read_csv2(file.path(processed_data_path, "lead_zec.csv"))

# Combine Demand Metrics
demand_summary <- reference_data %>%
  left_join(pasture_days_year_livestock,
            by = c("user_id", "livestock")) %>%
  left_join(pasture_days_year_device,
            by = c("user_id", "device_id")) %>%
  mutate(
    perc_ZEC = case_when(ZEC == 1 ~ 100, TRUE ~ 0),
    perc_ZEC = round(perc_ZEC, 0)
  ) %>%
  group_by(user_id) %>%
  arrange(desc(perc_ZEC), .by_group = TRUE)

# Import Traceability (Placeholder)
trace_data <- read_csv2(file.path(raw_data_path, "tabular/traceability_data.csv"))

# Select herd_size and herd_id
trace <- trace_data %>%
  select(device_id, herd_size, herd_id)
remove(trace_data)

# Combine with Traceability to produce demand by producer
demand_producer <- demand_summary %>%
  left_join(trace) %>%
  select(user_id,
         origin,
         livestock,
         Total_Pasture_Days_Livestock,
         device_id,
         N,
         ZEC,
         perc_ZEC,
         Total_Pasture_Days_Device,
         herd_size,
         herd_id)

# Season Start/End by Management Unit (UG)
unit_data <- read_csv2(
  file.path(processed_data_path, "ug_processed_data.csv")
) %>%
  select(device_id, livestock, UG, timestamp) %>%
  mutate(UG = str_replace(UG, "/ Unit", "- Unit")) %>%
  mutate(UG = str_replace(UG, "_", " - ")) %>%
  mutate(UG = str_replace(UG, "/", "-")) %>%
  mutate(UG = str_replace(UG, "No Unit", "No - Unit"))

# Entry by Management Unit
entry_unit <- unit_data %>%
  group_by(UG) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_head() %>%
  rename(Start_Date = timestamp) %>%
  rename(Lead_Device_Start = device_id) %>%
  select(Lead_Device_Start, UG, Start_Date)

# Exit by Management Unit
exit_unit <- unit_data %>%
  group_by(UG) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(ranking = dense_rank(timestamp)) %>%
  slice_tail() %>%
  rename(End_Date = timestamp) %>%
  rename(Lead_Device_End = device_id) %>%
  select(Lead_Device_End, UG, End_Date)

# Combine Entry and Exit for Units
unit_union <- entry_unit %>% left_join(exit_unit, keep = FALSE)

# Calculate Season Duration for Units
unit_season <- unit_union %>%
  mutate(
    Season_Duration = as.period(interval(start = Start_Date, end = End_Date))
  ) %>%
  relocate(
    c(UG, Season_Duration),
    .before = Lead_Device_Start
  )

# Pasture Days by Management Unit
pasture_days_month_unit <- unit_data %>%
  group_by(UG) %>%
  mutate(
    day = day(timestamp),
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(UG, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_unit <- pasture_days_month_unit %>%
  group_by(UG) %>%
  summarise(
    Total_Pasture_Days = sum(Pasture_Days_Month),
    .groups = "keep"
  )

# Combine with Unit Season
unit_season <- unit_season %>%
  left_join(pasture_days_year_unit) %>%
  relocate(UG, Total_Pasture_Days, Season_Duration, .before = Lead_Device_Start)

# Pasture Days by Livestock Type per Unit
pasture_days_month_livestock_unit <- unit_data %>%
  group_by(UG, livestock) %>%
  mutate(
    day = day(timestamp),
    month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(UG, livestock, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_livestock_unit <- pasture_days_month_livestock_unit %>%
  group_by(UG, livestock) %>%
  summarise(
    Total_Pasture_Days_Livestock = sum(Pasture_Days_Month),
    .groups = "keep"
  )

# Pasture Days by Device per Unit
pasture_days_month_device_unit <- unit_data %>%
  mutate(
    Day = day(timestamp),
    Month = str_to_lower(month(timestamp, label = TRUE), locale = "en")
  ) %>%
  group_by(UG, device_id, month) %>%
  summarise(
    Pasture_Days_Month = n_distinct(day),
    .groups = "keep"
  )

pasture_days_year_device_unit <- pasture_days_month_device_unit %>%
  group_by(UG, device_id) %>%
  summarise(
    Total_Pasture_Days_Device = sum(Pasture_Days_Month),
    .groups = "keep"
  )

# Import Unit Reference Data (Placeholder)
unit_reference <- read_csv2(file.path(processed_data_path, "lead_ug.csv"))

# Combine Demand Metrics for Units
demand_unit <- unit_reference %>%
  left_join(pasture_days_year_livestock_unit, by = c("UG", "livestock")) %>%
  left_join(pasture_days_year_device_unit, by = c("UG", "device_id")) %>%
  left_join(trace) %>%
  mutate(
    Perc_Occupancy = case_when(
      ZEC == 1 ~ 100,
      TRUE ~ 0
    ),
    Perc_Occupancy = round(Perc_Occupancy, 0)
  ) %>%
  group_by(UG) %>%
  arrange(desc(Perc_Occupancy), .by_group = TRUE) %>%
  select(UG, livestock, Total_Pasture_Days_Livestock, device_id, ZEC,
         Perc_Occupancy,
         Total_Pasture_Days_Device,
         herd_size,
         herd_id)

# Export (generic output paths)
write_csv2(pasture_days_month_livestock_unit,
  file.path(output_path, "pasture_days_month_unit.csv")
)
write_csv2(season_summary,
  file.path(output_path, "season_producer.csv")
)
write_csv2(demand_producer,
  file.path(output_path, "demand_producer.csv")
)
write_csv2(unit_season, file.path(output_path, "season_unit.csv"))
write_csv2(demand_unit, file.path(output_path, "demand_unit.csv"))