#### Exploratory Data Analysis ####

# Libraries
library(scales)
library(ggplot2)
library(sf)
library(tidyverse)

# Import Cleaned Data
processed_data_path <- Sys.getenv("PROCESSED_DATA_PATH", "data/output")
input_data <- read_csv2(file.path(processed_data_path, "processed_data.csv"))  # Placeholder for input data
output_path <- Sys.getenv("OUTPUT_PATH", "output")

## Total Observations by Livestock Type ##
bar_data <- input_data %>%
  mutate(livestock_type = case_when(
    livestock_type == "caprino" ~ "Caprine",
    livestock_type == "equino" ~ "Equine",
    livestock_type == "ovino" ~ "Ovine",
    livestock_type == "Sin_Gan" ~ "No_ID",
    livestock_type == "vacuno" ~ "Bovine"
  )) %>%
  filter(livestock_type != "No_ID") %>%
  group_by(livestock_type) %>%
  summarise(n = n())

p_bar <- ggplot(bar_data, aes(x = livestock_type, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "GPS Observations by Livestock Type",
    x = "Livestock",
    y = "Observations"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display
plot(p_bar)

# Save the Plot
ggsave(
  filename = file.path(output_path, "gps_obs_by_livestock_type.png"),
  plot = p_bar,
  width = 8,
  height = 6,
  dpi = 300
)

## ZEC Proportion of Observations by Livestock Type ##
pie_data <- input_data %>%
  mutate(
    zec_status = as.factor(ZEC),
    zec_status = if_else(ZEC == "1", "Inside", "Outside"),
    livestock_type = case_when(
      livestock_type == "caprino" ~ "Caprine",
      livestock_type == "equino" ~ "Equine",
      livestock_type == "ovino" ~ "Ovine",
      livestock_type == "Sin_Gan" ~ "No_ID",
      livestock_type == "vacuno" ~ "Bovine"
    )
  ) %>%
  filter(livestock_type != "No_ID") %>%
  group_by(livestock_type, zec_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(livestock_type) %>%
  mutate(prop = n / sum(n))

p_pie <- ggplot(pie_data, aes(x = "", y = prop, fill = zec_status)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = sprintf("%.1f%%", prop * 100)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ livestock_type, ncol = 2) +
  labs(
    title = "ZEC GPS Observations: Percentage Distribution by Livestock Type",
    fill = "ZEC"
  ) +
  scale_fill_manual(values = c("Inside" = "#66c2a5", "Outside" = "#ff9999")) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )

# Display
plot(p_pie)

# Save the Plot
ggsave(
  filename = file.path(output_path, "zec_proportion.png"),
  plot = p_pie,
  width = 8,
  height = 6,
  dpi = 300
)

# Import Management Units Grazing Seasons (Placeholder)
season_unit <- read_csv2(file.path(output_path, "season_unit.csv"))

# Import Management Units Monthly Grazing Days (Placeholder)
monthly_grazing_unit <- read_csv2(file.path(output_path, "pasture_days_month_unit.csv"))

## Total Grazing Days by Management Unit ##
total_graz <- ggplot(season_unit, aes(x = reorder(management_unit, Total_Pasture_Days), y = Total_Pasture_Days)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Total Grazing Days by Management Unit",
    x = "Management Unit",
    y = "Grazing Days"
  ) +
  scale_y_continuous(labels = label_number()) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display
plot(total_graz)

# Save the Plot
ggsave(
  filename = file.path(output_path, "total_grazing_days_by_ug.png"),
  plot = total_graz,
  width = 8,
  height = 6,
  dpi = 300
)

## Monthly ZEC Grazed Days by Livestock Type and Management Unit ##
monthly_grazing_unit <- monthly_grazing_unit %>%
  mutate(livestock_type = case_when(
    livestock_type == "caprino" ~ "Caprine",
    livestock_type == "equino" ~ "Equine",
    livestock_type == "ovino" ~ "Ovine",
    livestock_type == "Sin_Gan" ~ "No_ID",
    livestock_type == "vacuno" ~ "Bovine"
  )) %>%
  filter(livestock_type != "No_ID")

monthly_graz <- ggplot(monthly_grazing_unit, aes(x = Month, y = Pasture_Days_Month)) +
  geom_point(aes(color = livestock_type), size = 1) +
  geom_smooth(se = FALSE, aes(color = livestock_type)) +
  facet_wrap(~ management_unit) +
  labs(
    title = "Monthly ZEC Grazing Days by Livestock Type and Management Unit",
    x = "Month",
    y = "Grazed Days"
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(
    name = "Livestock",
    values = c("Bovine" = "#1f77b4", "Ovine" = "#ff7f0e", "Caprine" = "#2ca02c", "Equine" = "red")
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 0.001),
    legend.justification = c("right", "bottom"),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    color = guide_legend(ncol = 2, byrow = TRUE)
  )

# Display
plot(monthly_graz)

# Save the Plot
ggsave(
  filename = file.path(output_path, "monthly_grazing_days_by_ug.png"),
  plot = monthly_graz,
  width = 8,
  height = 6,
  dpi = 300
)

# Import Landscape Data (Placeholder)
landscape_data <- read_csv2(file.path(output_path, "landscape_data.csv"))

# Keep Observations in Specific Land Covers
filtered_data <- landscape_data %>%
  filter(land_cover_type %in% c("Rock Outcrops", "Shrublands", "Forest", "Herbaceous Meadow - Pasture", "Pasture - Scrubland", "Meadows")) %>%
  mutate(
    land_cover_type = case_when(
      land_cover_type %in% c("Forest") ~ "Forest",
      TRUE ~ land_cover_type
    ),
    livestock_type = case_when(
      livestock_type == "caprino" ~ "Caprine",
      livestock_type == "equino" ~ "Equine",
      livestock_type == "ovino" ~ "Ovine",
      livestock_type == "Sin_Gan" ~ "No_ID",
      livestock_type == "vacuno" ~ "Bovine"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    slope_category = factor(slope_category, levels = c("<30%", "30-50%", ">50%"))
  ) %>%
  filter(!is.na(livestock_type)) %>%
  filter(livestock_type != "No_ID")

## Altitude Distribution by Livestock Type and Season ##
filtered_data_clean <- filtered_data %>% filter(!is.na(altitude) & is.finite(altitude))

p_altitude <- ggplot(filtered_data_clean, aes(x = livestock_type, y = altitude, fill = livestock_type)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, fill = "white") +
  facet_grid(. ~ season, scales = "free_x") +
  labs(
    title = "Altitude Distribution of GPS Observations by Livestock Type and Season",
    y = "Altitude (m)",
    fill = "Livestock"
  ) +
  scale_fill_manual(
    values = c("Bovine" = "#1f77b4", "Ovine" = "#ff7f0e", "Caprine" = "#2ca02c", "Equine" = "red")
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(ncol = 4, byrow = TRUE, keywidth = unit(1.5, "cm"), keyspacing.x = unit(0.5, "cm"))
  )

# Display
plot(p_altitude)

# Save
ggsave(
  filename = file.path(output_path, "altitude_distribution_by_season.png"),
  plot = p_altitude,
  width = 8,
  height = 6,
  dpi = 300
)

## Slope Distribution by Livestock Type and Season ##
p_slope <- ggplot(filtered_data, aes(x = slope_category, y = after_stat(prop*100), group = 1)) +
  geom_bar(aes(fill = livestock_type)) +
  facet_grid(season ~ livestock_type) +
  labs(
    title = "Slope Distribution of GPS Observations by Livestock Type and Season",
    x = "Slope",
    y = "Observations (%)",
    fill = "Livestock"
  ) +
  scale_fill_manual(
    values = c("Bovine" = "#1f77b4", "Ovine" = "#ff7f0e", "Caprine" = "#2ca02c", "Equine" = "red")
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

# Display
plot(p_slope)

# Save
ggsave(
  filename = file.path(output_path, "slope_distribution_by_season.png"),
  plot = p_slope,
  width = 8,
  height = 6,
  dpi = 300
)

## Land Cover Distribution by Livestock Type ##
land_cover_summary <- filtered_data %>%
  group_by(livestock_type, land_cover_type) %>%
  summarise(count = n()) %>%
  group_by(livestock_type) %>%
  mutate(percentage = (count / sum(count)) * 100)

p_land_cover <- ggplot(land_cover_summary, aes(x = land_cover_type, y = percentage, fill = livestock_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ livestock_type, scales = "free_x") +
  labs(
    title = "Land Cover Distribution of GPS Observations by Livestock Type",
    x = "Cover Type",
    y = "Observations (%)"
  ) +
  scale_fill_manual(
    values = c("Bovine" = "#1f77b4", "Ovine" = "#ff7f0e", "Caprine" = "#2ca02c", "Equine" = "red")
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Display
plot(p_land_cover)

# Save
ggsave(
  filename = file.path(output_path, "land_cover_distribution_by_livestock.png"),
  plot = p_land_cover,
  width = 8,
  height = 6,
  dpi = 300
)

# Seasonal Grazing Intensity by Livestock Type
heatmap_data <- filtered_data %>%
  group_by(livestock_type, season) %>%
  summarise(n = n(), .groups = "drop")

# Note: Assuming 'heat_ggplot' is a typo; using ggplot instead
ggplot(heatmap_data, aes(x = season, y = livestock_type, fill = n)) +
  geom_tile() +
  geom_text(aes(label = scales::comma(n)), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = comma) +
  labs(
    title = "Grazing Intensity by Livestock Type and Season",
    x = "Season",
    y = "Livestock Type",
    fill = "Observations"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save
ggsave(
  filename = file.path(output_path, "grazing_intensity_heatmap.png"),
  width = 8,
  height = 6,
  dpi = 300
)

# Observations by Clearing Year
clear_data <- filtered_data %>%
  pivot_longer(cols = starts_with("clearing_year"), names_to = "Year", values_to = "Clearing") %>%
  filter(!is.na(Clearing)) %>%
  group_by(livestock_type, Year) %>%
  summarise(n = n(), .groups = "drop")

ggplot(clear_data, aes(x = Year, y = n, fill = livestock_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "GPS Observations by Clearing Year and Livestock Type",
    x = "Clearing Year",
    y = "Observations",
    fill = "Livestock"
  ) +
  scale_fill_manual(
    values = c("Bovine" = "#1f77b4", "Ovine" = "#ff7f0e", "Caprine" = "#2ca02c", "Equine" = "red")
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save
ggsave(
  filename = file.path(output_path, "clearings.png"),
  width = 10,
  height = 8,
  dpi = 300
)