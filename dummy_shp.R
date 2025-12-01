# ========================================
# 🗺️ CREATE DUMMY SHAPEFILES FOR clean_III.R
# ========================================
library(sf)
library(terra)

# SET CRS (EPSG:25830 = UTM Zone 30N)
crs_25830 <- "EPSG:25830"

# 1. COUNTRY BOUNDARIES (1 big polygon = Spain)
country_poly <- data.frame(
  id = 1,
  name = "Spain"
)
country_coords <- rbind(
  c(400000, 4300000), c(450000, 4300000), c(450000, 4350000),
  c(400000, 4350000), c(400000, 4300000)
)
country_geom <- st_polygon(list(country_coords))
country_sf <- st_sfc(country_geom, crs = crs_25830) %>% 
  st_sf(geometry = ., data = country_poly)
write_sf(country_sf, "data/country_boundaries.shp")
cat("✅ country_boundaries.shp CREATED (1 polygon)\n")

# 2. ZEC BOUNDARIES (2 ZEC zones)
zec_poly <- data.frame(
  id = c(1, 2),
  zec_name = c("ZEC_001", "ZEC_002"),
  area_ha = c(5000, 3000)
)
zec_coords1 <- rbind(
  c(410000, 4310000), c(420000, 4310000), c(420000, 4315000),
  c(410000, 4315000), c(410000, 4310000)
)
zec_coords2 <- rbind(
  c(430000, 4320000), c(440000, 4320000), c(440000, 4325000),
  c(430000, 4325000), c(430000, 4320000)
)
zec_geom <- st_sfc(list(st_polygon(list(zec_coords1)), st_polygon(list(zec_coords2))), crs = crs_25830)
zec_sf <- st_sf(geometry = zec_geom, data = zec_poly)
write_sf(zec_sf, "data/zec_boundaries.shp")
cat("✅ zec_boundaries.shp CREATED (2 ZECs)\n")

# 3. MANAGEMENT UNITS (3 UGs)
ug_poly <- data.frame(
  id = c(1, 2, 3),
  management_unit = c("UG001", "UG002", "UG003"),
  area_ha = c(1200, 800, 1000)
)
ug_coords1 <- rbind(
  c(415000, 4312000), c(418000, 4312000), c(418000, 4314000),
  c(415000, 4314000), c(415000, 4312000)
)
ug_coords2 <- rbind(
  c(435000, 4322000), c(438000, 4322000), c(438000, 4324000),
  c(435000, 4324000), c(435000, 4322000)
)
ug_coords3 <- rbind(
  c(425000, 4318000), c(428000, 4318000), c(428000, 4320000),
  c(425000, 4320000), c(425000, 4318000)
)
ug_geom <- st_sfc(
  list(st_polygon(list(ug_coords1)), st_polygon(list(ug_coords2)), st_polygon(list(ug_coords3))), 
  crs = crs_25830
)
ug_sf <- st_sf(geometry = ug_geom, data = ug_poly)
write_sf(ug_sf, "data/management_units.shp")
cat("✅ management_units.shp CREATED (3 UGs)\n")

# FINAL VERIFICATION
cat("\n🎉 3 DUMMY SHAPEFILES READY FOR clean_III.R!\n")
cat("📁 data/ contains:\n")
list.files("data/", pattern = "\\.shp$", full.names = FALSE)