# Modern R Suitability Analysis for Cape Town City Bowl
# By M.Nsofu, 19th October 2025
# ======================================================

# Load packages -----

library(osmdata)
library(terra)
library(sf)
library(tidyverse)
library(tmap)

# Define helper functions ----

normalize_01 <- function(raster) {
  min_val <- global(raster, "min", na.rm = TRUE) |> pull(min)
  max_val <- global(raster, "max", na.rm = TRUE) |> pull(max)
  
# Avoid division by zero for flat rasters
  
  if (max_val == min_val) {
    return(raster - min_val)
  }
  
  (raster - min_val) / (max_val - min_val)
}

# 1. Define Area of Interest (City Bowl) ----

aoi_bbox <- c(
  xmin = 18.40,
  ymin = -33.97,
  xmax = 18.47,
  ymax = -33.91
)

# 2. Download OSM data ----

message("Downloading road network...")

roads <- opq(bbox = aoi_bbox) |>
  add_osm_feature(
    key = "highway",
    value = c("primary", "secondary", "tertiary")
  ) |>
  osmdata_sf() |>
  pluck("osm_lines") |>
  st_transform(4326)

message("Downloading protected areas...")

protected <- opq(bbox = aoi_bbox) |>
  add_osm_feature(key = "leisure", value = "nature_reserve") |>
  osmdata_sf() |>
  pluck("osm_polygons") |>
  st_transform(4326)

# 3. Create synthetic DEM and calculate slope ----

message("Generating terrain data...")

dem <- rast(
  xmin = aoi_bbox["xmin"],
  xmax = aoi_bbox["xmax"],
  ymin = aoi_bbox["ymin"],
  ymax = aoi_bbox["ymax"],
  resolution = 0.001,
  crs = "EPSG:4326"
)

set.seed(42)
values(dem) <- 50 + 
  20 * sin(3 * xFromCell(dem, seq_len(ncell(dem)))) +
  15 * cos(3 * yFromCell(dem, seq_len(ncell(dem)))) +
  rnorm(ncell(dem), mean = 0, sd = 1)

slope <- terrain(dem, v = "slope", unit = "degrees")

# 4. Calculate distance to roads ----

if (nrow(roads) == 0) {
  stop("No roads found in the specified area. Please check your AOI.")
}

message("Calculating distance to roads...")

roads_vect <- vect(roads)
roads_raster <- rasterize(roads_vect, dem, field = 1)
dist_roads <- distance(roads_raster)

# 5. Compute suitability scores ----

message("Computing suitability...")

slope_suitability <- 1 - normalize_01(slope)
road_suitability <- 1 - normalize_01(dist_roads)

suitability <- 0.6 * road_suitability + 0.4 * slope_suitability

# Mask protected areas if present

if (nrow(protected) > 0) {
  message("Masking protected areas...")
  suitability <- mask(suitability, vect(protected), inverse = TRUE)
}

# 6. Visualize results with tmap----

message("Creating visualization...")

# Set tmap mode (plot for static, view for interactive)

tmap_mode("plot")

suitability_map <- tm_shape(suitability) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "viridis"),
    col.legend = tm_legend(title = "Suitability\nIndex"),
    col_alpha = 1
  ) +
  tm_shape(roads) +
  tm_lines(
    col = "black",
    lwd = 1.5,
    col_alpha = 0.7
  ) +
  tm_title(
    text = "Cape Town City Bowl Suitability Map",
    size = 1.2,
    fontface = "bold"
  ) +
  tm_credits(
    text = "Plotted by M.Nsofu, 19th October 2025",
    position = c("left", "bottom"),
    size = 0.9,
    fontface = "bold",
    col = "white"
   
  ) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("right", "top"),
    legend.title.size = 1,
    legend.text.size = 0.7,
    frame = TRUE
  ) +
  tm_grid(
    x = c(18.40, 18.43, 18.46),
    y = c(-33.97, -33.94, -33.91),
    alpha = 0.3,
    labels.size = 0.6
  )

# Add protected areas if present

if (nrow(protected) > 0) {
  suitability_map <- suitability_map +
    tm_shape(protected) +
    tm_borders(
      col = "darkred",
      lwd = 2,
      fill_alpha = 0.8
    )
}

# Display the map

print(suitability_map)

# Save the map as high-quality PNG ----

tmap_save(
  tm = suitability_map,
  filename = "cape_town_suitability_map.png",
  width = 3000,      # Width in pixels
  height = 2400,     # Height in pixels
  dpi = 300          # High resolution for publication
)

message("Map saved as 'cape_town_suitability_map.png'")

