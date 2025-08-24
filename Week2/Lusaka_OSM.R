################################################################################
# Name: Mubanga Nsofu                                                          #
# Institution: University of East London                                       #
# Date : 24th August  2025                                                     #
# Course: UEL-DS-7002 (Spatial Data Analytics)                                     #                  
# Program: Master of Data Science (MSc)                                        #
# Lecturer: Dr Mahmoud Elbattah                                                #
# Formative Assignment : Week 2                                                #
################################################################################

# 0.0 DESCRIPTION OF THE SCRIPT----------

# This script finds Buildings on parcels ≥ 1000 m² and > 250 m from highways 
# (OSM via osmdata) in Longacres, Lusaka, Zambia.

# 1.0 INSTALL THE NECESSARY PACKAGES----

if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(osmdata,
               tidyverse,
               sf,
               glue,
               units)


# 2.0 ---- Parameters ----
# Longacres, Lusaka bounding box (lon_min, lat_min, lon_max, lat_max)
longacres_bbox <- c(28.30, -15.44, 28.33, -15.40)

# Target CRS (meters) for Zambia: UTM Zone 35S
CRS_METERS <- 32735  # EPSG:32735

MIN_PARCEL_AREA_M2 <- set_units(1000, m^2) |> drop_units()  # 1000 m²
MIN_HWY_DIST_M     <- set_units(250, m)     |> drop_units()  # 250 m

# 3.0 ---- Utilities ----

# Robust binder for polygon + multipolygon layers from osmdata_sf()
combine_polys <- function(od) {
  polys  <- od$osm_polygons
  mpolys <- od$osm_multipolygons
  
  pieces <- list(polys, mpolys)
  pieces <- pieces[vapply(pieces, function(x) inherits(x, "sf") && nrow(x) > 0, logical(1))]
  if (!length(pieces)) return(NULL)
  
  # Superset of columns; add missing as NA; reorder consistently
  all_cols <- Reduce(union, lapply(pieces, names))
  std <- lapply(pieces, function(x) {
    missing <- setdiff(all_cols, names(x))
    for (m in missing) x[[m]] <- NA
    x <- x[, all_cols]
    x
  })
  
  out <- do.call(rbind, std)
  
  # Geometry clean-up
  out <- suppressWarnings(st_make_valid(out))
  out <- out[!st_is_empty(out), ]
  
  # Force polygonal type
  if (any(st_geometry_type(out) %in% c("POLYGON", "MULTIPOLYGON"))) {
    out <- st_cast(out, "MULTIPOLYGON", warn = FALSE)
  }
  out
}

# Safe transform
to_m <- function(x) st_transform(x, CRS_METERS)

# 4.0  BUILDING POLYGONS ----

message("Downloading buildings …")
buildings_q  <- opq(bbox = longacres_bbox) |> add_osm_feature(key = "building")
buildings_od <- osmdata_sf(buildings_q)
buildings_sf <- combine_polys(buildings_od)
if (is.null(buildings_sf) || nrow(buildings_sf) == 0) {
  stop("No building polygons found in the specified bounding box.")
}
buildings_sf <- buildings_sf |> mutate(building_id = dplyr::row_number())

# 5.0 PARCEL PROXY (landuse polygons) ----
# Note: OSM does not expose legal cadastre; we use landuse as a proxy.

message("Downloading landuse (parcel proxy) …")
landuse_q  <- opq(bbox = longacres_bbox) |> add_osm_feature(key = "landuse")
landuse_od <- osmdata_sf(landuse_q)
landuse_sf <- combine_polys(landuse_od)
if (is.null(landuse_sf) || nrow(landuse_sf) == 0) {
  stop("No landuse polygons found in the specified bounding box.")
}

# Parcel-like uses (tune as needed)
parcel_sf <- landuse_sf |>
  filter(!is.na(landuse)) |>
  filter(str_detect(landuse, "(?i)residential|commercial|retail|industrial|mixed|institutional|apartments|education|construction|public"))

if (nrow(parcel_sf) == 0) {
  stop("No landuse polygons matched the parcel-like filters. Relax the regex if needed.")
}

# 6.0 HIGHWAYS (major) ----

message("Downloading highways …")
highways_q  <- opq(bbox = longacres_bbox) |> add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary"))
highways_od <- osmdata_sf(highways_q)
highways_sf <- highways_od$osm_lines
if (is.null(highways_sf) || nrow(highways_sf) == 0) {
  warning("No major highways found in the bbox. Distance filter will remove all rows.")
  highways_sf <- st_sf(geometry = st_sfc(), crs = 4326)
}

# 7.0 Reproject to meters for area/distance ----

buildings_m <- buildings_sf |> to_m() |> st_make_valid()
parcels_m   <- parcel_sf    |> to_m() |> st_make_valid()
if (nrow(highways_sf)) {
  highways_m <- highways_sf |> to_m() |> st_make_valid()
  hw_union_m <- st_union(highways_m)  # faster distance
} else {
  highways_m <- NULL
  hw_union_m <- NULL
}

# 8.0 Keep buildings that fall within parcels ----
# If too strict (e.g., buildings slightly cross edges), use st_intersects + area fraction logic.

b_on_p <- st_join(
  buildings_m,
  parcels_m |> select(parcel_landuse = landuse),
  join = st_within,
  left = FALSE
)
if (nrow(b_on_p) == 0) {
  stop("No buildings fall entirely within the selected landuse polygons. Try st_intersects if needed.")
}

# 9.0 Parcel areas and filter ≥ 1000 m² ----

parcels_m <- parcels_m |>
  mutate(
    parcel_id      = dplyr::row_number(),
    parcel_area_m2 = as.numeric(st_area(geometry))
  )

big_parcels <- parcels_m |> filter(parcel_area_m2 >= MIN_PARCEL_AREA_M2)
if (nrow(big_parcels) == 0) {
  stop("No parcels ≥ 1000 m² in this bbox.")
}

# Re-join buildings to qualifying parcels
b_on_bigp <- st_join(
  buildings_m,
  big_parcels |> select(parcel_id, parcel_landuse = landuse, parcel_area_m2),
  join = st_within,
  left = FALSE
)
if (nrow(b_on_bigp) == 0) {
  stop("No buildings located on parcels ≥ 1000 m².")
}

# 10.0 Distance to highways > 250 m ----

if (!is.null(hw_union_m) && length(hw_union_m)) {
  dists_m <- as.numeric(st_distance(b_on_bigp, hw_union_m))
  result_m <- b_on_bigp |>
    mutate(dist_to_highway_m = dists_m) |>
    filter(dist_to_highway_m > MIN_HWY_DIST_M)
} else {
  # No highways present -> nothing qualifies against a distance threshold
  result_m <- b_on_bigp |>
    mutate(dist_to_highway_m = NA_real_) |>
    filter(FALSE)
}

message(glue("Total qualifying buildings: {nrow(result_m)}"))

# 11.0 Return/export ----

result_wgs84 <- st_transform(result_m, 4326)

# 12.0 Optional: write GeoPackage for GIS use-----

 st_write(result_wgs84,
          "longacres_buildings_ge_1000m2_gt_250m_from_highways.gpkg",
          layer = "qualified_buildings", delete_dsn = TRUE)


# 13.0 Plot and save visual  ----

png("longacres_buildings_map.png",
    width  = 9,       # width in inches
    height = 6,       # height in inches
    units  = "in",
    res    = 900)     # resolution (dots per inch)


if (interactive()) {
  plot(st_geometry(parcels_m),
       col  = adjustcolor("lightblue", 0.4),
       border = "blue",
       main = "Longacres — Buildings on parcels ≥1000 m² and >250 m from highways")
  
  if (!is.null(highways_m) && nrow(highways_m)) {
    plot(st_geometry(highways_m), col = "darkgreen", lwd = 2, add = TRUE)
  }
  
  plot(st_geometry(result_m), col = "red", pch = 16, cex = 0.8, add = TRUE)
  
  box()
  
  legend("topright",
         legend = c("Parcels (≥1000 m²)", "Highways", "Qualified Buildings"),
         fill   = c(adjustcolor("lightblue", 0.4), NA, "darkred"),
         border = c("blue", NA, "darkred"),
         lty    = c(NA, 1, NA),
         lwd    = c(NA, 2, NA),
         pch    = c(NA, NA, 16),
         col    = c("blue", "darkgreen", "darkred"),
         pt.cex = c(NA, NA, 0.8),
         bty    = "n")
}


# --- close device to write file ---
dev.off()

# 14.0 Final object----

result_wgs84
