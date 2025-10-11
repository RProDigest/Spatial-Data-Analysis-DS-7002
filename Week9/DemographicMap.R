# -----------------------------------------
# Eurostat + GISCO Neighbourhood Need Score
# -----------------------------------------
# Indicators (NUTS-3):
# 1) Population density  -> demo_r_d3dens
# 2) Unemployment rate   -> lfst_r_lfu3rt
# 3) AROPE (poverty risk)-> ilc_peps11 (NUTS-2, joined to NUTS-3)
#
# Output: composite need_score (0–1), top decile flagged as priority
# Created by M.Nsofu
# Date: 11th October 2025

# ---- Load libraries ---------

library(eurostat)   # Eurostat data
library(giscoR)     # EU boundaries
library(sf)
library(tidyverse)
library(tmap)

tmap_mode("plot")

# ---- Choose country (ISO2) ----

ISO2 <- "DE"   # e.g. Germany ("DE" Germany, "FR" France, etc.)
NUTS_YEAR <- 2021
NUTS_LEVEL <- 3  # NUTS 3 density data only available at this level

# ---- 1) Population density (NUTS-3) ----

dens <- get_eurostat("demo_r_d3dens", time_format = "num") |>
  rename(year = any_of(c("time", "TIME_PERIOD"))) |>
  filter(str_starts(geo, ISO2)) |>
  group_by(geo) |>
  filter(year == max(year, na.rm = TRUE)) |>
  ungroup() |>
  select(geo, dens = values)

# ---- 2) Unemployment rate (NUTS-3, age 15–74, total) ------

unemp <- get_eurostat("lfst_r_lfu3rt", time_format = "num") |>
  rename(year = any_of(c("time", "TIME_PERIOD"))) |>  
  filter(str_starts(geo, ISO2),
         sex == "T", age == "Y15-74", unit == "PC_ACT") |>
  group_by(geo) |>
  filter(year == max(year, na.rm = TRUE)) |>
  ungroup() |>
  select(geo, unemp = values)

# ---- 3) AROPE (people at risk of poverty or social exclusion, %) ------
# Note: AROPE data typically only available at NUTS 2, so we'll join later

arope <- get_eurostat("ilc_peps11", time_format = "num") |>
  rename(year = TIME_PERIOD) |>  # Correct column name
  filter(str_starts(geo, ISO2),
         unit == "PC") |>  # Only keep percentage values
  group_by(geo) |>
  filter(year == max(year, na.rm = TRUE)) |>
  ungroup() |>
  select(geo, arope = values)

# ---- 4) Boundaries (NUTS-3 polygons) -------

nuts3 <- gisco_get_nuts(year = NUTS_YEAR, epsg = 3035, resolution = "20",
                        nuts_level = NUTS_LEVEL) |>
  filter(CNTR_CODE == ISO2)

# ---- 5) Join & score ----

rank_pct <- function(x, reverse = FALSE) {
  r <- dplyr::percent_rank(x)
  if (reverse) 1 - r else r
}

# Create NUTS2 code from NUTS3 for joining AROPE data

dat <- nuts3 |>
  select(geo = NUTS_ID, NAME_LATN, geometry) |>
  left_join(dens,  by = "geo") |>
  left_join(unemp, by = "geo") |>
  # For AROPE: extract NUTS2 code (first 4 chars) and join
  mutate(geo_nuts2 = substr(geo, 1, 4)) |>
  left_join(arope, by = c("geo_nuts2" = "geo")) |>
  # Remove rows with all missing indicator data
  filter(!is.na(dens) | !is.na(unemp) | !is.na(arope))


# Compute composite score (only for regions with at least some data)

dat <- dat |>
  mutate(
    r_dens = rank_pct(dens),
    r_unem = rank_pct(unemp),
    r_arop = rank_pct(arope),
    # Calculate mean of available indicators
    need_score = rowMeans(cbind(r_dens, r_unem, r_arop), na.rm = TRUE),
    need_decile = ntile(need_score, 10),
    priority = need_decile == 10
  )


# ---- 6) Map ----

if (nrow(dat) == 0 || sum(!is.na(dat$need_score)) == 0) {
  cat("ERROR: No valid data to plot. Check if the datasets have data for", ISO2, "\n")
} else {
  # Create base map
  map_plot <- tm_shape(dat) +
    tm_polygons(
      fill = "need_score",
      fill.scale = tm_scale_continuous(
        values = c("#56B4E9", "#009E73", "#F0E442", "#E69F00", "#D55E00")
      ),
      fill.legend = tm_legend(title = "Need Score")
    ) +
    tm_borders(lwd = 0.3)
  
  # Add priority borders only if there are priority regions
  priority_regions <- dat |> filter(priority == TRUE)
  
  if (nrow(priority_regions) > 0) {
    cat("Adding", nrow(priority_regions), "priority regions to map\n")
    map_plot <- map_plot +
      tm_shape(priority_regions) +
      tm_borders(lwd = 1.4, col = "red")
  } else {
    cat("Note: No priority regions identified (may need more regions for decile calculation)\n")
  }
  
  # Add title and credits
  map_plot <- map_plot +
    tm_title(paste0("Regional Socio-Economic Need Index across Germany (NUTS-3, 2021): Priority Areas in the Top 10%", ISO2)) +
    tm_credits("Data: Eurostat/GISCO | Plotted By M.Nsofu", position = c("left", "bottom"))
  
  # Display the map
  print(map_plot)
}

