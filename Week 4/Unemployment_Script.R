# Load required libraries
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(scales)
library(extrafont)
library(ggspatial)


# Custom Okabe-Ito inspired gradient----
okabe_gradient <- c(
  "#CCE7F0",  # Very light blue
  "#99D8EA",  # Light blue
  "#66C2E3",  # Medium light blue
  "#E69F00",  # Okabe-Ito Orange
  "#D55E00",  # Okabe-Ito Vermillion
  "#B8460F",  # Darker red-orange
  "#8B2635"   # Dark red
)

# Function to safely get eurostat data----
get_unemployment_data <- function() {
  datasets <- c("lfst_r_lfu3rt", "lfst_r_lfu3pers", "une_rt_a")
  for (dataset in datasets) {
    tryCatch({
      unemployment_data <- get_eurostat(dataset, time_format = "num", cache = TRUE)
      if (!is.null(unemployment_data) && nrow(unemployment_data) > 0) return(unemployment_data)
    }, error = function(e) message("Failed to load ", dataset, ": ", e$message))
  }
  stop("Could not load any unemployment dataset")
}

# Get and process data----
unemployment_data <- get_unemployment_data()
time_col <- names(unemployment_data)[grepl("time|year|period", names(unemployment_data), ignore.case = TRUE)][1]
latest_year <- max(unemployment_data[[time_col]], na.rm = TRUE)

# Smart filtering function----
filter_unemployment_data <- function(data, latest_year, time_col) {
  filtered_data <- data %>% filter(!!sym(time_col) == latest_year)
  if ("sex" %in% names(filtered_data)) filtered_data <- filter(filtered_data, sex %in% c("T", "TOTAL"))
  if ("age" %in% names(filtered_data)) {
    target_age <- intersect(c("Y15-74", "TOTAL", "Y_GE15"), unique(filtered_data$age))[1]
    if (!is.na(target_age)) filtered_data <- filter(filtered_data, age == target_age)
  }
  if ("unit" %in% names(filtered_data)) {
    target_unit <- intersect(c("PC", "PERCENT", "RT"), unique(filtered_data$unit))[1]
    if (!is.na(target_unit)) filtered_data <- filter(filtered_data, unit == target_unit)
  }
  geo_col <- intersect(c("geo", "GEO", "NUTS"), names(filtered_data))[1]
  value_col <- intersect(c("values", "value", "VALUE", "OBS_VALUE"), names(filtered_data))[1]
  filtered_data %>%
    select(all_of(c(geo_col, value_col))) %>%
    rename(geo = all_of(geo_col), unemployment_rate = all_of(value_col)) %>%
    filter(!is.na(unemployment_rate))
}

# Process data----
unemployment_filtered <- filter_unemployment_data(unemployment_data, latest_year, time_col)

# Get geographical data----
nuts_geo <- get_eurostat_geospatial(resolution = 20, nuts_level = 2, year = 2021)

# Select Poland and merge data----
selected_country <- "PL"
country_geo <- nuts_geo %>% filter(substr(NUTS_ID, 1, 2) == selected_country)
map_data <- country_geo %>% left_join(unemployment_filtered, by = c("NUTS_ID" = "geo"))

# Calculate statistics for legend breaks----
unemployment_stats <- na.omit(map_data$unemployment_rate)
custom_breaks <- round(quantile(unemployment_stats, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 1)

# MAP CODE -----
professional_map <- ggplot(map_data) +
  # Main choropleth layer (using original continuous data)
  geom_sf(aes(fill = unemployment_rate), color = "white", size = 0.2, alpha = 0.9) +
  
  # Restored original color scale with your okabe_gradient
  scale_fill_gradientn(
    name = "Unemployment Rate (%)",
    colors = okabe_gradient,
    na.value = "#F5F5F5",
    breaks = custom_breaks,
    labels = function(x) paste0(x, "%"),
    limits = c(min(unemployment_stats), max(unemployment_stats)),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = 30, barheight = 1.8,
      frame.colour = "#2C3E50", frame.linewidth = 0.8,
      ticks.colour = "#2C3E50", ticks.linewidth = 0.5,
      direction = "horizontal", label.position = "bottom"
    )
  ) +
  
  # Scale bar and a simplified North Arrow for a cleaner look-----
annotation_scale(location = "bl", width_hint = 0.4, style = "bar",
                 pad_x = unit(0.7, "in"), pad_y = unit(0.4, "in"),
                 text_family = "sans", text_col = "#2C3E50") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.0, "cm"), width = unit(0.8, "cm"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal(line_col = "#2C3E50")) +
  
  # Theme with left-aligned text----
theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(t = 20, b = 10, l = 50),
                              family = "sans", color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 25, l = 50),
                                 family = "sans", color = "#34495E"),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 15, l = 50),
                                family = "sans", color = "#7F8C8D", lineheight = 1.2),
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(t = 30, b = 10),
    plot.margin = margin(t = 30, r = 50, b = 40, l = 50),
    panel.border = element_rect(color = "#BDC3C7", fill = NA, linewidth = 0.5)
  ) +
  
  # Labels
  labs(
    title = "Unemployment Rate by NUTS 2 Regions",
    subtitle = paste("Poland •", latest_year),
    caption = paste(
      "Data Source: Eurostat (", latest_year, ")\n",
      "NUTS: Nomenclature of Territorial Units for Statistics\n",
      "Plotted by M.Nsofu | Twitter: @RProDigest",
      sep = ""
    )
  )

# Display and save the map
print(professional_map)
ggsave("professional_unemployment_poland_corrected.png", plot = professional_map,
       width = 12, height = 14, dpi = 300, bg = "white")



# --- SYMBOL MAPS (Proportional vs Graduated) -------------------------------



# 1) Centroids for symbols (keeps points inside polygons)----
centroids <- st_point_on_surface(map_data) |> 
  st_as_sf() |> 
  select(NUTS_ID, unemployment_rate, geometry)

# Common background (neutral polygons)
bg_poly <- ggplot(map_data) +
  geom_sf(fill = "#F2F4F7", color = "white", size = 0.2)

# Helpful ticks for legends
rate_vals <- na.omit(centroids$unemployment_rate)
prop_breaks <- pretty(rate_vals, n = 4)                 # nice rounded ticks
prop_limits <- range(rate_vals, na.rm = TRUE)

# --- A) PROPORTIONAL SYMBOL MAP (continuous) -------------------------------
# Area of circles is proportional to the value (perception-friendly).
proportional_map <- bg_poly +
  geom_sf(
    data = centroids,
    aes(size = unemployment_rate),
    shape = 21, fill = "#D55E00", color = "white", alpha = 0.85, stroke = 0.4
  ) +
  scale_size_area(
    name = "Unemployment rate (%)",
    max_size = 14, limits = prop_limits, breaks = prop_breaks,
    guide = guide_legend(
      title.position = "top", title.hjust = 0.5,
      override.aes = list(fill = "#D55E00", alpha = 0.85, color = "white"),
      keywidth = unit(1.2, "cm"), keyheight = unit(1.2, "cm")
    ),
    labels = function(x) paste0(x, "%")
  ) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(
    title = "Unemployment (Proportional Symbol Map)",
    subtitle = paste("Poland •", latest_year, "— circle AREA ∝ rate"),
    caption = "Source: Eurostat | Map: R (ggplot2, sf)"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
    plot.subtitle = element_text(size = 12, colour = "#34495E"),
    plot.caption = element_text(size = 9, colour = "#7F8C8D")
  )

# --- B) GRADUATED SYMBOL MAP (binned) --------------------------------------
# Discrete classes (e.g., quantiles) with a size per class.
n_classes <- 5
breaks_q <- quantile(rate_vals, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
labels_q <- paste0(
  sprintf("%.1f", head(breaks_q, -1)), "–", sprintf("%.1f", tail(breaks_q, -1)), "%"
)

centroids <- centroids |>
  mutate(class = cut(unemployment_rate, breaks = breaks_q, include.lowest = TRUE, labels = labels_q))

# map class → a set of sizes (small → large)
size_levels <- seq(4, 14, length.out = n_classes)

graduated_map <- bg_poly +
  geom_sf(
    data = centroids,
    aes(size = class),
    shape = 21, fill = "#E69F00", color = "white", alpha = 0.9, stroke = 0.4
  ) +
  scale_size_manual(
    name = "Unemployment rate (binned)",
    values = setNames(size_levels, levels(centroids$class)),
    guide = guide_legend(
      title.position = "top", title.hjust = 0.5,
      override.aes = list(fill = "#E69F00", alpha = 0.9, color = "white"),
      keywidth = unit(1.2, "cm"), keyheight = unit(1.2, "cm")
    )
  ) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(
    title = "Unemployment (Graduated Symbol Map)",
    subtitle = paste("Poland •", latest_year, "— circle SIZE by class (quantiles)"),
    caption = "Source: Eurostat | Map: R (ggplot2, sf)"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
    plot.subtitle = element_text(size = 12, colour = "#34495E"),
    plot.caption = element_text(size = 9, colour = "#7F8C8D")
  )

# Show and save
print(proportional_map)
ggsave("poland_unemployment_proportional_symbols.png", proportional_map, width = 10, height = 12, dpi = 300, bg = "white")

print(graduated_map)
ggsave("poland_unemployment_graduated_symbols.png", graduated_map, width = 10, height = 12, dpi = 300, bg = "white")

