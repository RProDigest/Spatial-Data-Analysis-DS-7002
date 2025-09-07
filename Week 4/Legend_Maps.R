# ------------------------------------------------------------
# Legend sketches for 7 thematic map types
# (choropleth, proportional, graduated, isoline, dot density,
#  dasymetric, flow) — ggplot2 + patchwork
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

# Helper: minimalist canvas
blank_theme <- theme_void(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.margin = margin(6, 6, 6, 6)
  )

# Helper: fixed coords
make_canvas <- function(xlim = c(0, 10), ylim = c(0, 4)) {
  ggplot() + coord_equal(xlim = xlim, ylim = ylim, expand = FALSE)
}

# Okabe–Ito style ramp (light -> dark)
oi_seq <- c("#E8F4FA", "#CBE7F3", "#ABD9EC", "#76B7E3", "#4BA3D1", "#2E7FB6", "#175E8C")

# 1) Choropleth legend (classed ramp)
choropleth_legend <- make_canvas() +
  geom_tile(
    data = tibble(x = 1:7, y = 2, fill = factor(1:7)),
    aes(x, y, fill = fill),
    width = 1, height = 1.2, color = "grey90", linewidth = 0.3
  ) +
  scale_fill_manual(values = oi_seq, guide = "none") +
  annotate("text", x = 1:7, y = 0.9,
           label = c("0–2%", "2–4%", "4–6%", "6–8%", "8–10%", "10–12%", "12–14%"),
           size = 3) +
  labs(title = "Choropleth", subtitle = "Color classes (light = low, dark = high)") +
  blank_theme

# 2) Proportional symbol legend (unclassed; area ∝ value)
prop_vals <- c(2, 4, 6, 8)
prop_sizes <- scales::rescale(prop_vals, to = c(4, 16))
proportional_legend <- make_canvas() +
  geom_point(
    data = tibble(x = 2 + (0:(length(prop_vals)-1))*2.2, y = 2, v = prop_vals, s = prop_sizes),
    aes(x, y, size = s),
    shape = 21, fill = "#D55E00", color = "white", alpha = 0.9, stroke = 0.4
  ) +
  scale_size_identity() +
  annotate("text",
           x = 2 + (0:(length(prop_vals)-1))*2.2, y = 0.8,
           label = paste0(prop_vals, "%"), size = 3) +
  annotate("text", x = 8.8, y = 3.4, label = "Circle area ∝ value", hjust = 1, size = 3.2) +
  labs(title = "Proportional Symbols", subtitle = "Continuous scaling (exact values)") +
  blank_theme

# 3) Graduated symbol legend (classed sizes)
grad_breaks <- c("0–2%", "2–4%", "4–6%", "6–8%", "8–10%")
grad_sizes <- seq(4, 16, length.out = length(grad_breaks))
graduated_legend <- make_canvas() +
  geom_point(
    data = tibble(x = 2 + (0:(length(grad_breaks)-1))*1.6, y = 2,
                  lab = grad_breaks, s = grad_sizes),
    aes(x, y, size = s),
    shape = 21, fill = "#E69F00", color = "white", alpha = 0.9, stroke = 0.4
  ) +
  scale_size_identity() +
  annotate("text",
           x = 2 + (0:(length(grad_breaks)-1))*1.6, y = 0.8,
           label = grad_breaks, size = 3) +
  labs(title = "Graduated Symbols", subtitle = "Classed sizes (ranges)") +
  blank_theme

# 4) Isoline legend (contours with labels)
iso_vals <- c(100, 200, 300, 400)
isoline_legend <- make_canvas(ylim = c(0,5)) +
  geom_segment(
    data = tibble(y = 1:4, v = iso_vals),
    aes(x = 1, xend = 9, y = y, yend = y),
    linewidth = 0.9, color = "grey20"
  ) +
  annotate("label",
           x = 9.2, y = 1:4,
           label = iso_vals, size = 3, label.size = 0, hjust = 0) +
  labs(title = "Isolines", subtitle = "Equal-value lines (intervals labelled)") +
  blank_theme

# 5) Dot density legend (1 dot = X units)
dot_rect <- tibble(xmin = 1, xmax = 9, ymin = 0.8, ymax = 3.2)
set.seed(42)
dots <- tibble(
  x = runif(40, 1.2, 8.8),
  y = runif(40, 1.0, 3.0)
)
dot_density_legend <- make_canvas(ylim = c(0,4)) +
  geom_rect(data = dot_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", color = "grey70") +
  geom_point(data = dots, aes(x, y), size = 1.5, color = "black") +
  annotate("text", x = 1, y = 3.6, label = "Each dot = 1,000 people", hjust = 0, size = 3.2) +
  labs(title = "Dot Density", subtitle = "Sample area with dot value note") +
  blank_theme

# 6) Dasymetric legend (classed fill + ancillary mask)
# Base classes (like choropleth)
das_classes <- tibble(
  x = 1:4,
  lab = c("0–2%", "2–4%", "4–6%", "6–8%"),
  fill = oi_seq[c(1,3,5,7)]
)
# Ancillary mask (e.g., land use) overlaid as hatch bands
hatch <- tibble(
  x = rep(1:4, each = 6),
  y = rep(seq(1.2, 2.8, length.out = 6), times = 4)
)
dasymetric_legend <- make_canvas(xlim = c(0, 10)) +
  geom_tile(data = das_classes, aes(x = x*2, y = 2, fill = lab),
            width = 1.6, height = 1.2, color = "grey90") +
  scale_fill_manual(values = setNames(das_classes$fill, das_classes$lab), guide = "none") +
  geom_segment(
    data = hatch,
    aes(x = (x*2 - 0.7), xend = (x*2 + 0.7), y = y, yend = y),
    color = "white", linewidth = 0.5, alpha = 0.8
  ) +
  annotate("text", x = das_classes$x*2, y = 0.9, label = das_classes$lab, size = 3) +
  annotate("text", x = 9.6, y = 3.3,
           label = "Boundaries refined\nby ancillary data",
           hjust = 1, vjust = 1, size = 3.1) +
  labs(title = "Dasymetric", subtitle = "Refined zones using ancillary data") +
  blank_theme

# 7) Flow legend (arrows with varying thickness)
flow_vals <- c(100, 500, 1000)
flow_df <- tibble(
  y = c(1, 2.2, 3.4),
  w = scales::rescale(flow_vals, to = c(0.6, 2.0))
)
flow_legend <- make_canvas(ylim = c(0,4)) +
  geom_segment(
    data = flow_df,
    aes(x = 1.2, xend = 8.5, y = y, yend = y),
    linewidth = flow_df$w,
    color = "#2E7FB6",
    arrow = arrow(type = "closed", length = unit(0.18, "in"))
  ) +
  annotate("text", x = 8.8, y = flow_df$y,
           label = paste(flow_vals, "units"),
           hjust = 0, size = 3) +
  labs(title = "Flow", subtitle = "Arrow width ∝ magnitude") +
  blank_theme

# Preview: arrange all legends
legend_grid <- (
  choropleth_legend | proportional_legend | graduated_legend
) /
  (isoline_legend     | dot_density_legend | dasymetric_legend) /
  (flow_legend + plot_spacer() + plot_spacer())

legend_grid
 ggsave("legend_sketches_all.png", legend_grid, width = 11, height = 8.5, dpi = 600)
