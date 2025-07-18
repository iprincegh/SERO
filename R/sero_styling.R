# SERO Styling and Color Schemes
# Centralized styling constants for consistent theming across all visualizations

#' SERO Color Palette
#' 
#' Standardized color scheme for SERO package visualizations
#' @export
SERO_COLORS <- list(
  # Basemap colors
  district_fill = "#f0f0f0",
  district_border = "#808080",
  road_color = "#ffffff",
  
  # Density colors (viridis palette)
  density_low = "#440154",
  density_mid = "#21908c", 
  density_high = "#fde725",
  
  # Emergency response colors
  existing_station = "#2E8B57",  # Sea Green
  new_station = "#DC143C",       # Crimson
  hotspot = "#FF4500",           # Orange Red
  accident_low = "#FFA500",      # Orange
  accident_high = "#FF0000",     # Red
  
  # Transparency levels
  alpha_basemap = 0.8,
  alpha_density = 0.7,
  alpha_accident = 0.6,
  alpha_hotspot = 0.5
)

#' Get SERO Theme
#' 
#' Returns standardized ggplot2 theme for SERO visualizations
#' @export
get_sero_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )
}

#' Get SERO Color Scale
#' 
#' Returns standardized color scale for density visualizations
#' @param type character, type of scale ("density", "accidents", "hotspots")
#' @param alpha numeric, transparency level (0-1)
#' @export
get_sero_scale <- function(type = "density", alpha = 0.7) {
  switch(type,
    "density" = ggplot2::scale_fill_viridis_c(
      name = "Density", 
      na.value = "transparent", 
      alpha = alpha,
      option = "plasma",
      trans = "sqrt"
    ),
    "accidents" = ggplot2::scale_color_gradient(
      low = SERO_COLORS$accident_low,
      high = SERO_COLORS$accident_high,
      na.value = "transparent"
    ),
    "hotspots" = ggplot2::scale_fill_manual(
      values = c("hotspot" = SERO_COLORS$hotspot),
      na.value = "transparent"
    )
  )
}
