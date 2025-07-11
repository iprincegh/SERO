#!/usr/bin/env Rscript

# SERO Heatmap Parameter Examples
# This script demonstrates key parameter customizations

library(SERO)

# Load data
cat("Loading SERO data...\n")
data <- sero_load_data()
cat("Loaded", nrow(data$accident), "accidents\n\n")

# Example 1: Change risk categories
cat("Example 1: Different risk categories\n")
cat("- Fatal accidents only (risk_categories = c(1))\n")
heatmap_fatal <- sero_heatmap(data$accident, risk_categories = c(1))

cat("- Fatal and serious accidents (risk_categories = c(1,2)) - DEFAULT\n")
heatmap_severe <- sero_heatmap(data$accident, risk_categories = c(1, 2))

cat("- All accident categories (risk_categories = c(1,2,3))\n")
heatmap_all <- sero_heatmap(data$accident, risk_categories = c(1, 2, 3))

# Example 2: Change bandwidth (smoothness)
cat("\nExample 2: Different bandwidth settings\n")
cat("- Fine detail (bandwidth = 300)\n")
heatmap_fine <- sero_heatmap(data$accident, bandwidth = 300)

cat("- Medium smoothing (bandwidth = 1000) - DEFAULT\n")
heatmap_medium <- sero_heatmap(data$accident, bandwidth = 1000)

cat("- Broad overview (bandwidth = 2000)\n")
heatmap_broad <- sero_heatmap(data$accident, bandwidth = 2000)

# Example 3: Change grid size (resolution)
cat("\nExample 3: Different grid sizes\n")
cat("- High resolution (grid_size = 50)\n")
heatmap_hires <- sero_heatmap(data$accident, grid_size = 50)

cat("- Standard resolution (grid_size = 100) - DEFAULT\n")
heatmap_standard <- sero_heatmap(data$accident, grid_size = 100)

cat("- Low resolution (grid_size = 200)\n")
heatmap_lowres <- sero_heatmap(data$accident, grid_size = 200)

# Example 4: Change color schemes
cat("\nExample 4: Different color schemes\n")
color_schemes <- c("viridis", "plasma", "inferno", "magma")
for (scheme in color_schemes) {
  cat(sprintf("- %s color scheme\n", scheme))
  heatmap_color <- sero_heatmap(data$accident, color_scheme = scheme)
}

# Example 5: Using presets
cat("\nExample 5: Using presets\n")
presets <- c("urban", "regional", "overview", "detailed", "presentation")
for (preset in presets) {
  cat(sprintf("- %s preset\n", preset))
  params <- sero_heatmap_presets(preset)
  heatmap_preset <- do.call(sero_heatmap, c(list(data$accident), params))
}

# Example 6: Custom combinations
cat("\nExample 6: Custom parameter combinations\n")

cat("- Urban analysis: fatal accidents, fine detail, inferno colors\n")
heatmap_urban <- sero_heatmap(
  data$accident,
  risk_categories = c(1),
  bandwidth = 400,
  grid_size = 50,
  color_scheme = "inferno",
  show_accidents = TRUE
)

cat("- Regional overview: all accidents, broad smoothing, clean visualization\n")
heatmap_regional <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2, 3),
  bandwidth = 1500,
  grid_size = 150,
  show_accidents = FALSE,
  color_scheme = "viridis"
)

cat("- Planning analysis: with land use context\n")
heatmap_planning <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 800,
  grid_size = 75,
  show_landuse = TRUE,
  color_scheme = "plasma"
)

cat("\n✓ All parameter examples completed successfully!\n")
cat("\nKey takeaways:\n")
cat("- risk_categories: Filter by accident severity (1=fatal, 2=serious, 3=slight)\n")
cat("- bandwidth: Control smoothness (smaller = more detail, larger = smoother)\n")
cat("- grid_size: Control resolution (smaller = higher resolution, larger = faster)\n")
cat("- color_scheme: Choose from viridis, plasma, inferno, magma\n")
cat("- Use presets for common scenarios: urban, regional, overview, detailed, presentation\n")
cat("- Combine parameters for custom analysis needs\n")
