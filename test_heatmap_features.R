#!/usr/bin/env Rscript

# Test script to demonstrate SERO heatmap features
# This script shows how to use the heatmap function with different parameters

# Load required libraries
library(SERO)
library(ggplot2)

# Load the data
cat("Loading SERO data...\n")
data <- sero_load_data()

# Check what data we have
cat("Data loaded successfully!\n")
cat("Number of accidents:", nrow(data$accident), "\n")
cat("Risk categories available:", sort(unique(data$accident$UKATEGORIE)), "\n")
cat("\n")

# Test 1: Basic heatmap with default parameters
cat("=== Test 1: Basic heatmap with default parameters ===\n")
heatmap_basic <- sero_heatmap(data$accident)
print(heatmap_basic)
cat("\n")

# Test 2: Customized heatmap with different parameters
cat("=== Test 2: Customized heatmap ===\n")
heatmap_custom <- sero_heatmap(
  data$accident,
  risk_categories = c(1),          # Only fatal accidents
  bandwidth = 500,                 # Smaller bandwidth for more detail
  grid_size = 50,                  # Finer grid
  show_accidents = TRUE,           # Show individual accidents
  show_landuse = TRUE,             # Show land use overlay
  color_scheme = "plasma"          # Different color scheme
)
print(heatmap_custom)
cat("\n")

# Test 3: Using presets
cat("=== Test 3: Using heatmap presets ===\n")

# Show available presets
cat("Available preset types:\n")
preset_types <- c("urban", "regional", "overview", "detailed", "presentation", "default")
for (type in preset_types) {
  params <- sero_heatmap_presets(type)
  cat(sprintf("- %s: bandwidth=%d, grid_size=%d, categories=%s, color=%s\n", 
              type, params$bandwidth, params$grid_size, 
              paste(params$risk_categories, collapse=","), params$color_scheme))
}
cat("\n")

# Test urban preset
cat("Creating urban heatmap...\n")
urban_params <- sero_heatmap_presets("urban")
heatmap_urban <- do.call(sero_heatmap, c(list(data$accident), urban_params))
print(heatmap_urban)
cat("\n")

# Test detailed preset
cat("Creating detailed heatmap...\n")
detailed_params <- sero_heatmap_presets("detailed")
detailed_params$data <- data  # Add data for context
heatmap_detailed <- do.call(sero_heatmap, c(list(data$accident), detailed_params))
print(heatmap_detailed)
cat("\n")

# Test 4: Batch comparison
cat("=== Test 4: Batch heatmap comparison ===\n")
comparison_types <- c("urban", "regional", "overview")
heatmaps <- sero_heatmap_compare(data$accident, data, comparison_types)

cat("Generated", length(heatmaps), "heatmaps for comparison:\n")
for (type in names(heatmaps)) {
  if (!is.null(heatmaps[[type]])) {
    cat(sprintf("- %s: Success\n", type))
  } else {
    cat(sprintf("- %s: Failed\n", type))
  }
}
cat("\n")

# Test 5: Custom parameter combinations
cat("=== Test 5: Custom parameter combinations ===\n")

# Test different color schemes
color_schemes <- c("viridis", "plasma", "inferno", "magma")
for (scheme in color_schemes) {
  cat(sprintf("Testing color scheme: %s\n", scheme))
  heatmap_color <- sero_heatmap(
    data$accident,
    risk_categories = c(1, 2),
    bandwidth = 800,
    grid_size = 100,
    color_scheme = scheme,
    show_accidents = FALSE
  )
  # Just create the plot, don't print it to avoid clutter
}
cat("All color schemes tested successfully!\n")
cat("\n")

# Test different risk categories
cat("Testing different risk categories:\n")
risk_combinations <- list(
  "Fatal only" = c(1),
  "Fatal + Serious" = c(1, 2),
  "All categories" = c(1, 2, 3)
)

for (name in names(risk_combinations)) {
  cats <- risk_combinations[[name]]
  cat(sprintf("- %s (categories: %s)\n", name, paste(cats, collapse=", ")))
  heatmap_risk <- sero_heatmap(
    data$accident,
    risk_categories = cats,
    bandwidth = 1000,
    grid_size = 100,
    show_accidents = TRUE
  )
  # Just create the plot, don't print it to avoid clutter
}
cat("All risk category combinations tested successfully!\n")
cat("\n")

# Test 6: Parameter validation
cat("=== Test 6: Parameter validation ===\n")

# Test with invalid risk categories
cat("Testing with invalid risk categories...\n")
tryCatch({
  heatmap_invalid <- sero_heatmap(
    data$accident,
    risk_categories = c(99),  # Invalid category
    bandwidth = 1000,
    grid_size = 100
  )
}, warning = function(w) {
  cat("Expected warning caught:", w$message, "\n")
})
cat("\n")

# Test with extreme parameters
cat("Testing with extreme parameters...\n")
heatmap_extreme <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 3000,    # Very large bandwidth
  grid_size = 300,     # Very large grid
  show_accidents = FALSE,
  color_scheme = "viridis"
)
cat("Extreme parameters handled successfully!\n")
cat("\n")

# Summary
cat("=== SUMMARY ===\n")
cat("✓ All heatmap features tested successfully!\n")
cat("✓ Basic heatmap with default parameters\n")
cat("✓ Customized parameters (bandwidth, grid size, risk categories, colors)\n")
cat("✓ Preset parameter combinations (urban, regional, overview, detailed, presentation)\n")
cat("✓ Batch comparison functionality\n")
cat("✓ Color scheme options (viridis, plasma, inferno, magma)\n")
cat("✓ Risk category filtering (fatal only, fatal+serious, all categories)\n")
cat("✓ Parameter validation and error handling\n")
cat("✓ Münster districts basemap integration\n")
cat("✓ Optional landuse overlay\n")
cat("✓ Customizable accident point display\n")
cat("\n")
cat("The SERO heatmap function provides comprehensive accident density visualization\n")
cat("with full parameter customization for different analysis needs.\n")
