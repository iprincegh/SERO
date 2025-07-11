# ==============================================================================
# SERO Package - Advanced Parameter Examples
# ==============================================================================
# This script demonstrates advanced parameter customization for the SERO package.
# Shows how to modify analysis parameters to customize:
# - Risk categories and accident filtering
# - Land use preferences
# - Spatial analysis parameters
# - Route calculation settings
# ==============================================================================

library(SERO)

# ==============================================================================
# 1. PARAMETER EXPLORATION - Understanding Available Options
# ==============================================================================

cat("=== SERO Package - Advanced Parameter Examples ===\n\n")

# Load data first
cat("Loading built-in spatial data...\n")
data <- sero_load_data()

# Explore accident categories
cat("Exploring accident categories (UKATEGORIE):\n")
accident_categories <- table(data$accident$UKATEGORIE)
print(accident_categories)
cat("1 = Fatal, 2 = Serious injury, 3 = Slight injury\n\n")

# Explore land use classes (if available)
if (!is.null(data$landuse) && "class" %in% names(data$landuse)) {
  cat("Available land use classes:\n")
  landuse_classes <- table(data$landuse$class)
  print(landuse_classes)
  cat("\n")
}

# ==============================================================================
# 2. CUSTOM RISK CATEGORIES
# ==============================================================================

cat("=== Example 1: Custom Risk Categories ===\n")

# Example 1a: Fatal accidents only
cat("\nAnalyzing fatal accidents only (UKATEGORIE = 1):\n")
hotspots_fatal <- sero_identify_hotspots(
  data$accident, 
  risk_categories = c(1),  # Fatal only
  buffer = 800,         # Smaller buffer for more precise hotspots
  min_events = 3           # Lower threshold for sparse fatal accidents
)
print(hotspots_fatal)

# Example 1b: All accident types
cat("\nAnalyzing all accident types (UKATEGORIE = 1, 2, 3):\n")
hotspots_all <- sero_identify_hotspots(
  data$accident, 
  risk_categories = c(1, 2, 3),  # All categories
  buffer = 1200,              # Larger buffer for broader analysis
  min_events = 10                # Higher threshold for all accidents
)
print(hotspots_all)

# ==============================================================================
# 3. CUSTOM LAND USE PREFERENCES
# ==============================================================================

cat("\n=== Example 2: Custom Land Use Preferences ===\n")

# Example 2a: Residential areas only
cat("\nOptimizing for residential areas only:\n")
locations_residential <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential"),  # Residential only
  min_road_distance = 300,              # Closer to roads for residential
  max_road_distance = 800,              # Smaller buffer
  grid_size = 50,                       # Finer grid for residential analysis
  max_locations = 8
)
print(locations_residential)

# Example 2b: Commercial and industrial focus
cat("\nOptimizing for commercial and industrial areas:\n")
locations_commercial <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("commercial", "industrial"),
  min_road_distance = 600,              # Further from roads (less congestion)
  max_road_distance = 1200,             # Larger buffer
  grid_size = 150,                      # Coarser grid for larger areas
  max_locations = 5
)
print(locations_commercial)

# ==============================================================================
# 4. SPATIAL ANALYSIS PARAMETERS
# ==============================================================================

cat("\n=== Example 3: Spatial Analysis Parameters ===\n")

# Example 3a: Fine-grained analysis
cat("\nFine-grained spatial analysis:\n")
locations_fine <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 200,              # Very close to roads
  max_road_distance = 600,              # Tight buffer
  grid_size = 50,                       # Fine grid (50m cells)
  max_locations = 15                    # More locations
)
print(locations_fine)

# Example 3b: Coarse-grained analysis
cat("\nCoarse-grained spatial analysis:\n")
locations_coarse <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 800,              # Further from roads
  max_road_distance = 1500,             # Large buffer
  grid_size = 200,                      # Coarse grid (200m cells)
  max_locations = 5                     # Fewer locations
)
print(locations_coarse)

# ==============================================================================
# 5. ROUTE CALCULATION PARAMETERS
# ==============================================================================

cat("\n=== Example 4: Route Calculation Parameters ===\n")

# Example 4a: Many short routes
cat("\nCalculating many short routes:\n")
routes_many <- sero_calculate_routes(
  locations_fine,
  data$accident,
  max_routes = 50,           # Many routes
  max_distance = 5000        # Only short routes (5km)
)
print(routes_many)

# Example 4b: Few long routes
cat("\nCalculating few long routes:\n")
routes_few <- sero_calculate_routes(
  locations_coarse,
  data$accident,
  max_routes = 10,           # Few routes
  max_distance = 15000       # Allow longer routes (15km)
)
print(routes_few)

# ==============================================================================
# 6. COMPLETE WORKFLOW WITH CUSTOM PARAMETERS
# ==============================================================================

cat("\n=== Example 5: Complete Custom Workflow ===\n")

# Scenario: Emergency services for urban core
cat("\nScenario: Emergency services optimized for urban core:\n")
urban_analysis <- sero_analyze(
  risk_categories = c(1, 2),                                    # Fatal and serious
  suitable_landuse = c("residential", "commercial"),            # Urban land use
  max_locations = 12,                                          # More locations
  max_routes = 30                                              # More routes
)
print(urban_analysis)

# Scenario: Emergency services for industrial areas
cat("\nScenario: Emergency services optimized for industrial areas:\n")
industrial_analysis <- sero_analyze(
  risk_categories = c(1, 2, 3),                               # All accident types
  suitable_landuse = c("industrial", "commercial"),           # Industrial focus
  max_locations = 6,                                          # Fewer locations
  max_routes = 15                                             # Fewer routes
)
print(industrial_analysis)

# ==============================================================================
# 7. VISUALIZATION WITH CUSTOM PARAMETERS
# ==============================================================================

cat("\n=== Creating Custom Visualizations ===\n")

# Plot comparison: Fatal vs All accidents
cat("Creating comparison visualization...\n")
plot_fatal <- plot(hotspots_fatal)
plot_all <- plot(hotspots_all)

# Plot locations with different parameters
plot_residential <- plot(locations_residential)
plot_commercial <- plot(locations_commercial)

# Plot routes with different parameters
plot_many_routes <- plot(routes_many)
plot_few_routes <- plot(routes_few)

# ==============================================================================
# 8. PARAMETER SENSITIVITY ANALYSIS
# ==============================================================================

cat("\n=== Parameter Sensitivity Analysis ===\n")

# Test different grid sizes
cat("Testing different grid sizes:\n")
grid_sizes <- c(50, 100, 150, 200)
grid_results <- list()

for (size in grid_sizes) {
  cat("Grid size:", size, "m\n")
  result <- sero_compute_optimal_locations(
    data,
    grid_size = size,
    max_locations = 10
  )
  grid_results[[paste0("grid_", size)]] <- result
  cat("  Locations found:", nrow(result$locations), "\n")
}

# Test different road distance buffers
cat("\nTesting different road distance buffers:\n")
road_distances <- list(
  "close" = c(200, 500),
  "medium" = c(500, 1000),
  "far" = c(800, 1500)
)

road_results <- list()
for (name in names(road_distances)) {
  distances <- road_distances[[name]]
  cat("Buffer:", name, "(", distances[1], "-", distances[2], "m)\n")
  result <- sero_compute_optimal_locations(
    data,
    min_road_distance = distances[1],
    max_road_distance = distances[2],
    max_locations = 10
  )
  road_results[[name]] <- result
  cat("  Locations found:", nrow(result$locations), "\n")
}

# ==============================================================================
# 9. SAVE CUSTOM RESULTS
# ==============================================================================

cat("\n=== Saving Custom Results (Optional) ===\n")

# Uncomment to save specific results
# cat("Saving fatal accident hotspots...\n")
# sf::st_write(hotspots_fatal$hotspots, "hotspots_fatal.gpkg", delete_dsn = TRUE)
# 
# cat("Saving residential locations...\n")
# sf::st_write(locations_residential$locations, "locations_residential.gpkg", delete_dsn = TRUE)
# 
# cat("Saving urban analysis results...\n")
# sf::st_write(urban_analysis$locations$locations, "locations_urban.gpkg", delete_dsn = TRUE)
# sf::st_write(urban_analysis$routes$routes, "routes_urban.gpkg", delete_dsn = TRUE)

cat("\n=== Advanced Parameter Examples Complete ===\n")
cat("This example demonstrated:\n")
cat("- Custom risk categories (fatal only, all types)\n")
cat("- Different land use preferences (residential, commercial, industrial)\n")
cat("- Spatial analysis parameters (grid size, road buffers)\n")
cat("- Route calculation settings (distance limits, route counts)\n")
cat("- Complete workflow customization\n")
cat("- Parameter sensitivity analysis\n")
