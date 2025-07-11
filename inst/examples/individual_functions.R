# ==============================================================================
# SERO Package - Individual Function Examples
# ==============================================================================
# This script demonstrates how to use each SERO function individually
# with detailed parameter explanations and calculation examples.
# Shows the step-by-step process of emergency response optimization.
# ==============================================================================

library(SERO)

# ==============================================================================
# 1. DATA LOADING AND VALIDATION
# ==============================================================================

cat("=== SERO Package - Individual Function Examples ===\n\n")

# Load and validate data
cat("=== Data Loading and Validation ===\n")

# Load built-in data
cat("Loading built-in spatial data...\n")
data <- sero_load_data()

# Validate data structure
cat("Validating data structure...\n")
validated_data <- sero_validate_data(data)
cat("Data validation complete!\n")

# Display data information
cat("\nData Summary:\n")
for (layer_name in names(data)) {
  layer <- data[[layer_name]]
  cat("-", layer_name, ":", nrow(layer), "features\n")
  if ("UKATEGORIE" %in% names(layer)) {
    cat("  Accident categories:", paste(unique(layer$UKATEGORIE), collapse = ", "), "\n")
  }
  if ("class" %in% names(layer)) {
    cat("  Land use classes:", paste(unique(layer$class), collapse = ", "), "\n")
  }
}

# ==============================================================================
# 2. HOTSPOT IDENTIFICATION - DETAILED EXAMPLES
# ==============================================================================

cat("\n=== Hotspot Identification Examples ===\n")

# Example 2.1: Basic hotspot identification
cat("\nExample 2.1: Basic hotspot identification\n")
cat("Parameters: Default settings\n")
hotspots_basic <- sero_identify_hotspots(data$accident)
print(hotspots_basic)

# Example 2.2: Custom bandwidth analysis
cat("\nExample 2.2: Custom bandwidth analysis\n")
cat("Parameters: bandwidth = 500m (smaller for precise hotspots)\n")
hotspots_precise <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 500,        # Smaller bandwidth = more precise hotspots
  min_events = 3          # Lower threshold for sparse areas
)
print(hotspots_precise)

# Example 2.3: Broader area analysis
cat("\nExample 2.3: Broader area analysis\n")
cat("Parameters: bandwidth = 1500m (larger for regional hotspots)\n")
hotspots_broad <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 1500,       # Larger bandwidth = broader hotspots
  min_events = 8          # Higher threshold for significance
)
print(hotspots_broad)

# Example 2.4: Fatal accidents only
cat("\nExample 2.4: Fatal accidents only\n")
cat("Parameters: risk_categories = c(1) (fatal accidents only)\n")
hotspots_fatal <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1), # Fatal accidents only
  bandwidth = 800,
  min_events = 2          # Lower threshold for rare events
)
print(hotspots_fatal)

# ==============================================================================
# 3. OPTIMAL LOCATION COMPUTATION - DETAILED EXAMPLES
# ==============================================================================

cat("\n=== Optimal Location Computation Examples ===\n")

# Example 3.1: Default multi-criteria analysis
cat("\nExample 3.1: Default multi-criteria analysis\n")
cat("Parameters: All default settings\n")
locations_default <- sero_compute_optimal_locations(data)
print(locations_default)

# Example 3.2: Residential area focus
cat("\nExample 3.2: Residential area focus\n")
cat("Parameters: suitable_landuse = c('residential')\n")
locations_residential <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential"),
  min_road_distance = 300,    # Closer to roads for residential access
  max_road_distance = 700,
  grid_size = 75,             # Medium grid size
  max_locations = 8
)
print(locations_residential)

# Example 3.3: Industrial area focus
cat("\nExample 3.3: Industrial area focus\n")
cat("Parameters: suitable_landuse = c('industrial', 'commercial')\n")
locations_industrial <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2, 3),  # All accident types for industrial
  suitable_landuse = c("industrial", "commercial"),
  min_road_distance = 700,       # Further from roads (less congestion)
  max_road_distance = 1200,
  grid_size = 150,               # Larger grid for industrial areas
  max_locations = 5
)
print(locations_industrial)

# Example 3.4: Fine-grained urban analysis
cat("\nExample 3.4: Fine-grained urban analysis\n")
cat("Parameters: grid_size = 50m, max_locations = 15\n")
locations_fine <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial"),
  min_road_distance = 200,
  max_road_distance = 600,
  grid_size = 50,                # Fine grid for detailed analysis
  max_locations = 15             # More locations
)
print(locations_fine)

# Example 3.5: Coarse-grained regional analysis
cat("\nExample 3.5: Coarse-grained regional analysis\n")
cat("Parameters: grid_size = 200m, max_locations = 5\n")
locations_coarse <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 800,
  max_road_distance = 1500,
  grid_size = 200,               # Coarse grid for regional analysis
  max_locations = 5              # Fewer strategic locations
)
print(locations_coarse)

# ==============================================================================
# 4. ROUTE CALCULATION - DETAILED EXAMPLES
# ==============================================================================

cat("\n=== Route Calculation Examples ===\n")

# Example 4.1: Basic route calculation
cat("\nExample 4.1: Basic route calculation\n")
cat("Parameters: Default settings\n")
routes_basic <- sero_calculate_routes(locations_default, data$accident)
print(routes_basic)

# Example 4.2: Many short routes
cat("\nExample 4.2: Many short routes\n")
cat("Parameters: max_routes = 25, max_distance = 5000m\n")
routes_short <- sero_calculate_routes(
  locations_fine,
  data$accident,
  max_routes = 25,
  max_distance = 5000         # Only routes up to 5km
)
print(routes_short)

# Example 4.3: Few long routes
cat("\nExample 4.3: Few long routes\n")
cat("Parameters: max_routes = 8, max_distance = 20000m\n")
routes_long <- sero_calculate_routes(
  locations_coarse,
  data$accident,
  max_routes = 8,
  max_distance = 20000        # Allow routes up to 20km
)
print(routes_long)

# Example 4.4: Route from specific location
cat("\nExample 4.4: Route from specific location to specific accident\n")
if (nrow(locations_default$locations) > 0 && nrow(data$accident) > 0) {
  # Get first optimal location and first accident
  origin <- locations_default$locations[1, ]
  destination <- data$accident[1, ]
  
  # Calculate single route
  single_route <- sero_calculate_routes(
    locations_default,
    destination,
    max_routes = 1
  )
  print(single_route)
  
  # Display route details
  if (nrow(single_route$routes) > 0) {
    route_info <- single_route$routes[1, ]
    cat("Route details:\n")
    cat("- Distance:", round(route_info$distance_m / 1000, 2), "km\n")
    cat("- Estimated time:", round(route_info$estimated_time, 1), "minutes\n")
    cat("- Speed assumed:", round(route_info$speed_kmh, 1), "km/h\n")
  }
}

# ==============================================================================
# 5. COMPARING DIFFERENT PARAMETER SETTINGS
# ==============================================================================

cat("\n=== Comparing Different Parameter Settings ===\n")

# Compare different risk categories
cat("\nComparing risk categories:\n")
risk_scenarios <- list(
  "Fatal only" = c(1),
  "Fatal + Serious" = c(1, 2),
  "All accidents" = c(1, 2, 3)
)

for (scenario_name in names(risk_scenarios)) {
  cat("\nScenario:", scenario_name, "\n")
  categories <- risk_scenarios[[scenario_name]]
  
  # Count accidents in each category
  accident_count <- sum(data$accident$UKATEGORIE %in% categories)
  cat("- Accidents included:", accident_count, "\n")
  
  # Calculate hotspots
  hotspots <- sero_identify_hotspots(data$accident, risk_categories = categories)
  cat("- Hotspots found:", nrow(hotspots$hotspots), "\n")
  
  # Calculate locations
  locations <- sero_compute_optimal_locations(data, risk_categories = categories)
  cat("- Optimal locations:", nrow(locations$locations), "\n")
}

# Compare different grid sizes
cat("\nComparing grid sizes:\n")
grid_scenarios <- c(50, 100, 150, 200)

for (grid_size in grid_scenarios) {
  cat("\nGrid size:", grid_size, "m\n")
  
  locations <- sero_compute_optimal_locations(
    data,
    grid_size = grid_size,
    max_locations = 10
  )
  cat("- Locations found:", nrow(locations$locations), "\n")
  
  # Calculate average score if available
  if ("score" %in% names(locations$locations)) {
    avg_score <- mean(locations$locations$score, na.rm = TRUE)
    cat("- Average score:", round(avg_score, 3), "\n")
  }
}

# ==============================================================================
# 6. VISUALIZATION OF INDIVIDUAL COMPONENTS
# ==============================================================================

cat("\n=== Visualizing Individual Components ===\n")

# Plot different hotspot analyses
cat("Creating hotspot visualizations...\n")
plot_hotspots_basic <- plot(hotspots_basic)
plot_hotspots_precise <- plot(hotspots_precise)
plot_hotspots_broad <- plot(hotspots_broad)

# Plot different location analyses
cat("Creating location visualizations...\n")
plot_locations_residential <- plot(locations_residential)
plot_locations_industrial <- plot(locations_industrial)
plot_locations_fine <- plot(locations_fine)

# Plot different route analyses
cat("Creating route visualizations...\n")
plot_routes_short <- plot(routes_short)
plot_routes_long <- plot(routes_long)

# ==============================================================================
# 7. DETAILED FUNCTION CALL EXAMPLES
# ==============================================================================

cat("\n=== Detailed Function Call Examples ===\n")

# Example with all parameters specified
cat("\nExample with all parameters specified:\n")
cat("sero_identify_hotspots(\n")
cat("  accidents = data$accident,\n")
cat("  risk_categories = c(1, 2),\n")
cat("  bandwidth = 1000,\n")
cat("  min_events = 5\n")
cat(")\n")

detailed_hotspots <- sero_identify_hotspots(
  accidents = data$accident,
  risk_categories = c(1, 2),
  bandwidth = 1000,
  min_events = 5
)

cat("\nsero_compute_optimal_locations(\n")
cat("  data = data,\n")
cat("  risk_categories = c(1, 2),\n")
cat("  suitable_landuse = c('residential', 'commercial', 'industrial'),\n")
cat("  min_road_distance = 500,\n")
cat("  max_road_distance = 1000,\n")
cat("  grid_size = 100,\n")
cat("  max_locations = 10\n")
cat(")\n")

detailed_locations <- sero_compute_optimal_locations(
  data = data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 500,
  max_road_distance = 1000,
  grid_size = 100,
  max_locations = 10
)

cat("\nsero_calculate_routes(\n")
cat("  locations = detailed_locations,\n")
cat("  accidents = data$accident,\n")
cat("  max_routes = 20,\n")
cat("  max_distance = 10000\n")
cat(")\n")

detailed_routes <- sero_calculate_routes(
  locations = detailed_locations,
  accidents = data$accident,
  max_routes = 20,
  max_distance = 10000
)

# ==============================================================================
# 8. RESULTS SUMMARY
# ==============================================================================

cat("\n=== Results Summary ===\n")

# Create summary table
cat("Analysis Summary:\n")
cat("================\n")
cat("Analysis Type        | Hotspots | Locations | Routes\n")
cat("--------------------|---------|-----------|---------\n")
cat("Basic               |", sprintf("%8d", nrow(hotspots_basic$hotspots)), "|", sprintf("%10d", nrow(locations_default$locations)), "|", sprintf("%8d", nrow(routes_basic$routes)), "\n")
cat("Precise (500m)      |", sprintf("%8d", nrow(hotspots_precise$hotspots)), "|", "         -", "|", "       -", "\n")
cat("Broad (1500m)       |", sprintf("%8d", nrow(hotspots_broad$hotspots)), "|", "         -", "|", "       -", "\n")
cat("Residential         |", "       -", "|", sprintf("%10d", nrow(locations_residential$locations)), "|", "       -", "\n")
cat("Industrial          |", "       -", "|", sprintf("%10d", nrow(locations_industrial$locations)), "|", "       -", "\n")
cat("Fine grid (50m)     |", "       -", "|", sprintf("%10d", nrow(locations_fine$locations)), "|", sprintf("%8d", nrow(routes_short$routes)), "\n")
cat("Coarse grid (200m)  |", "       -", "|", sprintf("%10d", nrow(locations_coarse$locations)), "|", sprintf("%8d", nrow(routes_long$routes)), "\n")

cat("\n=== Individual Function Examples Complete ===\n")
cat("This example demonstrated:\n")
cat("- Individual use of each SERO function\n")
cat("- Detailed parameter explanations\n")
cat("- Multiple scenarios and comparisons\n")
cat("- Step-by-step calculation process\n")
cat("- Visualization of individual components\n")
