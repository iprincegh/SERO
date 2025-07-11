# ==============================================================================
# SERO Package - Munster Demo
# ==============================================================================
# This demo showcases the complete functionality of the SERO package using
# the built-in Munster dataset. It demonstrates all core functions:
# 1. Individual and combined visualizations
# 2. Multi-criteria optimal location analysis
# 3. Interactive accident selection and routing
# 4. Comprehensive spatial analysis workflow
# ==============================================================================

library(SERO)

cat("=== SERO Package - Munster Demo ===\n\n")

# ==============================================================================
# 1. DATA LOADING AND EXPLORATION
# ==============================================================================

cat("=== Data Loading and Exploration ===\n")

# Load the built-in Munster dataset
cat("Loading Munster emergency response dataset...\n")
data <- sero_load_data()

# Display basic information about the dataset
cat("Dataset loaded successfully!\n")
cat("Data summary:\n")
cat("- Accidents:", nrow(data$accident), "records\n")
if ("roads" %in% names(data)) {
  cat("- Roads:", nrow(data$roads), "segments\n")
}
if ("landuse" %in% names(data)) {
  cat("- Land use areas:", nrow(data$landuse), "polygons\n")
}
if ("population" %in% names(data)) {
  cat("- Population areas:", nrow(data$population), "polygons\n")
}
if ("districts" %in% names(data)) {
  cat("- Districts:", nrow(data$districts), "polygons\n")
}

# Explore accident categories
cat("\nAccident categories (UKATEGORIE):\n")
accident_summary <- table(data$accident$UKATEGORIE)
print(accident_summary)
cat("1 = Fatal, 2 = Serious injury, 3 = Slight injury\n")

# ==============================================================================
# 2. INDIVIDUAL VISUALIZATIONS
# ==============================================================================

cat("\n=== Individual Visualizations ===\n")

# Plot 1: Munster city boundaries
cat("Creating Munster boundaries plot...\n")
plot_munster <- sero_plot_munster(data)
print(plot_munster)

# Plot 2: Land use data with labels
cat("Creating land use plot...\n")
plot_landuse <- sero_plot_landuse(data)
print(plot_landuse)

# Plot 3: Population density
cat("Creating population density plot...\n")
plot_population <- sero_plot_population(data)
print(plot_population)

# Plot 4: Road network
cat("Creating roads plot...\n")
plot_roads <- sero_plot_roads(data)
print(plot_roads)

# Plot 5: Accidents with severity labels
cat("Creating accidents plot...\n")
plot_accidents <- sero_plot_accidents(data)
print(plot_accidents)

# ==============================================================================
# 3. COMBINED VISUALIZATION
# ==============================================================================

cat("\n=== Combined Visualization ===\n")

# Combined plot showing all layers
cat("Creating combined plot with all layers...\n")
plot_combined <- sero_plot_combined(data, include_layers = c("districts", "landuse", "roads", "accidents"))
print(plot_combined)

# ==============================================================================
# 4. OPTIMAL LOCATION CALCULATION
# ==============================================================================

cat("\n=== Optimal Location Calculation ===\n")

# Calculate optimal emergency unit locations using multi-criteria analysis
cat("Calculating optimal emergency service locations...\n")
cat("Criteria applied:\n")
cat("  1. High-risk accidents (fatal and serious injury)\n")
cat("  2. Suitable land use (residential, commercial, industrial)\n")
cat("  3. Proximity to roads (500m-1000m distance)\n")
cat("  4. Accident density (100m grid cells)\n")
cat("  5. Population density (higher priority)\n")
cat("  6. Grid centroid calculation\n")

optimal_locations <- sero_calculate_optimal_locations(data, max_locations = 10)
print(optimal_locations)

# Plot optimal locations
cat("Creating optimal locations plot...\n")
plot_optimal <- plot(optimal_locations)
print(plot_optimal)

# ==============================================================================
# 5. COMPLETE ANALYSIS WORKFLOW
# ==============================================================================

cat("\n=== Complete Analysis Workflow ===\n")

# Run complete analysis with default parameters
cat("Performing comprehensive emergency response analysis...\n")
results <- sero_analyze()

# Display high-level results
print(results)

# ==============================================================================
# 6. INTERACTIVE ROUTING DEMONSTRATION
# ==============================================================================

cat("\n=== Interactive Routing Demonstration ===\n")

# Create interactive map (if plotly is available)
cat("Creating interactive map for accident selection...\n")
if (requireNamespace("plotly", quietly = TRUE)) {
  interactive_map <- sero_create_interactive_map(data, optimal_locations)
  cat("Interactive map created! Use the following workflow:\n")
  cat("  1. View interactive map: interactive_map\n")
  cat("  2. Click on accident points to get coordinates\n")
  cat("  3. Use coordinates in routing function\n")
  print(interactive_map)
} else {
  cat("plotly package not available. Install with: install.packages('plotly')\n")
}

# Demonstrate routing with example coordinates
cat("Demonstrating routing with example accident selection...\n")
sample_accident <- data$accident[1, ]
sample_coords <- sf::st_coordinates(sf::st_transform(sample_accident, 4326))
cat("Example accident coordinates:\n")
cat("  Longitude:", sample_coords[1], "\n")
cat("  Latitude:", sample_coords[2], "\n")

# Calculate route to selected accident
cat("Calculating route to selected accident...\n")
example_route <- sero_interactive_routing_workflow(
  data, optimal_locations,
  longitude = sample_coords[1],
  latitude = sample_coords[2],
  show_all_routes = TRUE
)

print(example_route)

# Plot the route
cat("Creating route visualization...\n")
route_plot <- plot(example_route)
print(route_plot)

# ==============================================================================
# 7. COMPLETE DEMO WORKFLOW
# ==============================================================================

cat("\n=== Complete Demo Workflow ===\n")

# Run complete demonstration
cat("Running complete demonstration with all features...\n")
demo_results <- sero_complete_demo(show_interactive = TRUE)

# Display demo results
print(demo_results)

# Show individual plots
cat("Displaying individual demo plots...\n")
cat("  - Munster boundaries\n")
print(demo_results$plots$munster)

cat("  - Land use with labels\n")
print(demo_results$plots$landuse)

cat("  - Population density\n")
print(demo_results$plots$population)

cat("  - Road network\n")
print(demo_results$plots$roads)

cat("  - Accidents with severity\n")
print(demo_results$plots$accidents)

cat("  - Combined visualization\n")
print(demo_results$plots$combined)

cat("  - Optimal locations\n")
print(demo_results$plots$optimal)

cat("  - Example route\n")
print(demo_results$plots$route)

# ==============================================================================
# 8. STEP-BY-STEP ANALYSIS (ORIGINAL)
# ==============================================================================

cat("\n=== Step-by-Step Analysis ===\n")

# Step 1: Identify hotspots using original method
cat("Step 1: Identifying accident hotspots...\n")
hotspots <- sero_identify_hotspots(data$accident)
print(hotspots)

# ==============================================================================
# 9. DETAILED RESULTS ANALYSIS
# ==============================================================================

cat("\n=== Detailed Results Analysis ===\n")

# Analyze optimal location results
if (nrow(optimal_locations$locations) > 0) {
  cat("Optimal Location Analysis Details:\n")
  cat("- Number of optimal locations:", nrow(optimal_locations$locations), "\n")
  if ("composite_score" %in% names(optimal_locations$locations)) {
    cat("- Maximum composite score:", round(max(optimal_locations$locations$composite_score, na.rm = TRUE), 3), "\n")
    cat("- Average composite score:", round(mean(optimal_locations$locations$composite_score, na.rm = TRUE), 3), "\n")
  }
  if ("road_distance" %in% names(optimal_locations$locations)) {
    cat("- Average road distance:", round(mean(optimal_locations$locations$road_distance, na.rm = TRUE), 0), "m\n")
  }
  if ("accident_count" %in% names(optimal_locations$locations)) {
    cat("- Total accidents covered:", sum(optimal_locations$locations$accident_count, na.rm = TRUE), "\n")
  }
}

# Analyze interactive route results
if (exists("example_route") && nrow(example_route$routes) > 0) {
  cat("\nInteractive Route Analysis Details:\n")
  cat("- Route distance:", round(example_route$summary$optimal_distance, 0), "meters\n")
  cat("- Estimated travel time:", round(example_route$summary$optimal_time, 1), "minutes\n")
  cat("- Number of alternative routes:", example_route$summary$total_routes, "\n")
}

# ==============================================================================
# 10. COMPLETE ANALYSIS WORKFLOW (ORIGINAL)
# ==============================================================================

cat("\n=== Complete Analysis Workflow ===\n")

# Run complete analysis with default parameters
cat("Performing comprehensive emergency response analysis...\n")
results <- sero_analyze()

# Display high-level results
print(results)

# ==============================================================================
# 3. STEP-BY-STEP ANALYSIS
# ==============================================================================

cat("\n=== Step-by-Step Analysis ===\n")

# Step 1: Identify hotspots
cat("Step 1: Identifying accident hotspots...\n")
hotspots <- sero_identify_hotspots(data$accident)
print(hotspots)

# Step 2: Compute optimal locations
cat("Step 2: Computing optimal service locations...\n")
locations <- sero_compute_optimal_locations(data)
print(locations)

# Step 3: Calculate routes
cat("Step 3: Calculating emergency routes...\n")
routes <- sero_calculate_routes(locations, data$accident)
print(routes)

# ==============================================================================
# 4. CUSTOM PARAMETER EXAMPLES
# ==============================================================================

cat("\n=== Custom Parameter Examples ===\n")

# Example 1: Fatal accidents only
cat("Example 1: Analyzing fatal accidents only...\n")
fatal_hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1),  # Fatal only
  bandwidth = 800,
  min_events = 2
)
cat("Fatal accident hotspots:", nrow(fatal_hotspots$hotspots), "\n")

# Example 2: Residential area focus
cat("Example 2: Optimizing for residential areas...\n")
residential_locations <- sero_compute_optimal_locations(
  data,
  suitable_landuse = c("residential"),
  min_road_distance = 300,
  max_road_distance = 700,
  grid_size = 75,
  max_locations = 8
)
cat("Residential-focused locations:", nrow(residential_locations$locations), "\n")

# Example 3: Quick response routes
cat("Example 3: Calculating quick response routes...\n")
quick_routes <- sero_calculate_routes(
  locations,
  data$accident,
  max_routes = 15,
  max_distance = 5000  # 5km maximum
)
cat("Quick response routes:", nrow(quick_routes$routes), "\n")

# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

cat("\n=== Creating Visualizations ===\n")

# Plot hotspots
cat("Creating hotspot visualization...\n")
hotspot_plot <- plot(hotspots)
print(hotspot_plot)

# Plot optimal locations
cat("Creating optimal locations visualization...\n")
location_plot <- plot(locations)
print(location_plot)

# Plot routes
cat("Creating route visualization...\n")
route_plot <- plot(routes)
print(route_plot)

# Plot custom analysis results
cat("Creating custom analysis visualizations...\n")
fatal_plot <- plot(fatal_hotspots)
residential_plot <- plot(residential_locations)
quick_routes_plot <- plot(quick_routes)

# ==============================================================================
# 6. DETAILED RESULTS ANALYSIS
# ==============================================================================

cat("\n=== Detailed Results Analysis ===\n")

# Analyze hotspot results
if (nrow(hotspots$hotspots) > 0) {
  cat("Hotspot Analysis Details:\n")
  cat("- Number of hotspots:", nrow(hotspots$hotspots), "\n")
  if ("density" %in% names(hotspots$hotspots)) {
    cat("- Maximum density:", round(max(hotspots$hotspots$density, na.rm = TRUE), 3), "\n")
    cat("- Average density:", round(mean(hotspots$hotspots$density, na.rm = TRUE), 3), "\n")
  }
}

# Analyze location results
if (nrow(locations$locations) > 0) {
  cat("\nLocation Analysis Details:\n")
  cat("- Number of locations:", nrow(locations$locations), "\n")
  if ("score" %in% names(locations$locations)) {
    cat("- Maximum score:", round(max(locations$locations$score, na.rm = TRUE), 3), "\n")
    cat("- Average score:", round(mean(locations$locations$score, na.rm = TRUE), 3), "\n")
  }
  if ("road_distance" %in% names(locations$locations)) {
    cat("- Average road distance:", round(mean(locations$locations$road_distance, na.rm = TRUE), 0), "m\n")
  }
}

# Analyze route results
if (nrow(routes$routes) > 0) {
  cat("\nRoute Analysis Details:\n")
  cat("- Number of routes:", nrow(routes$routes), "\n")
  if ("distance_m" %in% names(routes$routes)) {
    cat("- Average distance:", round(mean(routes$routes$distance_m, na.rm = TRUE) / 1000, 2), "km\n")
    cat("- Maximum distance:", round(max(routes$routes$distance_m, na.rm = TRUE) / 1000, 2), "km\n")
  }
  if ("estimated_time" %in% names(routes$routes)) {
    cat("- Average response time:", round(mean(routes$routes$estimated_time, na.rm = TRUE), 1), "minutes\n")
    cat("- Maximum response time:", round(max(routes$routes$estimated_time, na.rm = TRUE), 1), "minutes\n")
  }
}

# ==============================================================================
# 7. SCENARIO COMPARISON
# ==============================================================================

cat("\n=== Scenario Comparison ===\n")

# Compare different scenarios
scenarios <- data.frame(
  Scenario = c("Default", "Fatal Only", "Residential", "Quick Response"),
  Hotspots = c(
    nrow(hotspots$hotspots),
    nrow(fatal_hotspots$hotspots),
    NA,
    NA
  ),
  Locations = c(
    nrow(locations$locations),
    NA,
    nrow(residential_locations$locations),
    NA
  ),
  Routes = c(
    nrow(routes$routes),
    NA,
    NA,
    nrow(quick_routes$routes)
  )
)

cat("Scenario Comparison:\n")
print(scenarios)

# ==============================================================================
# 8. MULTI-CRITERIA ANALYSIS SUMMARY
# ==============================================================================

cat("\n=== Multi-Criteria Analysis Summary ===\n")

cat("The SERO analysis applied the following 6 criteria:\n")
cat("1. High-Risk Accidents: UKATEGORIE = 1 (fatal) or 2 (serious)\n")
cat("2. Suitable Land Use: Residential, commercial, or industrial areas\n")
cat("3. Proximity to Roads: 500m minimum, 1000m maximum from roads\n")
cat("4. Accident Density: 100m grid cells for spatial analysis\n")
cat("5. Population Density: Higher density areas prioritized\n")
cat("6. Centroid Calculation: Optimal points within grid cells\n")

# ==============================================================================
# 9. EXPORT RESULTS (OPTIONAL)
# ==============================================================================

cat("\n=== Export Results (Optional) ===\n")

# Uncomment the following lines to save results
cat("To save results to files, uncomment the following lines:\n")
cat("# sf::st_write(hotspots$hotspots, 'munster_hotspots.gpkg', delete_dsn = TRUE)\n")
cat("# sf::st_write(locations$locations, 'munster_locations.gpkg', delete_dsn = TRUE)\n")
cat("# sf::st_write(routes$routes, 'munster_routes.gpkg', delete_dsn = TRUE)\n")

# Example of saving results
# sf::st_write(hotspots$hotspots, "munster_hotspots.gpkg", delete_dsn = TRUE)
# sf::st_write(locations$locations, "munster_locations.gpkg", delete_dsn = TRUE)
# sf::st_write(routes$routes, "munster_routes.gpkg", delete_dsn = TRUE)

# ==============================================================================
# 10. NEXT STEPS AND RECOMMENDATIONS
# ==============================================================================

cat("\n=== Next Steps and Recommendations ===\n")

cat("Demo completed successfully!\n")
cat("\nKey Results:\n")
cat("- Identified", nrow(hotspots$hotspots), "accident hotspots\n")
cat("- Found", nrow(locations$locations), "optimal service locations\n")
cat("- Calculated", nrow(routes$routes), "emergency routes\n")
if (nrow(routes$routes) > 0) {
  avg_time <- mean(routes$routes$estimated_time, na.rm = TRUE)
  cat("- Average response time:", round(avg_time, 1), "minutes\n")
}

cat("\nRecommendations:\n")
cat("1. Review hotspot locations for accident prevention measures\n")
cat("2. Consider optimal locations for emergency service placement\n")
cat("3. Analyze route efficiency for emergency response planning\n")
cat("4. Customize parameters for specific emergency service types\n")

cat("\nTo explore more advanced features:\n")
cat("- Run source(system.file('examples', 'advanced_parameters.R', package = 'SERO'))\n")
cat("- Run source(system.file('examples', 'parameter_optimization.R', package = 'SERO'))\n")
cat("- Run source(system.file('examples', 'custom_analysis.R', package = 'SERO'))\n")
cat("- See USER_GUIDE.md for comprehensive documentation\n")

cat("\n=== Munster Demo Complete ===\n")
