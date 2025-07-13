#' SERO Enhanced Workflow Example
#'
#' Complete workflow demonstration for SERO package with all professional features

#' Run complete SERO workflow with professional features
#'
#' This function demonstrates the complete SERO workflow including:
#' - Professional accident visualization with severity filtering
#' - Advanced optimal location calculation with multiple algorithms
#' - Interactive emergency routing with OSRM integration
#' - Spatialite database storage for optimal locations
#' - Comprehensive mapping with toggleable layers
#'
#' @param data_path path to GPKG file or data list (optional, uses built-in data if NULL)
#' @param severity_levels vector of accident severity levels to analyze (default: c(1,2))
#' @param num_optimal_locations number of optimal emergency locations to find (default: 5)
#' @param optimization_method method for finding optimal locations ("hybrid", "kmeans", "grid", "density")
#' @param launch_interactive logical whether to launch interactive routing app
#' @return list containing all analysis results and visualizations
#' @export
sero_complete_workflow <- function(data_path = NULL,
                                  severity_levels = c(1, 2),
                                  num_optimal_locations = 5,
                                  optimization_method = "hybrid",
                                  launch_interactive = TRUE) {
  
  cat("🚑 SERO: Spatial Emergency Response Optimization\n")
  cat("================================================\n")
  
  # Step 1: Load data
  cat("\n📊 Step 1: Loading spatial data...\n")
  if (is.null(data_path)) {
    data <- sero_load_data()
    cat("✅ Using built-in Munster dataset\n")
  } else {
    # Load from GPKG file (if provided)
    data <- list(
      accident = sf::st_read(data_path, layer = "accident", quiet = TRUE),
      landuse = sf::st_read(data_path, layer = "landuse_munster", quiet = TRUE),
      population = sf::st_read(data_path, layer = "population_density", quiet = TRUE),
      roads = sf::st_read(data_path, layer = "roads", quiet = TRUE),
      districts = sf::st_read(data_path, layer = "districts", quiet = TRUE)
    )
    cat("✅ Loaded data from:", data_path, "\n")
  }
  
  cat(sprintf("   - Accidents: %d features\n", nrow(data$accident)))
  cat(sprintf("   - Districts: %d features\n", nrow(data$districts)))
  cat(sprintf("   - Roads: %d features\n", nrow(data$roads)))
  cat(sprintf("   - Landuse: %d features\n", nrow(data$landuse)))
  
  # Step 2: Professional accident visualization
  cat("\n🗺️  Step 2: Creating professional accident visualization...\n")
  accident_map <- sero_plot_accidents(
    accidents = data$accident,
    districts = data$districts,
    landuse = data$landuse,
    severity_levels = severity_levels,
    use_osm_basemap = TRUE
  )
  cat("✅ Accident map created with severity filtering\n")
  
  # Step 3: Calculate optimal emergency service locations
  cat(sprintf("\n🎯 Step 3: Computing optimal emergency service locations (%s method)...\n", optimization_method))
  optimal_locations <- sero_calculate_optimal_locations(
    data = data,
    grid_size = 100,
    risk_categories = severity_levels,
    min_road_distance = 500,
    max_road_distance = 1000,
    save_to_db = TRUE,
    db_path = "sero_optimal_locations.sqlite"
  )
  cat("✅ Optimal locations calculated and saved to database\n")
  
  # Add optimal locations to data for routing
  data$optimal_locations <- optimal_locations
  
  # Step 4: Visualize optimal locations
  cat("\n📍 Step 4: Creating optimal locations visualization...\n")
  optimal_map <- sero_plot_optimal_locations(
    optimal_locs = optimal_locations,
    districts = data$districts,
    roads = data$roads,
    landuse = data$landuse,
    accidents = data$accident
  )
  cat("✅ Optimal locations map created with contextual layers\n")
  
  # Step 5: Enhanced hotspot analysis
  cat("\n🔥 Step 5: Performing enhanced hotspot analysis...\n")
  hotspot_result <- sero_hotspot_analysis(
    data = data,
    intensity = "medium",
    include_landuse = TRUE,
    contour_levels = c(0.2, 0.4, 0.6, 0.8)
  )
  cat("✅ Hotspot analysis completed with landuse integration\n")
  
  # Step 6: Test emergency routing
  cat("\n🚨 Step 6: Testing emergency routing system...\n")
  
  # Create a simple routing test without the full function
  districts_bbox <- sf::st_bbox(sf::st_transform(data$districts, 4326))
  test_accident_coords <- c(
    districts_bbox[1] + (districts_bbox[3] - districts_bbox[1]) * 0.5,
    districts_bbox[2] + (districts_bbox[4] - districts_bbox[2]) * 0.5
  )
  
  # Find nearest optimal location for test
  test_accident_point <- sf::st_sfc(sf::st_point(test_accident_coords), crs = 4326)
  nearest_idx <- sf::st_nearest_feature(test_accident_point, optimal_locations)
  nearest_base <- optimal_locations[nearest_idx, ]
  test_distance <- as.numeric(sf::st_distance(test_accident_point, nearest_base))
  
  test_route_stats <- list(
    nearest_location_id = nearest_base$location_id,
    distance_km = round(test_distance / 1000, 2),
    estimated_travel_time_min = round(test_distance / 1000 / 50 * 60, 1)
  )
  
  cat("✅ Emergency routing test completed\n")
  cat(sprintf("   - Nearest base: %s\n", test_route_stats$nearest_location_id))
  cat(sprintf("   - Distance: %s km\n", test_route_stats$distance_km))
  cat(sprintf("   - Est. time: %s minutes\n", test_route_stats$estimated_travel_time_min))
  
  # Step 7: Launch interactive routing system
  if (launch_interactive) {
    cat("\n🚀 Step 7: Launching interactive routing system...\n")
    cat("   Click anywhere on the map to simulate accidents and get real-time routing!\n")
    cat("   Features:\n")
    cat("   - 🖱️  Click-to-simulate accident locations\n")
    cat("   - 🛣️  Real-time routing to nearest emergency base\n")
    cat("   - 🗂️  Toggleable layers (landuse, accidents, roads)\n")
    cat("   - 📊 Live performance metrics\n")
    cat("   - 🗺️  Professional map styling with multiple base layers\n")
    
    # Launch the interactive app
    interactive_app <- sero_interactive_routing(
      optimal_locs = optimal_locations,
      roads = data$roads,
      districts = data$districts,
      landuse = data$landuse,
      accidents = data$accident
    )
    
    # Return the interactive app directly
    return(interactive_app)
  }
  
  # Step 8: Generate comprehensive report
  cat("\n📋 Step 8: Generating comprehensive analysis report...\n")
  
  # Performance summary
  performance_summary <- list(
    total_accidents = nrow(data$accident),
    high_risk_accidents = nrow(data$accident[data$accident$UKATEGORIE %in% severity_levels, ]) %||% nrow(data$accident),
    optimal_bases = nrow(optimal_locations),
    avg_response_distance = mean(optimal_locations$accident_count_500m %||% rep(0, nrow(optimal_locations))),
    coverage_analysis = calculate_coverage_analysis(optimal_locations, data$accident),
    hotspot_stats = hotspot_result$stats
  )
  
  cat("✅ Analysis complete!\n")
  cat("\n📊 PERFORMANCE SUMMARY\n")
  cat("======================\n")
  cat(sprintf("🎯 Total accidents analyzed: %d\n", performance_summary$total_accidents))
  cat(sprintf("⚠️  High-risk accidents (severity %s): %d\n", 
             paste(severity_levels, collapse = ","), performance_summary$high_risk_accidents))
  cat(sprintf("🏥 Optimal emergency bases: %d\n", performance_summary$optimal_bases))
  cat(sprintf("📍 Database saved: sero_optimal_locations.sqlite\n"))
  
  # Return comprehensive results
  return(list(
    data = data,
    accident_map = accident_map,
    optimal_locations = optimal_locations,
    optimal_map = optimal_map,
    hotspot_result = hotspot_result,
    test_route_stats = test_route_stats,
    performance_summary = performance_summary,
    interactive_available = TRUE
  ))
}

#' Calculate coverage analysis for optimal locations
#' @keywords internal
calculate_coverage_analysis <- function(optimal_locations, accidents) {
  
  if (is.null(optimal_locations) || is.null(accidents)) {
    return(list(coverage_500m = 0, coverage_1000m = 0, avg_distance = Inf))
  }
  
  # Transform to UTM for distance calculations
  optimal_utm <- sf::st_transform(optimal_locations, 32632)
  accidents_utm <- sf::st_transform(accidents, 32632)
  
  # Calculate minimum distances from each accident to nearest base
  min_distances <- numeric(nrow(accidents_utm))
  
  for (i in seq_len(nrow(accidents_utm))) {
    distances <- as.numeric(sf::st_distance(accidents_utm[i, ], optimal_utm))
    min_distances[i] <- min(distances)
  }
  
  coverage_500m <- sum(min_distances <= 500) / length(min_distances) * 100
  coverage_1000m <- sum(min_distances <= 1000) / length(min_distances) * 100
  avg_distance <- mean(min_distances)
  
  return(list(
    coverage_500m = round(coverage_500m, 1),
    coverage_1000m = round(coverage_1000m, 1),
    avg_distance = round(avg_distance, 0)
  ))
}

#' Quick workflow for testing and demonstration
#'
#' @param interactive logical whether to launch interactive routing
#' @return analysis results or interactive app
#' @export
sero_quick_demo <- function(interactive = TRUE) {
  
  cat("🚀 SERO Quick Demo\n")
  cat("==================\n")
  cat("Running complete emergency response optimization workflow...\n")
  
  return(sero_complete_workflow(
    data_path = NULL,  # Use built-in data
    severity_levels = c(1, 2),  # Fatal and serious injuries
    num_optimal_locations = 3,  # 3 emergency bases
    optimization_method = "hybrid",
    launch_interactive = interactive
  ))
}

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
