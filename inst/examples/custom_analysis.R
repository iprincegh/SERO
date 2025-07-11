# ==============================================================================
# SERO Package - Custom Analysis Examples
# ==============================================================================
# This script demonstrates how to create custom analysis workflows by combining
# SERO functions in different ways and extracting specific insights for
# emergency response planning.
# ==============================================================================

library(SERO)

# ==============================================================================
# 1. CUSTOM ANALYSIS WORKFLOWS
# ==============================================================================

cat("=== SERO Package - Custom Analysis Examples ===\n\n")

# Load data
data <- sero_load_data()

cat("=== Custom Analysis Workflows ===\n")

# ==============================================================================
# 2. COMPARATIVE ANALYSIS - DIFFERENT RISK LEVELS
# ==============================================================================

cat("\n=== Comparative Analysis - Risk Levels ===\n")

# Function to perform comparative analysis
perform_comparative_analysis <- function(data) {
  
  # Analysis 1: Fatal accidents only
  cat("Analysis 1: Fatal accidents only (UKATEGORIE = 1)\n")
  fatal_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, risk_categories = c(1), bandwidth = 800),
    locations = sero_compute_optimal_locations(data, risk_categories = c(1), max_locations = 5)
  )
  
  # Analysis 2: Serious accidents (fatal + serious injury)
  cat("Analysis 2: Serious accidents (UKATEGORIE = 1, 2)\n")
  serious_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, risk_categories = c(1, 2), bandwidth = 1000),
    locations = sero_compute_optimal_locations(data, risk_categories = c(1, 2), max_locations = 8)
  )
  
  # Analysis 3: All accidents
  cat("Analysis 3: All accidents (UKATEGORIE = 1, 2, 3)\n")
  all_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, risk_categories = c(1, 2, 3), bandwidth = 1200),
    locations = sero_compute_optimal_locations(data, risk_categories = c(1, 2, 3), max_locations = 10)
  )
  
  # Compare results
  comparison <- data.frame(
    Risk_Level = c("Fatal Only", "Serious (Fatal+Injury)", "All Accidents"),
    Accidents_Included = c(
      sum(data$accident$UKATEGORIE == 1),
      sum(data$accident$UKATEGORIE %in% c(1, 2)),
      sum(data$accident$UKATEGORIE %in% c(1, 2, 3))
    ),
    Hotspots_Found = c(
      nrow(fatal_analysis$hotspots$hotspots),
      nrow(serious_analysis$hotspots$hotspots),
      nrow(all_analysis$hotspots$hotspots)
    ),
    Locations_Found = c(
      nrow(fatal_analysis$locations$locations),
      nrow(serious_analysis$locations$locations),
      nrow(all_analysis$locations$locations)
    )
  )
  
  cat("\nComparison Results:\n")
  print(comparison)
  
  return(list(
    fatal = fatal_analysis,
    serious = serious_analysis,
    all = all_analysis,
    comparison = comparison
  ))
}

# Run comparative analysis
comparative_results <- perform_comparative_analysis(data)

# ==============================================================================
# 3. MULTI-SCALE ANALYSIS - DIFFERENT SPATIAL SCALES
# ==============================================================================

cat("\n=== Multi-Scale Analysis ===\n")

# Function for multi-scale analysis
perform_multiscale_analysis <- function(data) {
  
  # Local scale: Fine grid, small bandwidth
  cat("Local scale analysis (fine resolution)\n")
  local_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, bandwidth = 500, min_events = 3),
    locations = sero_compute_optimal_locations(data, grid_size = 50, max_locations = 15)
  )
  
  # Regional scale: Medium grid, medium bandwidth
  cat("Regional scale analysis (medium resolution)\n")
  regional_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, bandwidth = 1000, min_events = 5),
    locations = sero_compute_optimal_locations(data, grid_size = 100, max_locations = 10)
  )
  
  # Strategic scale: Coarse grid, large bandwidth
  cat("Strategic scale analysis (coarse resolution)\n")
  strategic_analysis <- list(
    hotspots = sero_identify_hotspots(data$accident, bandwidth = 2000, min_events = 8),
    locations = sero_compute_optimal_locations(data, grid_size = 200, max_locations = 5)
  )
  
  # Compare scales
  scale_comparison <- data.frame(
    Scale = c("Local", "Regional", "Strategic"),
    Grid_Size = c(50, 100, 200),
    Bandwidth = c(500, 1000, 2000),
    Hotspots = c(
      nrow(local_analysis$hotspots$hotspots),
      nrow(regional_analysis$hotspots$hotspots),
      nrow(strategic_analysis$hotspots$hotspots)
    ),
    Locations = c(
      nrow(local_analysis$locations$locations),
      nrow(regional_analysis$locations$locations),
      nrow(strategic_analysis$locations$locations)
    )
  )
  
  cat("\nMulti-Scale Comparison:\n")
  print(scale_comparison)
  
  return(list(
    local = local_analysis,
    regional = regional_analysis,
    strategic = strategic_analysis,
    comparison = scale_comparison
  ))
}

# Run multi-scale analysis
multiscale_results <- perform_multiscale_analysis(data)

# ==============================================================================
# 4. LAND USE OPTIMIZATION - DIFFERENT DEVELOPMENT SCENARIOS
# ==============================================================================

cat("\n=== Land Use Optimization ===\n")

# Function for land use scenario analysis
perform_landuse_analysis <- function(data) {
  
  # Scenario 1: Residential focus
  cat("Scenario 1: Residential area focus\n")
  residential_analysis <- sero_compute_optimal_locations(
    data,
    suitable_landuse = c("residential"),
    min_road_distance = 200,
    max_road_distance = 600,
    grid_size = 75,
    max_locations = 10
  )
  
  # Scenario 2: Commercial focus
  cat("Scenario 2: Commercial area focus\n")
  commercial_analysis <- sero_compute_optimal_locations(
    data,
    suitable_landuse = c("commercial"),
    min_road_distance = 400,
    max_road_distance = 800,
    grid_size = 100,
    max_locations = 8
  )
  
  # Scenario 3: Industrial focus
  cat("Scenario 3: Industrial area focus\n")
  industrial_analysis <- sero_compute_optimal_locations(
    data,
    suitable_landuse = c("industrial"),
    min_road_distance = 600,
    max_road_distance = 1200,
    grid_size = 125,
    max_locations = 5
  )
  
  # Scenario 4: Mixed development
  cat("Scenario 4: Mixed development\n")
  mixed_analysis <- sero_compute_optimal_locations(
    data,
    suitable_landuse = c("residential", "commercial", "industrial"),
    min_road_distance = 500,
    max_road_distance = 1000,
    grid_size = 100,
    max_locations = 12
  )
  
  # Compare land use scenarios
  landuse_comparison <- data.frame(
    Scenario = c("Residential", "Commercial", "Industrial", "Mixed"),
    Locations = c(
      nrow(residential_analysis$locations),
      nrow(commercial_analysis$locations),
      nrow(industrial_analysis$locations),
      nrow(mixed_analysis$locations)
    ),
    Avg_Road_Distance = c(
      ifelse(nrow(residential_analysis$locations) > 0, 
             mean(residential_analysis$locations$road_distance, na.rm = TRUE), 0),
      ifelse(nrow(commercial_analysis$locations) > 0, 
             mean(commercial_analysis$locations$road_distance, na.rm = TRUE), 0),
      ifelse(nrow(industrial_analysis$locations) > 0, 
             mean(industrial_analysis$locations$road_distance, na.rm = TRUE), 0),
      ifelse(nrow(mixed_analysis$locations) > 0, 
             mean(mixed_analysis$locations$road_distance, na.rm = TRUE), 0)
    )
  )
  
  cat("\nLand Use Scenario Comparison:\n")
  print(landuse_comparison)
  
  return(list(
    residential = residential_analysis,
    commercial = commercial_analysis,
    industrial = industrial_analysis,
    mixed = mixed_analysis,
    comparison = landuse_comparison
  ))
}

# Run land use analysis
landuse_results <- perform_landuse_analysis(data)

# ==============================================================================
# 5. ROUTE OPTIMIZATION - DIFFERENT RESPONSE STRATEGIES
# ==============================================================================

cat("\n=== Route Optimization Strategies ===\n")

# Function for route optimization analysis
perform_route_analysis <- function(data, locations) {
  
  # Strategy 1: Quick response (short routes only)
  cat("Strategy 1: Quick response (short routes)\n")
  quick_routes <- sero_calculate_routes(
    locations,
    data$accident,
    max_routes = 30,
    max_distance = 5000  # 5km maximum
  )
  
  # Strategy 2: Comprehensive coverage (medium routes)
  cat("Strategy 2: Comprehensive coverage (medium routes)\n")
  comprehensive_routes <- sero_calculate_routes(
    locations,
    data$accident,
    max_routes = 20,
    max_distance = 12000  # 12km maximum
  )
  
  # Strategy 3: Emergency backup (long routes)
  cat("Strategy 3: Emergency backup (long routes)\n")
  backup_routes <- sero_calculate_routes(
    locations,
    data$accident,
    max_routes = 10,
    max_distance = 25000  # 25km maximum
  )
  
  # Calculate route statistics
  route_stats <- data.frame(
    Strategy = c("Quick Response", "Comprehensive", "Emergency Backup"),
    Routes_Calculated = c(
      nrow(quick_routes$routes),
      nrow(comprehensive_routes$routes),
      nrow(backup_routes$routes)
    ),
    Avg_Distance_km = c(
      ifelse(nrow(quick_routes$routes) > 0, 
             mean(quick_routes$routes$distance_m / 1000, na.rm = TRUE), 0),
      ifelse(nrow(comprehensive_routes$routes) > 0, 
             mean(comprehensive_routes$routes$distance_m / 1000, na.rm = TRUE), 0),
      ifelse(nrow(backup_routes$routes) > 0, 
             mean(backup_routes$routes$distance_m / 1000, na.rm = TRUE), 0)
    ),
    Avg_Time_min = c(
      ifelse(nrow(quick_routes$routes) > 0, 
             mean(quick_routes$routes$estimated_time, na.rm = TRUE), 0),
      ifelse(nrow(comprehensive_routes$routes) > 0, 
             mean(comprehensive_routes$routes$estimated_time, na.rm = TRUE), 0),
      ifelse(nrow(backup_routes$routes) > 0, 
             mean(backup_routes$routes$estimated_time, na.rm = TRUE), 0)
    )
  )
  
  cat("\nRoute Strategy Comparison:\n")
  print(route_stats)
  
  return(list(
    quick = quick_routes,
    comprehensive = comprehensive_routes,
    backup = backup_routes,
    stats = route_stats
  ))
}

# Run route analysis using regional scale locations
route_results <- perform_route_analysis(data, multiscale_results$regional$locations)

# ==============================================================================
# 6. CUSTOM SCORING AND RANKING
# ==============================================================================

cat("\n=== Custom Scoring and Ranking ===\n")

# Function to create custom scoring system
create_custom_scoring <- function(locations, accidents, routes) {
  
  if (nrow(locations$locations) == 0) {
    cat("No locations available for scoring\n")
    return(NULL)
  }
  
  # Extract location data
  location_data <- locations$locations
  
  # Calculate custom scores
  custom_scores <- data.frame(
    location_id = seq_len(nrow(location_data)),
    base_score = location_data$score,
    stringsAsFactors = FALSE
  )
  
  # Add accident coverage score
  # (This is a simplified example - in practice you'd calculate actual coverage)
  if (nrow(accidents) > 0) {
    custom_scores$accident_coverage <- runif(nrow(location_data), 0.5, 1.0)
  } else {
    custom_scores$accident_coverage <- 0
  }
  
  # Add route efficiency score
  if (!is.null(routes) && nrow(routes$routes) > 0) {
    # Calculate average route time for each location
    location_route_times <- aggregate(
      routes$routes$estimated_time,
      by = list(location_id = routes$routes$from_location_id),
      FUN = mean,
      na.rm = TRUE
    )
    
    # Merge with custom scores
    custom_scores <- merge(custom_scores, location_route_times, 
                          by = "location_id", all.x = TRUE)
    names(custom_scores)[names(custom_scores) == "x"] <- "avg_route_time"
    
    # Convert time to efficiency score (lower time = higher score)
    max_time <- max(custom_scores$avg_route_time, na.rm = TRUE)
    custom_scores$route_efficiency <- ifelse(
      is.na(custom_scores$avg_route_time), 0,
      1 - (custom_scores$avg_route_time / max_time)
    )
  } else {
    custom_scores$avg_route_time <- NA
    custom_scores$route_efficiency <- 0
  }
  
  # Calculate composite score
  custom_scores$composite_score <- (
    0.4 * custom_scores$base_score +
    0.3 * custom_scores$accident_coverage +
    0.3 * custom_scores$route_efficiency
  )
  
  # Rank locations
  custom_scores <- custom_scores[order(custom_scores$composite_score, decreasing = TRUE), ]
  custom_scores$rank <- seq_len(nrow(custom_scores))
  
  return(custom_scores)
}

# Apply custom scoring to regional analysis
cat("Applying custom scoring to regional analysis results...\n")
regional_scoring <- create_custom_scoring(
  multiscale_results$regional$locations,
  data$accident,
  route_results$comprehensive
)

if (!is.null(regional_scoring)) {
  cat("Top 5 locations by composite score:\n")
  print(head(regional_scoring, 5))
}

# ==============================================================================
# 7. TEMPORAL ANALYSIS SIMULATION
# ==============================================================================

cat("\n=== Temporal Analysis Simulation ===\n")

# Function to simulate temporal analysis
simulate_temporal_analysis <- function(data) {
  
  # Simulate different time periods by sampling accidents
  set.seed(123)  # For reproducible results
  
  # Period 1: Peak hours (simulate by sampling 70% of accidents)
  peak_accidents <- data$accident[sample(nrow(data$accident), 
                                        round(0.7 * nrow(data$accident))), ]
  
  # Period 2: Off-peak hours (simulate by sampling 30% of accidents)
  offpeak_accidents <- data$accident[sample(nrow(data$accident), 
                                           round(0.3 * nrow(data$accident))), ]
  
  # Period 3: Weekend (simulate by sampling 40% of accidents)
  weekend_accidents <- data$accident[sample(nrow(data$accident), 
                                           round(0.4 * nrow(data$accident))), ]
  
  # Analyze each period
  cat("Analyzing peak hours...\n")
  peak_hotspots <- sero_identify_hotspots(peak_accidents, bandwidth = 1000)
  
  cat("Analyzing off-peak hours...\n")
  offpeak_hotspots <- sero_identify_hotspots(offpeak_accidents, bandwidth = 1000)
  
  cat("Analyzing weekend...\n")
  weekend_hotspots <- sero_identify_hotspots(weekend_accidents, bandwidth = 1000)
  
  # Compare temporal patterns
  temporal_comparison <- data.frame(
    Period = c("Peak Hours", "Off-Peak Hours", "Weekend"),
    Accidents = c(nrow(peak_accidents), nrow(offpeak_accidents), nrow(weekend_accidents)),
    Hotspots = c(nrow(peak_hotspots$hotspots), nrow(offpeak_hotspots$hotspots), nrow(weekend_hotspots$hotspots))
  )
  
  cat("\nTemporal Analysis Results:\n")
  print(temporal_comparison)
  
  return(list(
    peak = peak_hotspots,
    offpeak = offpeak_hotspots,
    weekend = weekend_hotspots,
    comparison = temporal_comparison
  ))
}

# Run temporal analysis simulation
temporal_results <- simulate_temporal_analysis(data)

# ==============================================================================
# 8. CONSTRAINT-BASED OPTIMIZATION
# ==============================================================================

cat("\n=== Constraint-Based Optimization ===\n")

# Function for constraint-based optimization
perform_constraint_optimization <- function(data) {
  
  # Constraint 1: Budget constraint (maximum 6 locations)
  cat("Constraint 1: Budget constraint (max 6 locations)\n")
  budget_constrained <- sero_compute_optimal_locations(
    data,
    max_locations = 6,
    grid_size = 100
  )
  
  # Constraint 2: Coverage constraint (must cover all land use types)
  cat("Constraint 2: Coverage constraint (all land use types)\n")
  coverage_constrained <- sero_compute_optimal_locations(
    data,
    suitable_landuse = c("residential", "commercial", "industrial"),
    max_locations = 10,
    grid_size = 100
  )
  
  # Constraint 3: Accessibility constraint (close to roads)
  cat("Constraint 3: Accessibility constraint (close to roads)\n")
  accessibility_constrained <- sero_compute_optimal_locations(
    data,
    min_road_distance = 100,
    max_road_distance = 500,
    max_locations = 8,
    grid_size = 100
  )
  
  # Constraint 4: Safety constraint (away from high-traffic roads)
  cat("Constraint 4: Safety constraint (away from high-traffic roads)\n")
  safety_constrained <- sero_compute_optimal_locations(
    data,
    min_road_distance = 800,
    max_road_distance = 1500,
    max_locations = 6,
    grid_size = 100
  )
  
  # Compare constraint scenarios
  constraint_comparison <- data.frame(
    Constraint = c("Budget", "Coverage", "Accessibility", "Safety"),
    Max_Locations = c(6, 10, 8, 6),
    Locations_Found = c(
      nrow(budget_constrained$locations),
      nrow(coverage_constrained$locations),
      nrow(accessibility_constrained$locations),
      nrow(safety_constrained$locations)
    ),
    Min_Road_Dist = c(500, 500, 100, 800),
    Max_Road_Dist = c(1000, 1000, 500, 1500)
  )
  
  cat("\nConstraint-Based Optimization Results:\n")
  print(constraint_comparison)
  
  return(list(
    budget = budget_constrained,
    coverage = coverage_constrained,
    accessibility = accessibility_constrained,
    safety = safety_constrained,
    comparison = constraint_comparison
  ))
}

# Run constraint-based optimization
constraint_results <- perform_constraint_optimization(data)

# ==============================================================================
# 9. COMPREHENSIVE CUSTOM ANALYSIS
# ==============================================================================

cat("\n=== Comprehensive Custom Analysis ===\n")

# Function for comprehensive custom analysis
perform_comprehensive_analysis <- function(data) {
  
  # Step 1: Multi-risk hotspot analysis
  cat("Step 1: Multi-risk hotspot analysis\n")
  hotspot_analysis <- list(
    fatal = sero_identify_hotspots(data$accident, risk_categories = c(1), bandwidth = 800),
    serious = sero_identify_hotspots(data$accident, risk_categories = c(1, 2), bandwidth = 1000),
    all = sero_identify_hotspots(data$accident, risk_categories = c(1, 2, 3), bandwidth = 1200)
  )
  
  # Step 2: Multi-scale location optimization
  cat("Step 2: Multi-scale location optimization\n")
  location_analysis <- list(
    fine = sero_compute_optimal_locations(data, grid_size = 50, max_locations = 12),
    medium = sero_compute_optimal_locations(data, grid_size = 100, max_locations = 8),
    coarse = sero_compute_optimal_locations(data, grid_size = 150, max_locations = 5)
  )
  
  # Step 3: Multi-strategy route calculation
  cat("Step 3: Multi-strategy route calculation\n")
  route_analysis <- list(
    quick = sero_calculate_routes(location_analysis$medium, data$accident, max_routes = 20, max_distance = 5000),
    standard = sero_calculate_routes(location_analysis$medium, data$accident, max_routes = 15, max_distance = 10000),
    extended = sero_calculate_routes(location_analysis$medium, data$accident, max_routes = 10, max_distance = 20000)
  )
  
  # Step 4: Comprehensive summary
  summary_stats <- data.frame(
    Analysis_Type = c("Hotspots (Fatal)", "Hotspots (Serious)", "Hotspots (All)",
                     "Locations (Fine)", "Locations (Medium)", "Locations (Coarse)",
                     "Routes (Quick)", "Routes (Standard)", "Routes (Extended)"),
    Count = c(
      nrow(hotspot_analysis$fatal$hotspots),
      nrow(hotspot_analysis$serious$hotspots),
      nrow(hotspot_analysis$all$hotspots),
      nrow(location_analysis$fine$locations),
      nrow(location_analysis$medium$locations),
      nrow(location_analysis$coarse$locations),
      nrow(route_analysis$quick$routes),
      nrow(route_analysis$standard$routes),
      nrow(route_analysis$extended$routes)
    ),
    Parameter = c(
      "bandwidth=800", "bandwidth=1000", "bandwidth=1200",
      "grid=50", "grid=100", "grid=150",
      "dist=5km", "dist=10km", "dist=20km"
    )
  )
  
  cat("\nComprehensive Analysis Summary:\n")
  print(summary_stats)
  
  return(list(
    hotspots = hotspot_analysis,
    locations = location_analysis,
    routes = route_analysis,
    summary = summary_stats
  ))
}

# Run comprehensive custom analysis
comprehensive_results <- perform_comprehensive_analysis(data)

# ==============================================================================
# 10. RESULTS EXPORT AND REPORTING
# ==============================================================================

cat("\n=== Results Export and Reporting ===\n")

# Function to create analysis report
create_analysis_report <- function(results) {
  
  report <- list(
    title = "SERO Custom Analysis Report",
    timestamp = Sys.time(),
    data_summary = list(
      total_accidents = nrow(data$accident),
      study_area_km2 = round((sf::st_bbox(data$accident)$xmax - sf::st_bbox(data$accident)$xmin) * 
                            (sf::st_bbox(data$accident)$ymax - sf::st_bbox(data$accident)$ymin) / 1000000, 2),
      available_layers = names(data)
    ),
    analysis_results = results
  )
  
  # Print report summary
  cat("Analysis Report Generated:\n")
  cat("========================\n")
  cat("Title:", report$title, "\n")
  cat("Timestamp:", as.character(report$timestamp), "\n")
  cat("Total accidents:", report$data_summary$total_accidents, "\n")
  cat("Study area:", report$data_summary$study_area_km2, "km²\n")
  cat("Available layers:", paste(report$data_summary$available_layers, collapse = ", "), "\n")
  
  return(report)
}

# Create comprehensive report
analysis_report <- create_analysis_report(comprehensive_results)

# Save results (optional)
cat("\nSaving results (optional):\n")
cat("# Uncomment the following lines to save results:\n")
cat("# saveRDS(analysis_report, 'sero_custom_analysis_report.rds')\n")
cat("# write.csv(comprehensive_results$summary, 'sero_analysis_summary.csv', row.names = FALSE)\n")

# Export spatial data (optional)
cat("# Export spatial data:\n")
cat("# sf::st_write(comprehensive_results$locations$medium$locations, 'optimal_locations.gpkg')\n")
cat("# sf::st_write(comprehensive_results$hotspots$serious$hotspots, 'accident_hotspots.gpkg')\n")
cat("# sf::st_write(comprehensive_results$routes$standard$routes, 'emergency_routes.gpkg')\n")

cat("\n=== Custom Analysis Examples Complete ===\n")
cat("This example demonstrated:\n")
cat("- Comparative analysis across different risk levels\n")
cat("- Multi-scale spatial analysis\n")
cat("- Land use optimization scenarios\n")
cat("- Route optimization strategies\n")
cat("- Custom scoring and ranking systems\n")
cat("- Temporal analysis simulation\n")
cat("- Constraint-based optimization\n")
cat("- Comprehensive custom analysis workflow\n")
cat("- Results export and reporting\n")
cat("\nUse these examples as templates for your specific analysis needs.\n")
