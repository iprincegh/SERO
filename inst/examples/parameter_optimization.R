# ==============================================================================
# SERO Package - Parameter Optimization Guide
# ==============================================================================
# This script demonstrates how to optimize parameters for different scenarios
# and provides guidance on choosing appropriate parameter values for
# different types of emergency response optimization problems.
# ==============================================================================

library(SERO)

# ==============================================================================
# 1. PARAMETER OPTIMIZATION STRATEGIES
# ==============================================================================

cat("=== SERO Package - Parameter Optimization Guide ===\n\n")

# Load data
data <- sero_load_data()

cat("=== Parameter Optimization Strategies ===\n")

# ==============================================================================
# 2. SCENARIO-BASED PARAMETER RECOMMENDATIONS
# ==============================================================================

cat("\n=== Scenario-Based Parameter Recommendations ===\n")

# Scenario 1: Urban Dense Area
cat("\nScenario 1: Urban Dense Area\n")
cat("Characteristics: High accident density, mixed land use, good road network\n")
cat("Recommended parameters:\n")
cat("- risk_categories: c(1, 2) - Focus on serious accidents\n")
cat("- suitable_landuse: c('residential', 'commercial') - Urban land use\n")
cat("- min_road_distance: 200-400m - Close to roads for accessibility\n")
cat("- max_road_distance: 600-800m - Avoid congestion\n")
cat("- grid_size: 50-75m - Fine grid for detailed analysis\n")
cat("- max_locations: 10-15 - Multiple locations for coverage\n")
cat("- buffer: 500-800m - Precise hotspots\n")

urban_params <- list(
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial"),
  min_road_distance = 300,
  max_road_distance = 700,
  grid_size = 50,
  max_locations = 12,
  buffer = 600
)

cat("Running urban scenario analysis...\n")
urban_hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = urban_params$risk_categories,
  buffer = urban_params$buffer
)

urban_locations <- sero_compute_optimal_locations(
  data,
  risk_categories = urban_params$risk_categories,
  suitable_landuse = urban_params$suitable_landuse,
  min_road_distance = urban_params$min_road_distance,
  max_road_distance = urban_params$max_road_distance,
  grid_size = urban_params$grid_size,
  max_locations = urban_params$max_locations
)

cat("Urban scenario results:\n")
cat("- Hotspots found:", nrow(urban_hotspots$hotspots), "\n")
cat("- Optimal locations:", nrow(urban_locations$locations), "\n")

# Scenario 2: Rural/Sparse Area
cat("\nScenario 2: Rural/Sparse Area\n")
cat("Characteristics: Lower accident density, agricultural/rural land use, sparse roads\n")
cat("Recommended parameters:\n")
cat("- risk_categories: c(1, 2, 3) - Include all accidents (fewer events)\n")
cat("- suitable_landuse: c('residential', 'commercial', 'industrial') - Flexible\n")
cat("- min_road_distance: 500-800m - Further from roads (less congestion)\n")
cat("- max_road_distance: 1200-2000m - Larger buffer for sparse areas\n")
cat("- grid_size: 150-200m - Coarser grid for regional analysis\n")
cat("- max_locations: 5-8 - Fewer strategic locations\n")
cat("- buffer: 1200-2000m - Broader hotspots\n")

rural_params <- list(
  risk_categories = c(1, 2, 3),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 600,
  max_road_distance = 1500,
  grid_size = 150,
  max_locations = 6,
  buffer = 1500
)

cat("Running rural scenario analysis...\n")
rural_hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = rural_params$risk_categories,
  buffer = rural_params$buffer
)

rural_locations <- sero_compute_optimal_locations(
  data,
  risk_categories = rural_params$risk_categories,
  suitable_landuse = rural_params$suitable_landuse,
  min_road_distance = rural_params$min_road_distance,
  max_road_distance = rural_params$max_road_distance,
  grid_size = rural_params$grid_size,
  max_locations = rural_params$max_locations
)

cat("Rural scenario results:\n")
cat("- Hotspots found:", nrow(rural_hotspots$hotspots), "\n")
cat("- Optimal locations:", nrow(rural_locations$locations), "\n")

# Scenario 3: Industrial Zone
cat("\nScenario 3: Industrial Zone\n")
cat("Characteristics: High-risk accidents, industrial land use, heavy traffic\n")
cat("Recommended parameters:\n")
cat("- risk_categories: c(1, 2) - Focus on serious accidents\n")
cat("- suitable_landuse: c('industrial', 'commercial') - Industrial focus\n")
cat("- min_road_distance: 800-1000m - Away from congestion\n")
cat("- max_road_distance: 1500-2000m - Larger buffer\n")
cat("- grid_size: 100-150m - Medium grid for industrial areas\n")
cat("- max_locations: 4-6 - Fewer strategic locations\n")
cat("- buffer: 1000-1500m - Medium to broad hotspots\n")

industrial_params <- list(
  risk_categories = c(1, 2),
  suitable_landuse = c("industrial", "commercial"),
  min_road_distance = 800,
  max_road_distance = 1500,
  grid_size = 125,
  max_locations = 5,
  buffer = 1200
)

cat("Running industrial scenario analysis...\n")
industrial_hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = industrial_params$risk_categories,
  buffer = industrial_params$buffer
)

industrial_locations <- sero_compute_optimal_locations(
  data,
  risk_categories = industrial_params$risk_categories,
  suitable_landuse = industrial_params$suitable_landuse,
  min_road_distance = industrial_params$min_road_distance,
  max_road_distance = industrial_params$max_road_distance,
  grid_size = industrial_params$grid_size,
  max_locations = industrial_params$max_locations
)

cat("Industrial scenario results:\n")
cat("- Hotspots found:", nrow(industrial_hotspots$hotspots), "\n")
cat("- Optimal locations:", nrow(industrial_locations$locations), "\n")

# ==============================================================================
# 3. PARAMETER SENSITIVITY ANALYSIS
# ==============================================================================

cat("\n=== Parameter Sensitivity Analysis ===\n")

# Test buffer sensitivity
cat("\nTesting buffer sensitivity (hotspot precision):\n")
buffer_values <- c(300, 500, 800, 1000, 1200, 1500)
buffer_results <- data.frame(
  buffer = buffer_values,
  hotspots = integer(length(buffer_values)),
  max_density = numeric(length(buffer_values))
)

for (i in seq_along(buffer_values)) {
  buf <- buffer_values[i]
  result <- sero_identify_hotspots(data$accident, buffer = buf)
  buffer_results$hotspots[i] <- nrow(result$hotspots)
  if (nrow(result$hotspots) > 0 && "density" %in% names(result$hotspots)) {
    buffer_results$max_density[i] <- max(result$hotspots$density, na.rm = TRUE)
  }
}

print(buffer_results)

# Test grid size sensitivity
cat("\nTesting grid size sensitivity (location precision):\n")
grid_values <- c(50, 75, 100, 125, 150, 200)
grid_results <- data.frame(
  grid_size = grid_values,
  locations = integer(length(grid_values)),
  processing_time = numeric(length(grid_values))
)

for (i in seq_along(grid_values)) {
  gs <- grid_values[i]
  start_time <- Sys.time()
  result <- sero_compute_optimal_locations(data, grid_size = gs, max_locations = 10)
  end_time <- Sys.time()
  
  grid_results$locations[i] <- nrow(result$locations)
  grid_results$processing_time[i] <- as.numeric(end_time - start_time)
}

print(grid_results)

# Test road distance sensitivity
cat("\nTesting road distance sensitivity:\n")
road_distance_scenarios <- list(
  "very_close" = c(100, 300),
  "close" = c(300, 600),
  "medium" = c(500, 1000),
  "far" = c(800, 1500),
  "very_far" = c(1200, 2000)
)

road_results <- data.frame(
  scenario = names(road_distance_scenarios),
  min_dist = integer(length(road_distance_scenarios)),
  max_dist = integer(length(road_distance_scenarios)),
  locations = integer(length(road_distance_scenarios))
)

for (i in seq_along(road_distance_scenarios)) {
  scenario <- names(road_distance_scenarios)[i]
  distances <- road_distance_scenarios[[scenario]]
  
  result <- sero_compute_optimal_locations(
    data,
    min_road_distance = distances[1],
    max_road_distance = distances[2],
    max_locations = 10
  )
  
  road_results$min_dist[i] <- distances[1]
  road_results$max_dist[i] <- distances[2]
  road_results$locations[i] <- nrow(result$locations)
}

print(road_results)

# ==============================================================================
# 4. OPTIMIZATION WORKFLOW
# ==============================================================================

cat("\n=== Optimization Workflow ===\n")

# Step 1: Data exploration
cat("Step 1: Data Exploration\n")
cat("Understanding your data characteristics:\n")

# Accident distribution
accident_summary <- table(data$accident$UKATEGORIE)
cat("Accident categories:\n")
print(accident_summary)

# Spatial extent
bbox <- sf::st_bbox(data$accident)
area_km2 <- (bbox$xmax - bbox$xmin) * (bbox$ymax - bbox$ymin) / 1000000
cat("Study area:", round(area_km2, 2), "km²\n")

# Accident density
density_per_km2 <- nrow(data$accident) / area_km2
cat("Accident density:", round(density_per_km2, 2), "accidents/km²\n")

# Step 2: Parameter selection based on data characteristics
cat("\nStep 2: Parameter Selection Guidelines\n")

if (density_per_km2 > 50) {
  cat("High density area detected (>50 accidents/km²)\n")
  cat("Recommended: Urban parameters (fine grid, small bandwidth)\n")
} else if (density_per_km2 > 20) {
  cat("Medium density area detected (20-50 accidents/km²)\n")
  cat("Recommended: Suburban parameters (medium grid, medium bandwidth)\n")
} else {
  cat("Low density area detected (<20 accidents/km²)\n")
  cat("Recommended: Rural parameters (coarse grid, large bandwidth)\n")
}

# Step 3: Iterative optimization
cat("\nStep 3: Iterative Optimization Process\n")
cat("1. Start with recommended parameters for your scenario\n")
cat("2. Run analysis and evaluate results\n")
cat("3. Adjust parameters based on results:\n")
cat("   - Too many hotspots? Increase bandwidth or min_events\n")
cat("   - Too few hotspots? Decrease bandwidth or min_events\n")
cat("   - Too many locations? Decrease max_locations or increase grid_size\n")
cat("   - Too few locations? Increase max_locations or decrease grid_size\n")
cat("   - Locations too close to roads? Increase min_road_distance\n")
cat("   - Locations too far from roads? Decrease max_road_distance\n")

# ==============================================================================
# 5. QUALITY ASSESSMENT FUNCTIONS
# ==============================================================================

cat("\n=== Quality Assessment Functions ===\n")

# Function to assess hotspot quality
assess_hotspot_quality <- function(hotspots, accidents) {
  if (nrow(hotspots$hotspots) == 0) {
    return("No hotspots found - consider decreasing bandwidth or min_events")
  }
  
  total_accidents <- nrow(accidents)
  hotspot_count <- nrow(hotspots$hotspots)
  
  if (hotspot_count > total_accidents * 0.1) {
    return("Too many hotspots - consider increasing bandwidth or min_events")
  } else if (hotspot_count < 3) {
    return("Too few hotspots - consider decreasing bandwidth or min_events")
  } else {
    return("Hotspot count appears reasonable")
  }
}

# Function to assess location quality
assess_location_quality <- function(locations, target_count = 10) {
  if (nrow(locations$locations) == 0) {
    return("No locations found - check land use and road distance parameters")
  }
  
  location_count <- nrow(locations$locations)
  
  if (location_count < target_count * 0.5) {
    return("Fewer locations than expected - consider relaxing constraints")
  } else if (location_count > target_count * 1.5) {
    return("More locations than expected - consider tightening constraints")
  } else {
    return("Location count appears reasonable")
  }
}

# Assess current results
cat("Assessing urban scenario quality:\n")
cat("- Hotspots:", assess_hotspot_quality(urban_hotspots, data$accident), "\n")
cat("- Locations:", assess_location_quality(urban_locations, 12), "\n")

cat("\nAssessing rural scenario quality:\n")
cat("- Hotspots:", assess_hotspot_quality(rural_hotspots, data$accident), "\n")
cat("- Locations:", assess_location_quality(rural_locations, 6), "\n")

cat("\nAssessing industrial scenario quality:\n")
cat("- Hotspots:", assess_hotspot_quality(industrial_hotspots, data$accident), "\n")
cat("- Locations:", assess_location_quality(industrial_locations, 5), "\n")

# ==============================================================================
# 6. PARAMETER TEMPLATES
# ==============================================================================

cat("\n=== Parameter Templates ===\n")

# Create reusable parameter templates
parameter_templates <- list(
  
  # Urban dense area template
  urban_dense = list(
    description = "Urban dense area with high accident density",
    hotspot_params = list(
      risk_categories = c(1, 2),
      bandwidth = 600,
      min_events = 5
    ),
    location_params = list(
      risk_categories = c(1, 2),
      suitable_landuse = c("residential", "commercial"),
      min_road_distance = 300,
      max_road_distance = 700,
      grid_size = 50,
      max_locations = 12
    ),
    route_params = list(
      max_routes = 25,
      max_distance = 8000
    )
  ),
  
  # Suburban area template
  suburban = list(
    description = "Suburban area with medium accident density",
    hotspot_params = list(
      risk_categories = c(1, 2),
      bandwidth = 800,
      min_events = 4
    ),
    location_params = list(
      risk_categories = c(1, 2),
      suitable_landuse = c("residential", "commercial", "industrial"),
      min_road_distance = 400,
      max_road_distance = 1000,
      grid_size = 75,
      max_locations = 8
    ),
    route_params = list(
      max_routes = 20,
      max_distance = 12000
    )
  ),
  
  # Rural area template
  rural = list(
    description = "Rural area with low accident density",
    hotspot_params = list(
      risk_categories = c(1, 2, 3),
      bandwidth = 1500,
      min_events = 3
    ),
    location_params = list(
      risk_categories = c(1, 2, 3),
      suitable_landuse = c("residential", "commercial", "industrial"),
      min_road_distance = 600,
      max_road_distance = 1500,
      grid_size = 150,
      max_locations = 6
    ),
    route_params = list(
      max_routes = 15,
      max_distance = 20000
    )
  ),
  
  # Industrial area template
  industrial = list(
    description = "Industrial area with high-risk accidents",
    hotspot_params = list(
      risk_categories = c(1, 2),
      bandwidth = 1000,
      min_events = 4
    ),
    location_params = list(
      risk_categories = c(1, 2),
      suitable_landuse = c("industrial", "commercial"),
      min_road_distance = 800,
      max_road_distance = 1500,
      grid_size = 125,
      max_locations = 5
    ),
    route_params = list(
      max_routes = 15,
      max_distance = 15000
    )
  )
)

# Display templates
cat("Available parameter templates:\n")
for (template_name in names(parameter_templates)) {
  template <- parameter_templates[[template_name]]
  cat("\n", template_name, ":\n")
  cat("  Description:", template$description, "\n")
  cat("  Grid size:", template$location_params$grid_size, "m\n")
  cat("  Bandwidth:", template$hotspot_params$bandwidth, "m\n")
  cat("  Max locations:", template$location_params$max_locations, "\n")
}

# ==============================================================================
# 7. TEMPLATE USAGE EXAMPLES
# ==============================================================================

cat("\n=== Template Usage Examples ===\n")

# Function to apply template
apply_template <- function(data, template) {
  cat("Applying template:", template$description, "\n")
  
  # Apply hotspot parameters
  hotspots <- do.call(sero_identify_hotspots, c(
    list(accidents = data$accident),
    template$hotspot_params
  ))
  
  # Apply location parameters
  locations <- do.call(sero_compute_optimal_locations, c(
    list(data = data),
    template$location_params
  ))
  
  # Apply route parameters
  routes <- do.call(sero_calculate_routes, c(
    list(locations = locations, accidents = data$accident),
    template$route_params
  ))
  
  return(list(
    hotspots = hotspots,
    locations = locations,
    routes = routes
  ))
}

# Example: Apply urban template
cat("Example: Applying urban template\n")
urban_template_result <- apply_template(data, parameter_templates$urban_dense)
cat("Results: Hotspots =", nrow(urban_template_result$hotspots$hotspots), 
    ", Locations =", nrow(urban_template_result$locations$locations),
    ", Routes =", nrow(urban_template_result$routes$routes), "\n")

# Example: Apply rural template
cat("Example: Applying rural template\n")
rural_template_result <- apply_template(data, parameter_templates$rural)
cat("Results: Hotspots =", nrow(rural_template_result$hotspots$hotspots), 
    ", Locations =", nrow(rural_template_result$locations$locations),
    ", Routes =", nrow(rural_template_result$routes$routes), "\n")

cat("\n=== Parameter Optimization Guide Complete ===\n")
cat("This guide provided:\n")
cat("- Scenario-based parameter recommendations\n")
cat("- Parameter sensitivity analysis\n")
cat("- Optimization workflow\n")
cat("- Quality assessment functions\n")
cat("- Reusable parameter templates\n")
cat("- Template usage examples\n")
cat("\nUse these guidelines to optimize parameters for your specific use case.\n")
