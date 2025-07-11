# ==============================================================================
# SERO Package - Basic Workflow Example
# ==============================================================================
# This script demonstrates the basic workflow of the SERO package with
# step-by-step parameter customization and calculation examples.
# 
# The SERO package provides 3 core functions:
# 1. sero_identify_hotspots() - Point pattern analysis for accident hotspots
# 2. sero_compute_optimal_locations() - Multi-criteria analysis for service locations
# 3. sero_calculate_routes() - Route calculation with travel time estimation
# ==============================================================================

library(SERO)

# ==============================================================================
# 1. BASIC WORKFLOW - Default Parameters
# ==============================================================================

cat("=== SERO Package - Basic Workflow Example ===\n\n")

# Load built-in data
cat("Loading built-in spatial data...\n")
data <- sero_load_data()

# Display data summary
cat("Data loaded successfully!\n")
cat("Available layers:", paste(names(data), collapse = ", "), "\n")
cat("- Accidents:", nrow(data$accident), "records\n")
cat("- Roads:", nrow(data$roads), "segments\n")
if (!is.null(data$landuse)) {
  cat("- Land use:", nrow(data$landuse), "areas\n")
}
if (!is.null(data$population)) {
  cat("- Population:", nrow(data$population), "areas\n")
}

# ==============================================================================
# 2. STEP-BY-STEP ANALYSIS WITH DEFAULT PARAMETERS
# ==============================================================================

cat("\n=== Step-by-Step Analysis ===\n")

# Step 1: Identify hotspots (default parameters)
cat("\nStep 1: Identifying accident hotspots...\n")
hotspots <- sero_identify_hotspots(data$accident)
print(hotspots)

# Step 2: Compute optimal locations (default parameters)
cat("\nStep 2: Computing optimal service locations...\n")
locations <- sero_compute_optimal_locations(data)
print(locations)

# Step 3: Calculate routes (default parameters)
cat("\nStep 3: Calculating emergency routes...\n")
routes <- sero_calculate_routes(locations, data$accident)
print(routes)

# ==============================================================================
# 3. VISUALIZATION WITH DEFAULT PARAMETERS
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

# ==============================================================================
# 4. COMPLETE WORKFLOW WITH DEFAULT PARAMETERS
# ==============================================================================

cat("\n=== Complete Workflow (Alternative Method) ===\n")

# Run complete analysis in one step
complete_results <- sero_analyze()
print(complete_results)

# Access individual components
cat("\nAccessing individual components:\n")
cat("- Hotspots found:", nrow(complete_results$hotspots$hotspots), "\n")
cat("- Optimal locations:", nrow(complete_results$locations$locations), "\n")
cat("- Routes calculated:", nrow(complete_results$routes$routes), "\n")

# ==============================================================================
# 5. SAVE RESULTS (OPTIONAL)
# ==============================================================================

cat("\n=== Saving Results (Optional) ===\n")

# Uncomment the following lines to save results to files
# cat("Saving hotspots to file...\n")
# sf::st_write(hotspots$hotspots, "hotspots_basic.gpkg", delete_dsn = TRUE)
# 
# cat("Saving optimal locations to file...\n")
# sf::st_write(locations$locations, "locations_basic.gpkg", delete_dsn = TRUE)
# 
# cat("Saving routes to file...\n")
# sf::st_write(routes$routes, "routes_basic.gpkg", delete_dsn = TRUE)

cat("\n=== Basic Workflow Complete ===\n")
cat("This example used all default parameters.\n")
cat("See 'advanced_parameters.R' for custom parameter examples.\n")
