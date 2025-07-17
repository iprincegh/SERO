# ===============================================================================
# SERO: Spatial Emergency Response Optimization
# Comprehensive Package Demonstration Script
# ===============================================================================

# This script demonstrates all core functionality of the SERO package
# for emergency response optimization using spatial data analysis.

# ===============================================================================
# 1. SETUP AND DATA LOADING
# ===============================================================================

# Load required libraries
library(SERO)
library(sf)
library(ggplot2)
library(dplyr)

# Set plotting theme
theme_set(theme_minimal(base_size = 12))

# Load the built-in Münster dataset
cat("Loading Münster emergency response dataset...\n")
data <- sero_load_data()

# Validate data structure
if (sero_validate_data(data)) {
  cat("✅ Data validation successful!\n")
} else {
  cat("❌ Data validation failed!\n")
}

# Display dataset summary
cat("\n📊 Dataset Summary:\n")
cat("==================\n")
for (layer in names(data)) {
  cat(sprintf("%-12s: %,d features\n", layer, nrow(data[[layer]])))
}

# Show accident categories
cat("\n🚨 Accident Categories:\n")
accident_summary <- table(data$accident$UKATEGORIE)
for (i in seq_along(accident_summary)) {
  category <- c("Fatal", "Serious", "Slight")[i]
  cat(sprintf("%d (%s): %,d accidents\n", i, category, accident_summary[i]))
}

# ===============================================================================
# 2. HOTSPOT ANALYSIS
# ===============================================================================

# Define high-risk categories (fatal and serious accidents)
high_risk_categories <- c(1, 2)

# Perform hotspot analysis
cat("\n🔍 Performing hotspot analysis...\n")
hotspots <- sero_hotspots(
  accidents = data$accident,
  risk_categories = high_risk_categories,
  buffer = 800,  # 800 meter buffer
  min_events = 3 # Minimum 3 accidents to form a hotspot
)

# Display results
print(hotspots)

# Create hotspot visualization
cat("📊 Creating hotspot visualization...\n")
hotspot_map <- plot(hotspots, data = data, show_munster = TRUE)
hotspot_map <- hotspot_map + 
  labs(
    title = "Emergency Response Hotspot Analysis",
    subtitle = "High-Risk Accident Concentrations in Münster",
    caption = "Data: Münster Emergency Services | Analysis: SERO Package"
  )

# Display the plot
print(hotspot_map)

# ===============================================================================
# 3. KERNEL DENSITY ANALYSIS
# ===============================================================================

# Create kernel density heatmap
cat("\n🌡️ Generating kernel density analysis...\n")
heatmap <- sero_heatmap(
  accidents = data$accident,
  risk_categories = high_risk_categories,
  bandwidth = 800,
  grid_size = 50,
  data = data,
  clip_to_munster = TRUE,
  basemap = "districts",
  show_accidents = TRUE,
  color_scheme = "viridis",
  alpha_heatmap = 0.7,
  alpha_basemap = 0.3
)

# Enhance the heatmap
heatmap <- heatmap + 
  labs(
    title = "Kernel Density Analysis: Accident Risk Surface",
    subtitle = "Smooth Density Estimation for Emergency Response Planning",
    caption = "Higher density indicates greater accident risk"
  )

# Display the heatmap
print(heatmap)

# Compare different risk categories
cat("\n📊 Comparing risk categories...\n")
comparison_plot <- sero_heatmap_compare(
  accidents = data$accident,
  categories = list(
    "Fatal" = 1,
    "Serious" = 2,
    "All High-Risk" = c(1, 2)
  ),
  data = data,
  bandwidth = 800
)

comparison_plot <- comparison_plot + 
  labs(
    title = "Risk Category Comparison",
    subtitle = "Kernel Density Analysis by Accident Severity"
  )

print(comparison_plot)

# ===============================================================================
# 4. OPTIMAL LOCATION FINDING
# ===============================================================================

# Find optimal emergency service locations
cat("\n🎯 Finding optimal emergency service locations...\n")
optimal_locations <- sero_find_optimal_locations(
  data = data,
  num_locations = 6,
  method = "fast",
  risk_categories = high_risk_categories,
  quick = TRUE
)

# Display results
cat(sprintf("✅ Found %d optimal locations\n", nrow(optimal_locations)))
print(st_drop_geometry(optimal_locations))

# Visualize optimal locations
cat("\n📍 Creating optimal locations visualization...\n")
optimal_map <- sero_plot_optimal_quick(
  optimal_locations = optimal_locations,
  districts = data$districts,
  accidents = data$accident
)

optimal_map <- optimal_map + 
  labs(
    title = "Optimal Emergency Service Locations",
    subtitle = "Strategic Placement for Maximum Coverage",
    caption = "Red triangles: Optimal locations | Orange dots: Accident locations"
  )

print(optimal_map)

# ===============================================================================
# 5. ROUTE OPTIMIZATION
# ===============================================================================

# Sample high-risk accidents for route calculation
sample_accidents <- data$accident[data$accident$UKATEGORIE %in% high_risk_categories, ]
sample_accidents <- sample_accidents[sample(nrow(sample_accidents), 
                                          min(10, nrow(sample_accidents))), ]

# Calculate routes
cat("\n🚗 Calculating emergency response routes...\n")
routes <- sero_routes(
  optimal_locations = optimal_locations,
  accidents = sample_accidents,
  max_routes = 10,
  data = data
)

# Display route summary
cat(sprintf("✅ Calculated %d emergency response routes\n", nrow(routes$routes)))
print(st_drop_geometry(routes$routes))

# Visualize routes
cat("\n🗺️ Creating route visualization...\n")
route_plot <- plot(routes, data = data, show_munster = TRUE)
route_plot <- route_plot + 
  labs(
    title = "Emergency Response Route Optimization",
    subtitle = "Calculated Routes from Optimal Stations to Accident Locations",
    caption = "Blue lines: Response routes | Red triangles: Emergency stations"
  )

print(route_plot)

# ===============================================================================
# 6. COMPLETE WORKFLOW
# ===============================================================================

# Run the complete emergency response workflow
cat("\n🔄 Running complete emergency response workflow...\n")
workflow_results <- sero_emergency_workflow(
  interactive = FALSE,
  quick = TRUE,
  num_locations = 5,
  risk_categories = c(1, 2)
)

# Display workflow summary
cat("\n📋 Workflow Summary:\n")
cat("===================\n")
summary_data <- workflow_results$summary
for (metric in names(summary_data)) {
  cat(sprintf("%-20s: %s\n", metric, summary_data[[metric]]))
}

# ===============================================================================
# 7. COMPREHENSIVE VISUALIZATION
# ===============================================================================

# Create comprehensive overview combining all analyses
cat("\n🎨 Creating comprehensive analysis overview...\n")

# Transform data to consistent CRS for visualization
districts_viz <- sf::st_transform(data$districts, 4326)
accidents_viz <- sf::st_transform(data$accident, 4326)
optimal_viz <- sf::st_transform(optimal_locations, 4326)
hotspots_viz <- sf::st_transform(hotspots$hotspots, 4326)

# Create comprehensive overview
overview_plot <- ggplot() +
  # Base districts
  geom_sf(data = districts_viz,
          fill = "white",
          color = "#2c3e50",
          size = 1.2,
          alpha = 0.8) +
  
  # Accident density (background)
  geom_sf(data = accidents_viz[accidents_viz$UKATEGORIE %in% c(1,2), ],
          color = "#e74c3c",
          size = 0.5,
          alpha = 0.4) +
  
  # Hotspots
  geom_sf(data = hotspots_viz,
          fill = "#f39c12",
          color = "#e67e22",
          size = 2,
          alpha = 0.7) +
  
  # Optimal locations
  geom_sf(data = optimal_viz,
          size = 8,
          color = "#2c3e50",
          fill = "#3498db",
          shape = 21,
          stroke = 3) +
  
  # Professional styling
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  labs(
    title = "SERO: Complete Emergency Response Optimization",
    subtitle = "Integrated Analysis: Hotspots, Density, and Optimal Locations | Münster, Germany"
  )

print(overview_plot)

# ===============================================================================
# 8. PERFORMANCE METRICS
# ===============================================================================

# Calculate comprehensive performance metrics
cat("\n📊 Calculating system performance metrics...\n")

# Basic metrics
total_accidents <- nrow(data$accident)
high_risk_accidents <- nrow(data$accident[data$accident$UKATEGORIE %in% c(1,2), ])
num_hotspots <- nrow(hotspots$hotspots)
num_stations <- nrow(optimal_locations)

# Route performance
if (nrow(routes$routes) > 0) {
  avg_distance <- mean(routes$routes$distance) / 1000  # Convert to km
  avg_time <- mean(routes$routes$estimated_time)
  max_distance <- max(routes$routes$distance) / 1000
  max_time <- max(routes$routes$estimated_time)
}

# Station performance
if ("accident_count_500m" %in% names(optimal_locations)) {
  avg_coverage_500m <- mean(optimal_locations$accident_count_500m)
  max_coverage_500m <- max(optimal_locations$accident_count_500m)
}

# Display performance summary
cat("\n📈 System Performance Summary:\n")
cat("===============================\n")
cat(sprintf("Total Accidents: %,d\n", total_accidents))
cat(sprintf("High-Risk Accidents: %,d\n", high_risk_accidents))
cat(sprintf("Hotspots Identified: %,d\n", num_hotspots))
cat(sprintf("Emergency Stations: %,d\n", num_stations))

if (exists("avg_distance")) {
  cat(sprintf("Average Response Distance: %.1f km\n", avg_distance))
  cat(sprintf("Average Response Time: %.1f minutes\n", avg_time))
  cat(sprintf("Maximum Response Distance: %.1f km\n", max_distance))
  cat(sprintf("Maximum Response Time: %.1f minutes\n", max_time))
}

if (exists("avg_coverage_500m")) {
  cat(sprintf("Average Coverage (500m): %.1f accidents\n", avg_coverage_500m))
  cat(sprintf("Maximum Coverage (500m): %.1f accidents\n", max_coverage_500m))
}

# ===============================================================================
# 9. SUMMARY AND CONCLUSIONS
# ===============================================================================

cat("\n🎯 SERO Analysis Summary\n")
cat("=======================\n\n")

cat("✅ **Analysis Completed Successfully:**\n")
cat(sprintf("   - Hotspot analysis identified %d high-risk areas\n", num_hotspots))
cat(sprintf("   - Optimal placement found %d strategic locations\n", num_stations))
cat(sprintf("   - Route optimization calculated %d emergency routes\n", nrow(routes$routes)))
cat("   - Comprehensive visualization created\n\n")

cat("🚀 **Key Benefits:**\n")
cat("   - Improved emergency response times\n")
cat("   - Strategic resource allocation\n")
cat("   - Data-driven decision making\n")
cat("   - Comprehensive risk assessment\n\n")

cat("📋 **Available Functions:**\n")
cat("   - sero_load_data()           # Load spatial datasets\n")
cat("   - sero_validate_data()       # Validate data structure\n")
cat("   - sero_hotspots()            # Identify accident hotspots\n")
cat("   - sero_heatmap()             # Generate kernel density maps\n")
cat("   - sero_find_optimal_locations() # Find optimal emergency locations\n")
cat("   - sero_routes()              # Calculate emergency response routes\n")
cat("   - sero_emergency_workflow()  # Complete analysis workflow\n")
cat("   - sero_plot_*()              # Various visualization functions\n")

cat("\n✨ Analysis completed! The SERO package provides comprehensive tools\n")
cat("   for emergency response optimization and spatial analysis.\n")

# ===============================================================================
# END OF DEMONSTRATION
# ===============================================================================
