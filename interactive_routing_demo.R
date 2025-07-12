# SERO Interactive Routing Demo
# This script demonstrates the new interactive routing capabilities with OSRM

library(SERO)

# Load the built-in data
cat("Loading SERO data...\n")
data <- sero_load_data()

# Compute optimal locations (if not already done)
cat("Computing optimal emergency service locations...\n")
locations <- sero_optimal(data, max_locations = 8)

# Display summary
print(locations)

# Option 1: Basic interactive routing (leaflet only)
cat("\n=== Option 1: Basic Interactive Map ===\n")
cat("This creates a basic interactive map where you can click to simulate accidents.\n")
cat("Use this for simple visualization without full routing.\n\n")

# Create basic interactive map
basic_map <- sero_interactive_routing(data, locations)
print(basic_map)

# Option 2: Full interactive routing with OSRM (requires additional packages)
cat("\n=== Option 2: Full Interactive Routing with OSRM ===\n")
cat("This creates a Shiny app with real road-based routing using OSRM.\n")
cat("Note: Requires 'shiny', 'leaflet', and 'osrm' packages.\n\n")

# Check if required packages are available
packages_available <- all(c(
  requireNamespace("shiny", quietly = TRUE),
  requireNamespace("leaflet", quietly = TRUE),
  requireNamespace("osrm", quietly = TRUE)
))

if (packages_available) {
  cat("All required packages are available!\n")
  cat("Launching interactive routing app...\n")
  cat("Note: This uses the public OSRM server which has usage limits.\n")
  cat("For production use, set up your own OSRM server.\n\n")
  
  # Launch the interactive app
  # Uncomment the line below to run the interactive app
  # sero_launch_interactive(locations$locations)
  
  cat("To launch the interactive app, run:\n")
  cat("sero_launch_interactive(locations$locations)\n\n")
  
  # Option 3: Single route calculation with OSRM
  cat("=== Option 3: Single Route Calculation ===\n")
  cat("Calculate a single route from a point to the nearest optimal location.\n\n")
  
  # Example: Calculate route from Münster city center to nearest optimal location
  munster_center_lat <- 51.9606
  munster_center_lng <- 7.6261
  
  cat("Calculating route from Münster city center to nearest optimal location...\n")
  
  tryCatch({
    route <- sero_route_osrm(munster_center_lat, munster_center_lng, locations$locations)
    cat("Route calculated successfully!\n")
    cat("Distance:", round(route$distance, 2), "km\n")
    cat("Duration:", round(route$duration, 1), "minutes\n")
    cat("Average Speed:", round(route$distance / (route$duration / 60), 1), "km/h\n")
    
    # Plot the route
    cat("Plotting route...\n")
    print(plot(route))
    
  }, error = function(e) {
    cat("Error calculating route:", e$message, "\n")
    cat("This might be due to OSRM server availability or network issues.\n")
  })
  
} else {
  cat("Some required packages are not available.\n")
  cat("To install missing packages, run:\n")
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("install.packages('shiny')\n")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cat("install.packages('leaflet')\n")
  }
  if (!requireNamespace("osrm", quietly = TRUE)) {
    cat("install.packages('osrm')\n")
  }
}

# Save optimal locations for reuse
cat("\n=== Saving Optimal Locations ===\n")
cat("Saving optimal locations to GeoPackage for reuse...\n")
sero_save_locations(locations, "demo_optimal_locations.gpkg")

# Load optimal locations
cat("\nLoading optimal locations from file...\n")
loaded_locations <- sero_load_locations("demo_optimal_locations.gpkg")
cat("Loaded locations match original:", nrow(loaded_locations) == nrow(locations$locations), "\n")

cat("\n=== Demo Complete ===\n")
cat("Key functions for interactive routing:\n")
cat("- sero_interactive_routing(): Basic interactive map\n")
cat("- sero_launch_interactive(): Full Shiny app with OSRM routing\n")
cat("- sero_route_osrm(): Single route calculation\n")
cat("- sero_save_locations() / sero_load_locations(): Persist optimal locations\n")
