# SERO: Spatial Emergency Response Optimization

[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

SERO is an R package for spatial emergency response optimization, designed to help researchers and practitioners analyze spatial data, identify hotspots, and optimize routing solutions for various applications.

## Features

- **Enhanced Hotspot Analysis**: Customizable intensity levels (low, medium, high) with kernel density estimation
- **Advanced Optimal Location Finding**: Multiple algorithms (k-means, grid-based, density-based, hybrid)
- **Interactive Emergency Routing**: Click-to-simulate accident locations with nearest service routing
- **Comprehensive Spatial Visualization**: Munster district mapping with landuse integration
- **Multi-layered Data Support**: Accidents, districts, roads, landuse, and population data
- **Performance Metrics**: Distance analysis, coverage statistics, and response time estimation

## Installation

You can install the development version of SERO from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install SERO from GitHub (main branch - default)
devtools::install_github("iprincegh/SERO")

# Or install from master branch
devtools::install_github("iprincegh/SERO@master")
```

## Dependencies

SERO depends on several R packages for spatial analysis and visualization:

- `sf` - Simple Features for R
- `dplyr` - Data manipulation
- `ggplot2` - Data visualization
- `leaflet` - Interactive maps
- `DT` - Interactive tables
- `shiny` - Web applications
- `shinydashboard` - Dashboard framework
- `plotly` - Interactive plots

## Quick Start

```r
library(SERO)

# Load sample data (includes accidents, districts, roads, landuse, population)
data <- sero_load_data()

# Enhanced hotspot analysis with customizable intensity
hotspots <- sero_hotspot_analysis(data, intensity = "medium", include_landuse = TRUE)
print(hotspots$plot)

# Find optimal emergency service locations using advanced algorithms
optimal_result <- sero_optimal_locations(data, num_locations = 3, method = "hybrid")
print(optimal_result$plot)

# Interactive emergency routing (click on map to simulate new accidents)
interactive_map <- create_interactive_routing_map(data)

# Simulate emergency response to a new accident
route_result <- sero_emergency_routing(
  data, 
  new_accident_coords = c(longitude, latitude),
  routing_method = "road_network"
)
print(route_result$plot)
```

## Main Functions

### Enhanced Analysis Functions

- `sero_hotspot_analysis()` - Advanced hotspot detection with customizable intensity levels
- `sero_optimal_locations()` - Multi-algorithm optimal location finding (k-means, grid, density, hybrid)
- `sero_emergency_routing()` - Emergency response routing with multiple methods
- `create_interactive_routing_map()` - Interactive map for accident simulation and routing

### Core Analysis Functions

- `sero_analyze()` - Comprehensive spatial analysis workflow
- `sero_main()` - Main analysis function with customizable parameters
- `sero_optimal()` - Optimization algorithms for routing problems
- `sero_routes()` - Route generation and analysis

### Visualization Functions

- `sero_visualization()` - Create static visualizations
- `sero_interactive()` - Launch interactive dashboard
- `create_leaflet_map()` - Generate interactive maps

### Data Functions

- `sero_load_data()` - Load built-in Munster dataset
- `load_sample_data()` - Load sample datasets
- `identify_hotspots()` - Hotspot detection and analysis

## Examples

### Enhanced Hotspot Analysis with Customizable Intensity

```r
# Load Munster city data
data <- sero_load_data()

# Analyze accident hotspots with different intensity levels
low_intensity <- sero_hotspot_analysis(data, intensity = "low", include_landuse = TRUE)
medium_intensity <- sero_hotspot_analysis(data, intensity = "medium", include_landuse = TRUE)
high_intensity <- sero_hotspot_analysis(data, intensity = "high", include_landuse = TRUE)

# View results
print(medium_intensity$plot)
print(medium_intensity$stats)
```

### Advanced Optimal Location Finding

```r
# Find optimal emergency service locations using different algorithms
kmeans_result <- sero_optimal_locations(data, num_locations = 3, method = "kmeans")
density_result <- sero_optimal_locations(data, num_locations = 3, method = "density")
hybrid_result <- sero_optimal_locations(data, num_locations = 3, method = "hybrid")

# Compare performance metrics
print(hybrid_result$performance)
print(hybrid_result$plot)
```

### Interactive Emergency Routing

```r
# Create interactive map for accident simulation
interactive_map <- create_interactive_routing_map(data)

# Simulate emergency response (replace coordinates with actual accident location)
route_result <- sero_emergency_routing(
  data = data,
  new_accident_coords = c(7.5, 51.9),  # Longitude, Latitude
  routing_method = "road_network",
  include_roads = TRUE
)

# View routing results
print(route_result$plot)
cat("Nearest service:", route_result$statistics$nearest_location_id)
cat("Distance:", route_result$statistics$distance_km, "km")
cat("Estimated time:", route_result$statistics$estimated_travel_time_min, "minutes")
```

### Interactive Dashboard

```r
# Launch interactive dashboard
sero_interactive(data)
```

## Documentation

For more detailed documentation, use:

```r
# View package documentation
help(package = "SERO")

# Get help for specific functions
?sero_analyze
?sero_interactive
?identify_hotspots
```

## Data Format

SERO works with spatial data in various formats:

- **Shapefiles** (.shp)
- **GeoJSON** (.geojson)
- **GeoPackage** (.gpkg)
- **Spatial data frames** (sf objects)

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## License

This project is licensed under the MIT License.

## Support

For questions, issues, or support:

- Create an issue on [GitHub](https://github.com/iprincegh/SERO/issues)
- Contact the maintainer

## Citation

If you use SERO in your research, please cite:

```
SERO: Spatial Emergency Response Optimization
R Package Version 1.0.0
https://github.com/iprincegh/SERO
```

---

*Developed with ❤️ for the spatial analysis community*
