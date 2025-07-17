# SERO: Spatial Emergency Response Optimization

[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

SERO is an R package for spatial emergency response optimization, designed to help researchers and practitioners analyze accident patterns, identify hotspots, and optimize emergency service locations using existing spatial data.

## ✨ Core Features

- **🎯 Hotspot Analysis**: Identify accident concentration areas using spatial clustering
- **�️ Kernel Density Analysis**: Generate smooth risk surfaces for strategic planning
- **� Risk Parameter Visualization**: Plot various accident risk metrics and patterns
- **� Optimal Location Finding**: Determine best emergency service locations using k-means optimization
- **� Route Calculation**: Calculate routes to optimal locations using existing accident data
- **� Performance Metrics**: Comprehensive analysis of coverage and response times

## 🚀 Package Status
- ✅ **Package Version**: 0.1.0 - Simplified and Clean
- ✅ **Core Functions**: Hotspot analysis, kernel density, optimal locations, routing
- ✅ **No Interactive Dependencies**: Focused on core analytical functionality
- ✅ **Clean Package Structure**: 6 core R files, comprehensive documentation

## Installation

You can install the development version of SERO from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install SERO from GitHub
devtools::install_github("iprincegh/SERO")
```

## Dependencies

SERO depends on several R packages for spatial analysis and visualization:

- `sf` - Simple Features for R
- `dplyr` - Data manipulation
- `ggplot2` - Data visualization
- `rlang` - Programming tools
- `magrittr` - Pipe operators

## ⚡ Quick Start

```r
library(SERO)

# Load the built-in Münster dataset
data <- sero_load_data()

# Run complete emergency response workflow
results <- sero_emergency_workflow(
  interactive = FALSE,
  quick = TRUE,
  num_locations = 5,
  risk_categories = c(1, 2)  # Fatal and serious accidents
)

# Analyze accident hotspots
hotspots <- sero_hotspots(
  accidents = data$accident,
  risk_categories = c(1, 2),
  buffer = 800,
  min_events = 3
)

# Generate kernel density heatmap
heatmap <- sero_heatmap(
  accidents = data$accident,
  risk_categories = c(1, 2),
  bandwidth = 800,
  data = data
)

# Find optimal emergency service locations
optimal_locations <- sero_find_optimal_locations(
  data = data,
  num_locations = 6,
  method = "fast",
  risk_categories = c(1, 2)
)

# Calculate routes to optimal locations
sample_accidents <- data$accident[1:10, ]
routes <- sero_routes(
  optimal_locations = optimal_locations,
  accidents = sample_accidents,
  data = data
)
```

## Main Functions

### Core Analysis Functions

- `sero_load_data()` - Load built-in Münster spatial dataset
- `sero_validate_data()` - Validate spatial data structure
- `sero_hotspots()` - Identify accident hotspots using spatial clustering
- `sero_heatmap()` - Generate kernel density maps and risk surfaces
- `sero_find_optimal_locations()` - Find optimal emergency service locations
- `sero_routes()` - Calculate emergency response routes
- `sero_emergency_workflow()` - Complete analysis workflow

### Visualization Functions

- `sero_plot_accidents()` - Plot accident locations with severity filtering
- `sero_plot_optimal_quick()` - Quick visualization of optimal locations
- `sero_heatmap_compare()` - Compare different risk categories
- `plot.sero_hotspots()` - Plot hotspot analysis results
- `plot.sero_routes()` - Visualize emergency response routes

### Utility Functions

- `sero_heatmap_presets()` - Predefined heatmap configurations
- `print.sero_hotspots()` - Print hotspot analysis summary
- `print.sero_routes()` - Print route analysis summary

## Examples

### Comprehensive Analysis Workflow

```r
# Load Münster emergency response data
data <- sero_load_data()

# Validate data structure
if (sero_validate_data(data)) {
  cat("✅ Data validation successful!")
}

# Complete workflow analysis
results <- sero_emergency_workflow(
  interactive = FALSE,
  quick = TRUE,
  num_locations = 5,
  risk_categories = c(1, 2)
)

# Access results
print(results$optimal_locations)
print(results$accident_map)
print(results$optimal_map)
```

### Hotspot Analysis

```r
# Analyze accident hotspots
hotspots <- sero_hotspots(
  accidents = data$accident,
  risk_categories = c(1, 2),  # Fatal and serious accidents
  buffer = 800,               # 800 meter buffer
  min_events = 3              # Minimum 3 accidents per hotspot
)

# Visualize hotspots
hotspot_map <- plot(hotspots, data = data, show_munster = TRUE)
print(hotspot_map)

# Print summary
print(hotspots)
```

### Kernel Density Analysis

```r
# Generate kernel density heatmap
heatmap <- sero_heatmap(
  accidents = data$accident,
  risk_categories = c(1, 2),
  bandwidth = 800,
  grid_size = 50,
  data = data,
  clip_to_munster = TRUE,
  color_scheme = "viridis"
)

# Display heatmap
print(heatmap)

# Compare risk categories
comparison <- sero_heatmap_compare(
  accidents = data$accident,
  categories = list(
    "Fatal" = 1,
    "Serious" = 2,
    "All High-Risk" = c(1, 2)
  ),
  data = data
)
print(comparison)
```

### Optimal Location Finding

```r
# Find optimal emergency service locations
optimal_locations <- sero_find_optimal_locations(
  data = data,
  num_locations = 6,
  method = "fast",
  risk_categories = c(1, 2),
  quick = TRUE
)

# Visualize optimal locations
optimal_map <- sero_plot_optimal_quick(
  optimal_locations = optimal_locations,
  districts = data$districts,
  accidents = data$accident
)
print(optimal_map)

# Print performance metrics
print(optimal_locations)
```

### Route Optimization

```r
# Calculate routes from optimal locations to accident sites
high_risk_accidents <- data$accident[data$accident$UKATEGORIE %in% c(1, 2), ]
sample_accidents <- high_risk_accidents[1:8, ]

routes <- sero_routes(
  optimal_locations = optimal_locations,
  accidents = sample_accidents,
  max_routes = 8,
  data = data
)

# Visualize routes
route_map <- plot(routes, data = data, show_munster = TRUE)
print(route_map)

# Print route summary
print(routes)
```

## Documentation

For more detailed documentation, use:

```r
# View package documentation
help(package = "SERO")

# Get help for specific functions
?sero_hotspots
?sero_heatmap
?sero_find_optimal_locations
?sero_routes
?sero_emergency_workflow
```

## Data Format

SERO works with spatial data in Simple Features format:

- **Built-in Dataset**: Münster emergency response data (accidents, districts, roads, landuse)
- **Input Format**: sf objects (Simple Features for R)
- **Coordinate Systems**: Automatic CRS handling and transformation
- **Data Validation**: Built-in validation functions

## Comprehensive Examples

The package includes comprehensive demonstration files:

- `inst/examples/SERO_comprehensive_demo.Rmd` - Professional RMarkdown report
- `inst/examples/SERO_demo.R` - Complete R script demonstration
- `inst/examples/README.md` - Detailed usage instructions

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## License

This project is licensed under the MIT License.

## Support

For questions, issues, or support:

- Create an issue on [GitHub](https://github.com/iprincegh/SERO/issues)
- Contact the maintainer
- Check the comprehensive examples in `inst/examples/`

## Citation

If you use SERO in your research, please cite:

```
SERO: Spatial Emergency Response Optimization
R Package Version 0.1.0
https://github.com/iprincegh/SERO
```

---

*Developed for spatial emergency response optimization and analysis*
