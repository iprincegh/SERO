# SERO: Spatially Explicit Routing Optimization

[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

SERO is an R package for spatially explicit routing optimization, designed to help researchers and practitioners analyze spatial data, identify hotspots, and optimize routing solutions for various applications.

## Features

- **Spatial Data Analysis**: Load and analyze spatial datasets in various formats
- **Hotspot Detection**: Identify statistically significant spatial hotspots
- **Interactive Visualization**: Create interactive maps and visualizations
- **Route Optimization**: Optimize routing solutions for spatial problems
- **Comprehensive Analysis**: Integrated workflow for complete spatial analysis

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

# Load sample data
data <- load_sample_data()

# Perform comprehensive SERO analysis
results <- sero_analyze(data)

# Create interactive visualization
sero_interactive(results)

# Generate optimal routes
optimal_routes <- sero_optimal(data, method = "genetic")

# Visualize results
sero_visualization(results, type = "hotspots")
```

## Main Functions

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

- `load_sample_data()` - Load sample datasets
- `identify_hotspots()` - Hotspot detection and analysis

## Examples

### Basic Hotspot Analysis

```r
# Load your spatial data
data <- load_sample_data()

# Identify hotspots
hotspots <- identify_hotspots(data, method = "kernel")

# Visualize hotspots
sero_visualization(hotspots, type = "hotspots")
```

### Interactive Dashboard

```r
# Launch interactive dashboard
sero_interactive(data)
```

### Route Optimization

```r
# Optimize routes using genetic algorithm
optimal_solution <- sero_optimal(
  data = data,
  method = "genetic",
  population_size = 100,
  generations = 50
)

# Visualize optimized routes
sero_routes(optimal_solution)
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
SERO: Spatially Explicit Routing Optimization
R Package Version 1.0.0
https://github.com/iprincegh/SERO
```

---

*Developed with ❤️ for the spatial analysis community*
