# SERO: Spatial Emergency Response Optimization

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

**SERO** (Spatial Emergency Response Optimization) is an R package that provides comprehensive tools for optimizing emergency service locations using spatial statistical methods. The package combines hotspot analysis, kernel density estimation, and spatial optimization algorithms to help emergency service planners make data-driven decisions about station placement and resource allocation.

## Key Features

- **Hotspot Detection**: Identify accident concentration areas using kernel density estimation
- **Risk Surface Analysis**: Generate smooth risk surfaces through kernel density heatmaps
- **Optimal Location Finding**: Calculate strategic emergency service locations using spatial optimization
- **Route Analysis**: Analyze emergency response routes with performance metrics
- **Comprehensive Visualization**: Professional-quality spatial visualizations for analysis and reporting
- **Real-World Application**: Demonstrated with traffic accident data from Münster, Germany

## Installation

You can install the development version of SERO from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("iprincegh/SERO")
```

## Quick Start

```r
library(SERO)

# Load the comprehensive spatial dataset
data <- sero_load_data()

# Perform hotspot analysis for high-risk accidents
hotspots <- sero_hotspots(data$accident, 
                         risk_categories = c(1, 2), 
                         buffer = 1000)

# Generate risk surface heatmap
heatmap <- sero_heatmap(data$accident,
                       risk_categories = c(1, 2),
                       bandwidth = 1000,
                       data = data)

# Find optimal emergency station locations
optimal_locations <- sero_find_optimal_locations(data, 
                                                num_locations = 5,
                                                risk_categories = c(1, 2))

# Analyze emergency response routes
routes <- sero_routes(optimal_locations, 
                     data$accident[1:10, ], 
                     data = data)

# View results
plot(hotspots)
plot(heatmap)
summary(optimal_locations)
summary(routes)
```

## Core Functions

### Data Loading
- `sero_load_data()`: Load comprehensive spatial dataset with accidents, boundaries, roads, and land use

### Spatial Analysis
- `sero_hotspots()`: Identify accident hotspots using kernel density estimation
- `sero_heatmap()`: Generate kernel density risk surface visualizations

### Optimization
- `sero_find_optimal_locations()`: Calculate optimal emergency service locations
- `sero_routes()`: Analyze emergency response routes and performance

### Visualization
- `plot()` methods for all analysis objects
- Professional-quality maps with customizable styling
- Individual plot displays for detailed analysis

## Methodology

### Optimal Location Calculation

SERO uses a sophisticated spatial optimization algorithm with these principles:

1. **Distance-Based Accessibility**: Calculates accessibility scores based on Euclidean distances from potential emergency stations to high-risk accident sites
2. **Weighted Risk Categories**: Fatal accidents receive higher priority weights than serious accidents
3. **Coverage Optimization**: Minimizes average distance while ensuring reasonable maximum response distances
4. **Iterative Placement**: Adjusts station positions iteratively to minimize accessibility scores
5. **Spatial Constraints**: Considers geographical constraints and practical feasibility

### Analysis Types

- **Comprehensive Analysis**: All accident severity levels (fatal, serious, light)
- **High-Risk Focus**: Fatal and serious accidents only
- **Critical Zones**: Fatal accidents exclusively
- **Performance Comparison**: Trade-offs between different approaches

## Vignette

The package includes a comprehensive vignette demonstrating the complete workflow:

```r
# View the main vignette
vignette("SERO-intro", package = "SERO")

# Or browse vignettes
browseVignettes("SERO")
```

The vignette covers:
- **Introduction and Motivation**: Data-driven emergency service planning
- **Optimal Location Logic**: Detailed explanation of the optimization algorithm
- **Data Exploration**: Dataset overview and accident severity analysis
- **Spatial Pattern Analysis**: Hotspot detection and kernel density analysis
- **Emergency Service Optimization**: Optimal location strategy and route analysis
- **Results Discussion**: Key findings and strategic recommendations

## Example Analysis Workflow

### 1. Load and Explore Data
```r
library(SERO)
data <- sero_load_data()

# Explore accident severity distribution
severity_counts <- table(data$accident$UKATEGORIE)
print(severity_counts)
```

### 2. Hotspot Analysis
```r
# Analyze different risk categories
hotspots_all <- sero_hotspots(data$accident, 
                             risk_categories = c(1, 2, 3), 
                             buffer = 1000)

hotspots_high_risk <- sero_hotspots(data$accident, 
                                   risk_categories = c(1, 2), 
                                   buffer = 1000)

hotspots_fatal <- sero_hotspots(data$accident, 
                               risk_categories = c(1), 
                               buffer = 800)

# Visualize results
plot(hotspots_high_risk)
summary(hotspots_high_risk)
```

### 3. Risk Surface Analysis
```r
# Generate heatmaps for different severity levels
heatmap_fatal <- sero_heatmap(data$accident,
                             risk_categories = c(1),
                             bandwidth = 1000,
                             data = data, 
                             color_scheme = "plasma",
                             basemap = "districts")

heatmap_combined <- sero_heatmap(data$accident,
                                risk_categories = c(1, 2),
                                bandwidth = 1200,
                                data = data, 
                                color_scheme = "magma",
                                basemap = "districts")

plot(heatmap_fatal)
plot(heatmap_combined)
```

### 4. Optimization Analysis
```r
# Compare different station configurations
optimal_3 <- sero_find_optimal_locations(data, num_locations = 3, risk_categories = c(1, 2))
optimal_5 <- sero_find_optimal_locations(data, num_locations = 5, risk_categories = c(1, 2))
optimal_7 <- sero_find_optimal_locations(data, num_locations = 7, risk_categories = c(1, 2))

# Performance comparison
performance_data <- data.frame(
  Stations = c(3, 5, 7),
  Avg_Coverage = c(mean(optimal_3$accessibility_score),
                   mean(optimal_5$accessibility_score), 
                   mean(optimal_7$accessibility_score)),
  Max_Distance = c(max(optimal_3$accessibility_score),
                   max(optimal_5$accessibility_score),
                   max(optimal_7$accessibility_score))
)

print(performance_data)
```

### 5. Route Analysis
```r
# Analyze emergency response routes
high_risk_accidents <- data$accident[data$accident$UKATEGORIE %in% c(1, 2), ]
sample_accidents <- high_risk_accidents[sample(nrow(high_risk_accidents), 12), ]

routes <- sero_routes(optimal_5, sample_accidents, max_routes = 12, data = data)

# Performance metrics
summary(routes)

# Visualize routes
plot(routes)
```

## Data Requirements

SERO works with spatial datasets containing:

- **Accident Data**: Point locations with severity categories
- **Administrative Boundaries**: District/area polygons
- **Road Networks**: LineString geometries
- **Land Use**: Polygon data for geographic context
- **Population Data**: Optional demographic information

All spatial data should be in a consistent coordinate reference system (CRS).

## Applications

- **Emergency Service Planning**: Optimize ambulance, fire station, and police station locations
- **Urban Planning**: Inform infrastructure development based on risk patterns
- **Traffic Safety**: Identify high-risk areas for targeted interventions
- **Resource Allocation**: Balance coverage efficiency with budget constraints
- **Policy Analysis**: Evaluate different emergency service deployment strategies

## Performance Considerations

- **Bandwidth Selection**: Affects hotspot sensitivity and heatmap smoothness
- **Buffer Zones**: Influence hotspot detection radius
- **Risk Categories**: Determine which accident types to prioritize
- **Station Numbers**: Trade-off between coverage and resource requirements

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## Citation

If you use SERO in your research, please cite:

```
SERO Development Team (2025). SERO: Spatial Emergency Response Optimization. 
R package version 0.1.0. https://github.com/iprincegh/SERO
```

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Contact

- **GitHub**: [https://github.com/iprincegh/SERO](https://github.com/iprincegh/SERO)
- **Issues**: [https://github.com/iprincegh/SERO/issues](https://github.com/iprincegh/SERO/issues)

## Acknowledgments

- Traffic accident data from Münster, Germany
- Built with R spatial packages: sf, ggplot2, and related ecosystem
- Inspired by spatial optimization research in emergency services
