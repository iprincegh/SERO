# SERO: Spatial Emergency Response Optimization

A simple and focused R package for spatial emergency response optimization using built-in data. The package identifies accident hotspots, computes optimal emergency service locations, and calculates fastest routes to accident scenes.

## Features

- **Hotspot Analysis**: Point pattern analysis using spatstat (no interpolation)
- **Multi-Criteria Optimization**: 6 specific criteria for optimal location computation
- **Route Calculation**: Fastest routes to accident scenes
- **Built-in Data**: Uses only data from inst/gpkg folder
- **R-Spatial Best Practices**: Uses sf, ggplot2, and spatstat

## 6 Multi-Criteria Analysis Criteria

The package implements exactly 6 criteria for optimal emergency service location computation:

1. **High-Risk Accidents**: Only accidents with fatalities (UKATEGORIE = 1) or serious injuries (UKATEGORIE = 2) are considered
2. **Suitable Land Use**: Optimal locations are restricted to residential, commercial, or industrial land use areas
3. **Proximity to Roads**: 
   - Minimum 500m from roads (avoid congestion)
   - Maximum 1000m from roads (ensure accessibility)
4. **Accident Density**: Accidents are grouped into 100m grid cells using ST_SnapToGrid equivalent
5. **Population Density**: Higher population density areas are prioritized
6. **Centroid Calculation**: ST_Centroid equivalent to find optimal location within each grid cell

## Installation

```r
# Install from source
devtools::install_local("path/to/SERO")
```

## Quick Start

```r
library(SERO)

# Load built-in data and run complete analysis
results <- sero_analyze()

# View results
print(results)

# Create visualizations
plot(results$hotspots)   # Accident hotspots
plot(results$locations)  # Optimal service locations
plot(results$routes)     # Emergency routes
```

## Step-by-Step Analysis

```r
# Load built-in data
data <- sero_load_data()

# Step 1: Identify hotspots using point pattern analysis
hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1, 2),  # Fatal and serious accidents
  bandwidth = 1000,           # 1km bandwidth
  min_events = 5              # Minimum 5 accidents per hotspot
)

# Step 2: Compute optimal locations using 6 criteria
locations <- sero_compute_optimal_locations(
  data,
  risk_categories = c(1, 2),
  suitable_landuse = c("residential", "commercial", "industrial"),
  min_road_distance = 500,    # Minimum 500m from roads
  max_road_distance = 1000,   # Maximum 1000m from roads
  grid_size = 100,            # 100m grid cells
  max_locations = 10          # Maximum 10 service locations
)

# Step 3: Calculate routes
routes <- sero_calculate_routes(locations, data$accident, max_routes = 20)
```

## Data Requirements

The package uses built-in data from `inst/gpkg/dataset.gpkg` containing:

- **accident**: Accident points with UKATEGORIE column (1=fatal, 2=serious, 3=light)
- **roads**: Road network
- **landuse**: Land use polygons with fclass column
- **population**: Population density data
- **districts**: Administrative boundaries (optional)

## Dependencies

- sf (>= 1.0.0)
- ggplot2 (>= 3.0.0)
- spatstat (>= 3.0.0)
- spatstat.geom (>= 3.0.0)
- spatstat.explore (>= 3.0.0)

## Methods

### Hotspot Identification
- Uses spatstat point pattern analysis
- Kernel density estimation with configurable bandwidth
- Local maxima detection for hotspot centers
- Severity weighting (fatal=3, serious=2)

### Multi-Criteria Optimal Location Computation
Implements exactly 6 criteria:
1. **High-Risk Accidents**: Filters for UKATEGORIE = 1 (fatal) or 2 (serious) only
2. **Suitable Land Use**: Restricts to residential, commercial, or industrial areas
3. **Proximity to Roads**: 500m minimum (avoid congestion), 1000m maximum (accessibility)
4. **Accident Density**: 100m grid cells using ST_SnapToGrid equivalent
5. **Population Density**: Higher density areas prioritized
6. **Centroid Calculation**: ST_Centroid equivalent for optimal location within grid cells

### Route Calculation
- Straight-line distance calculations
- Estimated travel times based on average speeds
- Nearest service location assignment

## Core Functions

### Data Loading and Validation
- `sero_load_data()`: Load built-in spatial data from inst/gpkg folder
- `sero_validate_data()`: Validate and standardize spatial data

### Analysis and Optimization
- `sero_analyze()`: Complete analysis workflow using 6 criteria
- `sero_identify_hotspots()`: Point pattern analysis for accident hotspots
- `sero_compute_optimal_locations()`: Multi-criteria optimal location computation
- `sero_calculate_routes()`: Calculate fastest routes to accident scenes

### Visualization
All functions include plot() methods using ggplot2 and sf

## License

MIT License

## Citation

If you use this package in your research, please cite:

```
SERO: Spatial Emergency Response Optimization
Authors: Prince Oppong Boakye, Samuel, Ahmad, Ahmed
Version: 0.1.0
URL: https://github.com/your-username/SERO
```
