# SERO: Spatial Emergency Response Optimization

A simple and focused R package for spatial emergency response optimization using built-in data. The package identifies accident hotspots, computes optimal emergency service locations, and calculates fastest routes to accident scenes.

## Features

- **Hotspot Analysis**: Point pattern analysis using spatstat (no interpolation)
- **Accident Heatmaps**: Continuous density visualization using kernel density estimation
- **Multi-Criteria Optimization**: 6 specific criteria for optimal location computation
- **Route Calculation**: Fastest routes to accident scenes
- **Interactive Routing**: Click-to-simulate accidents with real road-based routing using OSRM
- **Data Persistence**: Save/load optimal locations using GeoPackage format
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

# For interactive routing features, install optional packages
install.packages(c("shiny", "leaflet", "osrm"))
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

# Interactive routing (requires leaflet, shiny, osrm packages)
data <- sero_load_data()
locations <- sero_optimal(data)

# Option 1: Basic interactive map
map <- sero_interactive_routing(data, locations)

# Option 2: Full interactive app with OSRM routing
sero_launch_interactive(locations$locations)
```

## Step-by-Step Analysis

```r
# Load built-in data
data <- sero_load_data()

# Step 1: Identify hotspots using point pattern analysis
hotspots <- sero_hotspots(
  data$accident,
  risk_categories = c(1, 2),  # Fatal and serious accidents
  buffer = 1000,              # 1km buffer
  min_events = 5              # Minimum 5 accidents per hotspot
)

# Step 1.5: Create accident density heatmap
heatmap <- sero_heatmap(
  data$accident,
  bandwidth = 1000,           # 1km bandwidth for density estimation
  grid_size = 100,            # 100m grid resolution
  color_scheme = "viridis"    # Color scheme
)

# Step 2: Compute optimal locations using 6 criteria
locations <- sero_optimal(
  data,
  risk_categories = c(1, 2),
  min_road_distance = 500,    # Minimum 500m from roads
  max_road_distance = 1000,   # Maximum 1000m from roads
  grid_size = 100,            # 100m grid cells
  max_locations = 10          # Maximum 10 service locations
)

# Step 3: Calculate routes
routes <- sero_routes(locations, data$accident, max_routes = 20)
```

## Parameter Customization

The SERO package offers flexible parameter customization for optimal location analysis:

### Custom Scoring Weights

Customize the importance of different criteria:

```r
# Prioritize accident density
result <- sero_optimal(data, 
  weights = list(
    accident_weight = 0.6,
    population_weight = 0.2,
    road_weight = 0.2
  ))

# Prioritize population density
result <- sero_optimal(data, 
  weights = list(
    accident_weight = 0.2,
    population_weight = 0.6,
    road_weight = 0.2
  ))
```

### Custom Scoring Functions

Use built-in alternative scoring methods or create your own:

```r
# Exponential scoring (emphasizes high-accident areas)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_exponential)

# Threshold-based scoring (minimum requirements)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_threshold)

# Create your own scoring function
my_scoring <- function(data) {
  # Custom logic here
  return(scores)
}
result <- sero_optimal(data, custom_scoring_function = my_scoring)
```

## Visualization Options

### Accident Heatmaps

Create continuous density visualizations:

```r
# Basic heatmap
heatmap_basic <- sero_heatmap(data$accident)

# High-resolution heatmap
heatmap_detailed <- sero_heatmap(data$accident,
                                bandwidth = 500,
                                grid_size = 50,
                                color_scheme = "plasma")

# Heatmap with landuse context
heatmap_context <- sero_heatmap(data$accident,
                               data = data,
                               show_landuse = TRUE,
                               show_accidents = FALSE,
                               color_scheme = "inferno")
```

### Color Schemes Available
- **"viridis"**: Perceptually uniform (default)
- **"plasma"**: High contrast, vibrant
- **"inferno"**: Dark background, bright highlights  
- **"magma"**: Purple-to-yellow gradient

## Interactive Routing

The SERO package includes comprehensive interactive routing capabilities using OSRM (OpenStreetMap Routing Machine).

### Interactive Functions

```r
# Basic interactive map (requires leaflet)
map <- sero_interactive_routing(data, locations)

# Full interactive app with real routing (requires shiny, leaflet, osrm)
sero_launch_interactive(locations$locations)

# Single route calculation (requires osrm)
route <- sero_route_osrm(51.9606, 7.6261, locations$locations)
```

### Features
- **Click-to-simulate**: Click on map to simulate accident locations
- **Real road routing**: Uses OSRM for actual road-based routes
- **Nearest location**: Automatically finds closest optimal emergency location
- **Route visualization**: Shows route, distance, and estimated time
- **Background layers**: Optional display of roads, landuse, population data

### OSRM Server Options
- **Public server**: `http://router.project-osrm.org` (default, has usage limits)
- **Custom server**: Set up your own OSRM server for production use

```r
# Using custom OSRM server
sero_launch_interactive(locations$locations, 
                       osrm_server = "http://your-osrm-server.com")
```

### Data Persistence

```r
# Save optimal locations for reuse
sero_save_locations(locations, "emergency_locations.gpkg")

# Load and use saved locations
saved_locations <- sero_load_locations("emergency_locations.gpkg")
sero_launch_interactive(saved_locations)
```

See `INTERACTIVE_ROUTING_GUIDE.md` for comprehensive documentation and setup instructions.

## Data Requirements

The package uses built-in data from `inst/gpkg/dataset.gpkg` containing:

- **accident**: Accident points with UKATEGORIE column (1=fatal, 2=serious, 3=light)
- **roads**: Road network
- **landuse**: Land use polygons with fclass column
- **population**: Population density data
- **districts**: Administrative boundaries (optional)

## Dependencies

### Core Dependencies
- sf (>= 1.0.0)
- ggplot2 (>= 3.0.0)
- spatstat (>= 3.0.0)
- spatstat.geom (>= 3.0.0)
- spatstat.explore (>= 3.0.0)

### Optional Dependencies (for interactive routing)
- shiny (for interactive applications)
- leaflet (for interactive maps)
- osrm (for road-based routing)

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

### Interactive Routing
- `sero_interactive_routing()`: Basic interactive map with click functionality
- `sero_launch_interactive()`: Full Shiny app with OSRM routing
- `sero_route_osrm()`: Single route calculation using OSRM
- `sero_save_locations()`: Save optimal locations to GeoPackage
- `sero_load_locations()`: Load optimal locations from GeoPackage

### Visualization
All functions include plot() methods using ggplot2 and sf

## License

MIT License

## Citation

If you use this package in your research, please cite:

```
SERO: Spatial Emergency Response Optimization
Authors: Prince Oppong Boakye
Version: 0.1.0
URL: https://github.com/iprincegh/SERO
```
