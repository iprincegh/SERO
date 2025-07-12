# SERO Interactive Routing Guide

## Overview

The SERO package now includes comprehensive interactive routing capabilities that allow users to:

1. **Click on a map** to simulate accident locations
2. **Calculate real road-based routes** using OSRM (OpenStreetMap Routing Machine)
3. **Find the nearest optimal emergency location** automatically
4. **Visualize routes** on an interactive map

## Available Functions

### 1. `sero_interactive_routing(data, optimal_locations)`
- **Purpose**: Basic interactive map with click functionality
- **Requirements**: Only `leaflet` package
- **Features**: Click to simulate accidents, basic visualization
- **Use Case**: Simple demos, basic interaction

### 2. `sero_launch_interactive(optimal_locations, roads, landuse, population)`
- **Purpose**: Full-featured Shiny app with OSRM routing
- **Requirements**: `shiny`, `leaflet`, `osrm` packages
- **Features**: Real road-based routing, route information, background layers
- **Use Case**: Production applications, detailed analysis

### 3. `sero_route_osrm(lat, lng, optimal_locations)`
- **Purpose**: Calculate single route using OSRM
- **Requirements**: `osrm` package
- **Features**: Returns route with distance, duration, geometry
- **Use Case**: Programmatic route calculation, batch processing

## Installation Requirements

```r
# Core SERO package
devtools::install_local("path/to/SERO")

# Required packages for interactive routing
install.packages(c("shiny", "leaflet", "osrm"))
```

## Basic Usage

### Step 1: Load Data and Compute Optimal Locations
```r
library(SERO)

# Load built-in data
data <- sero_load_data()

# Compute optimal emergency service locations
locations <- sero_optimal(data, max_locations = 8)
```

### Step 2: Choose Your Routing Method

#### Option A: Basic Interactive Map
```r
# Create basic interactive map
map <- sero_interactive_routing(data, locations)
print(map)
```

#### Option B: Full Interactive App
```r
# Launch full Shiny app with OSRM routing
sero_launch_interactive(locations$locations)

# With background layers
sero_launch_interactive(locations$locations, 
                       roads = data$roads,
                       landuse = data$landuse)
```

#### Option C: Single Route Calculation
```r
# Calculate route from specific point
route <- sero_route_osrm(51.9606, 7.6261, locations$locations)
print(route)
plot(route)
```

## OSRM Setup and Configuration

### Using Public OSRM Server (Default)
- **URL**: `http://router.project-osrm.org`
- **Limitations**: Usage limits, may be slow, not suitable for production
- **Best for**: Testing, demos, low-volume usage

### Using Custom OSRM Server (Recommended for Production)
```r
# Set custom OSRM server
sero_launch_interactive(locations$locations, 
                       osrm_server = "http://your-osrm-server.com")

# Or for single route calculation
route <- sero_route_osrm(51.9606, 7.6261, locations$locations,
                        osrm_server = "http://your-osrm-server.com")
```

### Setting Up Your Own OSRM Server
1. **Download OpenStreetMap data** for your region
2. **Install OSRM** (Docker recommended)
3. **Process the data** with OSRM
4. **Run OSRM server** locally or on cloud

Example Docker setup:
```bash
# Download OSM data for Germany
wget https://download.geofabrik.de/europe/germany-latest.osm.pbf

# Run OSRM with Docker
docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/germany-latest.osm.pbf
docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-partition /data/germany-latest.osrm
docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-customize /data/germany-latest.osrm
docker run -t -i -p 5000:5000 -v "${PWD}:/data" osrm/osrm-backend osrm-routed --algorithm mld /data/germany-latest.osrm

# Use with SERO
route <- sero_route_osrm(51.9606, 7.6261, locations$locations,
                        osrm_server = "http://localhost:5000")
```

## Interactive App Features

### Map Controls
- **Emergency Locations**: Red circles showing optimal service locations
- **Background Layers**: Toggle roads, landuse, population data
- **Click Interaction**: Click anywhere to simulate an accident

### Route Information Panel
- **Distance**: Route distance in kilometers
- **Duration**: Estimated travel time in minutes
- **Average Speed**: Calculated average speed
- **Service Location**: ID of nearest optimal location

### Error Handling
- **No Route Found**: When OSRM cannot find a route (e.g., isolated areas)
- **Server Unavailable**: When OSRM server is not accessible
- **Network Issues**: Connection problems

## Data Persistence

### Saving Optimal Locations
```r
# Save for reuse (GeoPackage format recommended)
sero_save_locations(locations, "my_optimal_locations.gpkg")
```

### Loading Optimal Locations
```r
# Load previously computed locations
saved_locations <- sero_load_locations("my_optimal_locations.gpkg")

# Use in interactive routing
sero_launch_interactive(saved_locations)
```

## Advanced Usage

### Custom Styling
```r
# Create interactive map with custom optimal locations
custom_locations <- sero_optimal(data, 
                                max_locations = 12,
                                min_road_distance = 300,
                                max_road_distance = 1500)

sero_launch_interactive(custom_locations$locations)
```

### Batch Route Calculation
```r
# Calculate multiple routes programmatically
accident_points <- data.frame(
  lat = c(51.9606, 51.9500, 51.9700),
  lng = c(7.6261, 7.6100, 7.6400)
)

routes <- list()
for (i in 1:nrow(accident_points)) {
  routes[[i]] <- sero_route_osrm(
    accident_points$lat[i], 
    accident_points$lng[i], 
    locations$locations
  )
}
```

### Integration with Other Packages
```r
# Use with leaflet for custom visualizations
library(leaflet)

map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(locations$locations, 4326))

# Add OSRM routes
route <- sero_route_osrm(51.9606, 7.6261, locations$locations)
map <- map %>% addPolylines(data = route, color = "blue", weight = 4)
```

## Performance Considerations

### OSRM Server Performance
- **Local Server**: Fastest, no network latency
- **Cloud Server**: Good for distributed teams
- **Public Server**: Slowest, has usage limits

### Large Datasets
- **Optimal Locations**: Keep to reasonable number (< 20 for best performance)
- **Background Layers**: Simplify complex geometries for faster rendering
- **Route Caching**: Cache frequently used routes

### Memory Usage
- **Shiny App**: Monitor memory usage for long-running sessions
- **Large Routes**: OSRM can return detailed geometries (many points)
- **Background Layers**: Large landuse/population data can slow rendering

## Troubleshooting

### Common Issues

1. **"osrm package not available"**
   - Solution: `install.packages("osrm")`

2. **"OSRM server not reachable"**
   - Check internet connection
   - Verify server URL
   - Try different server

3. **"No route found"**
   - Click closer to roads
   - Check if points are in routable area
   - Verify data coverage

4. **App crashes or freezes**
   - Restart R session
   - Check available memory
   - Reduce complexity of background layers

### Debug Mode
```r
# Enable debug output
options(osrm.debug = TRUE)

# Check OSRM server status
osrm::osrmIsochrone(loc = c(7.6261, 51.9606), breaks = 1)
```

## Best Practices

1. **Use Your Own OSRM Server** for production applications
2. **Save Optimal Locations** to avoid recomputation
3. **Test with Small Datasets** first
4. **Monitor Network Usage** when using public servers
5. **Handle Errors Gracefully** in production apps
6. **Cache Routes** for frequently used paths
7. **Validate Coordinates** before routing

## Examples and Use Cases

### Emergency Response Planning
```r
# Set up emergency response system
locations <- sero_optimal(data, max_locations = 6)
sero_save_locations(locations, "emergency_stations.gpkg")

# Daily operations - load and use
stations <- sero_load_locations("emergency_stations.gpkg")
sero_launch_interactive(stations)
```

### Route Analysis
```r
# Analyze response times for different scenarios
scenarios <- data.frame(
  name = c("City Center", "Suburbs", "Industrial Area"),
  lat = c(51.9606, 51.9400, 51.9800),
  lng = c(7.6261, 7.5800, 7.6500)
)

for (i in 1:nrow(scenarios)) {
  route <- sero_route_osrm(scenarios$lat[i], scenarios$lng[i], locations$locations)
  cat(scenarios$name[i], "- Duration:", round(route$duration, 1), "minutes\n")
}
```

### Integration with Real-Time Data
```r
# Function to handle real-time accident reports
handle_accident <- function(lat, lng, optimal_locations) {
  route <- sero_route_osrm(lat, lng, optimal_locations)
  
  # Send to nearest emergency service
  service_id <- route$service_location_id
  duration <- route$duration
  
  # Log or send alert
  cat("Emergency at", lat, lng, "- Response time:", duration, "minutes\n")
  
  return(route)
}
```

This comprehensive guide provides everything needed to implement and use the new interactive routing features in the SERO package.
