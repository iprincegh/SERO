# SERO Package Examples - Interactive Routing Demonstrations

This directory contains comprehensive examples demonstrating the SERO package's interactive emergency response routing capabilities.

## 🚨 Interactive Routing Demos

### 1. `simple_routing_demo.R` - **RECOMMENDED STARTING POINT**
**Purpose**: Streamlined interactive routing demonstration with click-to-simulate functionality
**Features**:
- Clean, responsive web interface using Shiny and Leaflet
- Real-time emergency incident simulation via map clicks
- OSRM-powered routing with automatic fallback to straight-line approximations
- Road network and district boundary visualization
- Route history tracking and analytics
- Works with or without optional packages

**Quick Start**:
```r
library(SERO)
source(system.file("examples", "simple_routing_demo.R", package = "SERO"))
```

### 2. `interactive_routing_demo.R` - Advanced Dashboard
**Purpose**: Professional dashboard with comprehensive routing analytics
**Features**:
- Shinydashboard-based professional interface
- Multi-tab navigation (Map, History, Statistics, Help)
- Enhanced data tables with DT package
- Real-time performance metrics and visualizations
- Customizable routing parameters
- Advanced styling and responsive design

**Requirements**: Additional packages (`shinydashboard`, `DT`)
```r
install.packages(c("shinydashboard", "DT", "osrm"))
source(system.file("examples", "interactive_routing_demo.R", package = "SERO"))
```

## 🎯 Key Features of Interactive Routing

### Click-to-Simulate Emergency Response
- **Interactive Map**: Click anywhere to simulate an emergency incident
- **Nearest Station**: Automatically finds closest optimal emergency location
- **Real-time Routing**: Calculates actual road-based routes using OSRM
- **Visual Feedback**: Shows routes, stations, and incident locations with clear styling

### Advanced Routing Technology
- **OSRM Integration**: Real road network routing when internet available
- **Fallback System**: Straight-line approximations for offline use
- **Emergency Profiles**: Appropriate speeds for emergency vehicles
- **Multi-Modal**: Support for different routing scenarios

### Beautiful User Interface
- **OpenStreetMap Base**: High-quality interactive maps
- **Road Network Display**: Actual infrastructure visualization
- **Responsive Design**: Works on desktop and mobile
- **Professional Styling**: Clean, modern interface with intuitive controls

### Analytics and Insights
- **Route History**: Track all calculated emergency routes
- **Performance Metrics**: Distance, duration, and speed analytics
- **Statistical Analysis**: Response time patterns and coverage analysis
- **Export Capabilities**: Save routes and locations for further analysis

## 🚀 Getting Started

### Option 1: Quick Demo (Easiest)
```r
# Load SERO package
library(SERO)

# Run the simple demo - no additional packages needed
source(system.file("examples", "simple_routing_demo.R", package = "SERO"))

# Click on the map to simulate emergencies!
```

### Option 2: Enhanced Experience
```r
# Install optional packages for best experience
install.packages(c("osrm", "DT", "shinydashboard"))

# Run the advanced dashboard demo
source(system.file("examples", "interactive_routing_demo.R", package = "SERO"))
```

### Option 3: Custom Integration
```r
library(SERO)

# Load your data
data <- sero_load_data()

# Calculate optimal emergency locations
optimal_locs <- sero_find_optimal_locations(data, num_locations = 6)

# Create custom routing application
app <- run_emergency_routing_demo(data, n_locations = 6)
```

## 🛣️ How It Works

### 1. **Spatial Data Loading**
- Loads Münster road network, districts, and accident data
- Transforms coordinate systems for accurate calculations
- Prepares spatial indices for fast queries

### 2. **Optimal Location Calculation** 
- Uses kernel density estimation on accident data
- Applies multi-criteria analysis for station placement
- Considers road access, population density, and coverage

### 3. **Interactive Routing**
- Click detection on leaflet map
- Nearest neighbor queries to find closest station
- OSRM API calls for real road routing
- Geometry creation and map updates

### 4. **Results Visualization**
- Route lines with distance/time popups
- Emergency station markers with details
- Incident markers with response information
- Layer controls for customization

## 📊 Use Cases and Applications

### Emergency Service Planning
```r
# Test different numbers of fire stations
demo_3_stations <- run_emergency_routing_demo(data, n_locations = 3)
demo_8_stations <- run_emergency_routing_demo(data, n_locations = 8)

# Compare coverage and response times
```

### Coverage Gap Analysis
- Click in various areas to test response times
- Identify neighborhoods with longer emergency response
- Plan additional stations to improve coverage

### Training and Education
- Demonstrate spatial optimization concepts
- Show real-world emergency planning challenges
- Interactive learning about GIS and routing

### Research and Development
- Test new routing algorithms
- Evaluate different station placement strategies
- Analyze emergency response patterns

## 🔧 Technical Requirements

### Essential (Automatic)
- `SERO` - Main package with spatial analysis functions
- `shiny` - Web application framework for interactivity
- `leaflet` - Interactive mapping and visualization
- `sf` - Spatial data processing and geometry handling

### Optional Enhancements
- `osrm` - Real road routing (highly recommended for accuracy)
- `DT` - Enhanced data tables with search and filtering
- `shinydashboard` - Professional dashboard layouts and styling

### Internet Connection
- **OSRM Routing**: Requires internet for real road routing
- **Map Tiles**: OpenStreetMap tiles benefit from internet
- **Offline Mode**: Falls back to straight-line routing automatically

## 🎨 Customization Options

### Map Styling
```r
# Add custom tile providers in the demo code
addProviderTiles(providers$Stamen.Terrain, group = "Terrain")
addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
```

### Routing Parameters
```r
# Modify emergency vehicle speeds
emergency_speed <- 40  # km/h instead of default 30

# Adjust maximum routing distance
max_route_distance <- 25  # km
```

### UI Themes
- Modify CSS in demo files for custom colors and styling
- Add new sidebar controls for additional parameters
- Integrate custom icons and markers

## 📈 Demo Data: Münster, Germany

The interactive demos use real spatial data from Münster:
- **40,175 road segments** from OpenStreetMap
- **5,695 historical traffic accidents** with severity coding
- **6 administrative districts** with boundaries
- **13,284 land use polygons** for context

This provides a realistic testing environment for emergency response optimization.

## 🐛 Troubleshooting

### OSRM Connection Issues
```
Error: Could not connect to OSRM server
```
**Solution**: Demo automatically falls back to straight-line routing

### Package Installation
```r
# Check if packages are available
if (!requireNamespace("osrm")) {
  message("OSRM not available - using approximations")
}
```

### Performance Optimization
- Road network sampling for better rendering performance
- Spatial indexing for fast nearest-neighbor queries
- Reactive updates to minimize calculations

## 📞 Next Steps

After exploring the interactive demos:

1. **Read the routing analysis**: See `ROUTING_ANALYSIS.md` for technical details
2. **Explore the main examples**: Check `munster_demo.R` for comprehensive workflow
3. **Build custom applications**: Use the demo code as a starting framework
4. **Contribute improvements**: Enhance routing algorithms or UI features

---

**🚨 Ready to see emergency response optimization in action?**

Start with the simple demo and click around the map to explore how SERO optimizes emergency response routing!
- Complete analysis workflow
- Step-by-step analysis
- Custom parameter examples
- Visualization
- Detailed results analysis
- Scenario comparison
- Multi-criteria analysis summary

**Run with**: `source("munster_demo.R")`

### 2. `basic_workflow.R` - Basic Usage Guide
**Purpose**: Demonstrates basic usage with default parameters
**Contents**:
- Simple workflow with default parameters
- Step-by-step analysis
- Basic visualization
- Results saving

**Best for**: First-time users learning the package basics
**Run with**: `source("basic_workflow.R")`

### 3. `advanced_parameters.R` - Parameter Customization
**Purpose**: Shows how to customize analysis parameters
**Contents**:
- Custom risk categories
- Land use preferences
- Spatial analysis parameters
- Route calculation settings
- Parameter sensitivity analysis

**Best for**: Users who need to customize analysis parameters
**Run with**: `source("advanced_parameters.R")`

### 4. `individual_functions.R` - Function-by-Function Guide
**Purpose**: Detailed examples of each SERO function
**Contents**:
- Individual function usage
- Parameter explanations
- Multiple scenarios
- Function comparisons

**Best for**: Users who want to understand each function in detail
**Run with**: `source("individual_functions.R")`

### 5. `parameter_optimization.R` - Parameter Optimization Guide
**Purpose**: Guidance on optimizing parameters for different scenarios
**Contents**:
- Scenario-based parameter recommendations
- Parameter sensitivity analysis
- Quality assessment functions
- Reusable parameter templates

**Best for**: Users who need to optimize parameters for specific use cases
**Run with**: `source("parameter_optimization.R")`

### 6. `custom_analysis.R` - Advanced Custom Analysis
**Purpose**: Advanced analysis workflows and custom combinations
**Contents**:
- Comparative analysis
- Multi-scale analysis
- Custom scoring systems
- Constraint-based optimization
- Comprehensive reporting

**Best for**: Advanced users creating custom analysis workflows
**Run with**: `source("custom_analysis.R")`

## Quick Start

1. **New to SERO?** Start with `basic_workflow.R`
2. **Need to customize parameters?** Use `advanced_parameters.R`
3. **Want to understand functions?** Try `individual_functions.R`
4. **Need to optimize for your area?** Use `parameter_optimization.R`
5. **Creating custom analysis?** Use `custom_analysis.R`
6. **Complete demonstration?** Run `munster_demo.R`

## Running Examples

### From R Console
```r
# Load the package
library(SERO)

# Run specific example
source(system.file("examples", "basic_workflow.R", package = "SERO"))
```

### Copy to Local Directory
```r
# Copy all examples to your working directory
file.copy(system.file("examples", package = "SERO"), ".", recursive = TRUE)

# Run from local files
source("examples/basic_workflow.R")
```

## Example Data

All examples use the built-in Munster dataset, which includes:
- **Accidents**: Point data with accident severity categories
- **Roads**: Road network for accessibility analysis
- **Land Use**: Land use classifications
- **Population**: Population density data

## Parameter Quick Reference

### Common Parameters
- `risk_categories`: `c(1, 2)` for serious accidents, `c(1, 2, 3)` for all
- `suitable_landuse`: `c("residential", "commercial", "industrial")`
- `bandwidth`: 500-2000m for hotspot analysis
- `grid_size`: 50-200m for location analysis
- `max_locations`: 5-15 locations typically
- `max_routes`: 10-30 routes typically

### Scenario-Specific Recommendations
- **Urban Dense**: Small grid (50m), small bandwidth (600m), many locations (12-15)
- **Rural Sparse**: Large grid (150m), large bandwidth (1500m), few locations (5-8)
- **Industrial**: Medium grid (125m), medium bandwidth (1000m), moderate locations (6-10)

## Getting Help

- Use `?function_name` for help on specific functions
- Check the USER_GUIDE.md for comprehensive documentation
- Review example comments for parameter explanations
- Start with simpler examples and progress to more complex ones

## Tips

1. Always start with `basic_workflow.R` to understand the fundamentals
2. Modify parameters gradually to see their effects
3. Use `plot()` functions to visualize results
4. Save results using `sf::st_write()` for further analysis
5. Check the console output for analysis summaries and recommendations
