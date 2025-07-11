# SERO Package Examples - Index

This directory contains comprehensive examples demonstrating how to use the SERO package for emergency response optimization.

## Available Examples

### 1. `munster_demo.R` - Complete Package Demonstration
**Purpose**: Complete demonstration of the SERO package using the built-in Munster dataset
**Contents**:
- Data loading and exploration
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
