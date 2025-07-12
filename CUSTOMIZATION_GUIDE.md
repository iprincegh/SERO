# SERO Package - Advanced Parameter Customization Guide
# =====================================================

## Overview

The SERO package now provides extensive parameter customization capabilities for optimal emergency response location analysis. This guide demonstrates how users can tailor the analysis to their specific needs and requirements.

## Key Customization Features

### 1. Flexible Scoring Weights
Users can now customize the importance of different criteria in the multi-criteria analysis:

```r
# Default balanced approach
result <- sero_optimal(data, 
  weights = list(
    accident_weight = 0.4,
    population_weight = 0.3,
    road_weight = 0.3
  ))

# Accident-focused analysis
result <- sero_optimal(data, 
  weights = list(
    accident_weight = 0.6,
    population_weight = 0.2,
    road_weight = 0.2
  ))

# Population-focused analysis
result <- sero_optimal(data, 
  weights = list(
    accident_weight = 0.2,
    population_weight = 0.6,
    road_weight = 0.2
  ))
```

### 2. Custom Scoring Functions
Replace the default linear scoring with advanced methods:

```r
# Exponential scoring (emphasizes high-accident areas)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_exponential)

# Threshold-based scoring (minimum requirements)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_threshold)

# Geometric mean scoring (balanced approach)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_geometric)

# Priority-based scoring (hierarchical)
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_priority)
```

### 3. Distance Constraints
Flexible road proximity requirements for different environments:

```r
# Urban analysis (closer to roads)
result <- sero_optimal(data, 
  min_road_distance = 200,
  max_road_distance = 600)

# Rural analysis (further from roads)
result <- sero_optimal(data, 
  min_road_distance = 800,
  max_road_distance = 1500)

# Mixed environment (default)
result <- sero_optimal(data, 
  min_road_distance = 500,
  max_road_distance = 1000)
```

### 4. Threshold-Based Filtering
Set minimum requirements for location consideration:

```r
# High-activity areas only
result <- sero_optimal(data, 
  min_accidents = 3,
  population_threshold = 100)

# Include all areas with any activity
result <- sero_optimal(data, 
  min_accidents = 1,
  population_threshold = 0)
```

### 5. Grid Resolution Control
Adjust analysis detail vs. computation speed:

```r
# High-detail analysis (slower)
result <- sero_optimal(data, grid_size = 50)

# Standard analysis (balanced)
result <- sero_optimal(data, grid_size = 100)

# Fast analysis (less detail)
result <- sero_optimal(data, grid_size = 200)
```

### 6. Land Use Customization
Control how existing land use areas are handled:

```r
# Avoid all land use areas
result <- sero_optimal(data, avoid_landuse = TRUE)

# Allow placement anywhere
result <- sero_optimal(data, avoid_landuse = FALSE)

# Avoid specific land use types
result <- sero_optimal(data, 
  avoid_landuse = TRUE,
  landuse_types = c("residential", "commercial"))
```

## Parameter Validation and Recommendations

### Parameter Validation
Check parameters before running analysis:

```r
# Validate parameters
validation <- sero_validate_params(
  data = data,
  grid_size = 100,
  weights = list(accident_weight = 0.4, population_weight = 0.3, road_weight = 0.3)
)

# Check if parameters are valid
if (validation$valid) {
  # Run analysis
  result <- sero_optimal(data, ...)
} else {
  # Fix errors first
  print(validation$errors)
}
```

### Parameter Recommendations
Get suggestions based on analysis context:

```r
# Urban analysis with accident priority
urban_params <- sero_recommend_params(
  data = data,
  analysis_type = "urban",
  priority = "accidents"
)

# Rural analysis with balanced approach
rural_params <- sero_recommend_params(
  data = data,
  analysis_type = "rural",
  priority = "balanced"
)

# Population-focused analysis
pop_params <- sero_recommend_params(
  data = data,
  analysis_type = "mixed",
  priority = "population"
)
```

## Built-in Scoring Functions

### Linear Scoring (Default)
```r
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_linear)
```
- Simple weighted average
- Balanced approach
- Good for general use

### Exponential Scoring
```r
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_exponential)
```
- Emphasizes high-accident areas
- Non-linear emphasis
- Good for high-risk focus

### Threshold Scoring
```r
result <- sero_optimal(data, 
  custom_scoring_function = function(data) {
    sero_scoring_threshold(data, 
      min_accident_norm = 0.4,
      min_population_norm = 0.3,
      min_road_norm = 0.2)
  })
```
- Minimum requirements approach
- Only considers locations meeting all thresholds
- Good for strict criteria

### Geometric Mean Scoring
```r
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_geometric)
```
- Balanced consideration of all criteria
- No criterion can be zero
- Good for avoiding extreme values

### Priority-Based Scoring
```r
result <- sero_optimal(data, 
  custom_scoring_function = sero_scoring_priority)
```
- Hierarchical approach
- Primary sort by accidents, secondary by population
- Good for clear priorities

## Creating Custom Scoring Functions

### Basic Template
```r
# Get the template
sero_scoring_template()

# Create your own function
my_custom_scoring <- function(data) {
  # Available data:
  # - data$accident_density_norm: normalized accident density (0-1)
  # - data$population_density_norm: normalized population density (0-1)
  # - data$road_accessibility_norm: normalized road accessibility (0-1)
  # - data$accident_count: raw accident count
  # - data$population_density: raw population density
  # - data$road_distance: raw road distance
  # - data$weights: list of weights
  
  # Example: Square root transformation
  accident_score <- sqrt(data$accident_density_norm)
  population_score <- sqrt(data$population_density_norm)
  road_score <- sqrt(data$road_accessibility_norm)
  
  scores <- data$weights$accident_weight * accident_score +
           data$weights$population_weight * population_score +
           data$weights$road_weight * road_score
  
  return(scores)
}

# Use your custom function
result <- sero_optimal(data, custom_scoring_function = my_custom_scoring)
```

## Complete Example Workflow

```r
library(SERO)

# Load your data
data <- list(
  accident = your_accident_data,
  roads = your_roads_data,
  landuse = your_landuse_data,     # optional
  population = your_population_data # optional
)

# 1. Validate parameters
validation <- sero_validate_params(data)

# 2. Get recommendations
params <- sero_recommend_params(data, "urban", "accidents")

# 3. Run customized analysis
result <- sero_optimal(
  data = data,
  grid_size = params$grid_size,
  weights = params$weights,
  min_road_distance = params$min_road_distance,
  max_road_distance = params$max_road_distance,
  min_accidents = params$min_accidents,
  max_locations = params$max_locations,
  custom_scoring_function = sero_scoring_exponential, # optional
  verbose = TRUE
)

# 4. Review results
summary(result)
plot(result, data = data)

# 5. Export results
sero_export_results(result, "my_analysis", format = "csv")
```

## Performance Considerations

### Grid Size Trade-offs
- **Small grid (50-100m)**: More detailed analysis, longer computation time
- **Medium grid (100-200m)**: Balanced detail and speed
- **Large grid (200-500m)**: Faster computation, less detailed results

### Distance Constraints
- **Tight constraints**: Fewer candidate locations, faster processing
- **Loose constraints**: More candidate locations, longer processing

### Custom Scoring Functions
- **Simple functions**: Faster computation
- **Complex functions**: More detailed scoring, longer computation

## Error Handling and Troubleshooting

### Common Issues and Solutions

1. **No locations found**
   - Increase road distance range
   - Decrease minimum thresholds
   - Disable land use avoidance
   - Increase grid size

2. **Very low scores**
   - Adjust scoring weights
   - Try different scoring functions
   - Check data quality

3. **Long computation times**
   - Increase grid size
   - Reduce analysis area
   - Use simpler scoring functions

### Parameter Validation Messages
- **Errors**: Must be fixed before analysis
- **Warnings**: Analysis will run but results may be affected
- **Suggestions**: Optional improvements for better results

## Best Practices

1. **Start with validation**: Always validate parameters first
2. **Use recommendations**: Get parameter suggestions for your context
3. **Test different approaches**: Compare multiple scoring methods
4. **Check results**: Review summary statistics and visualizations
5. **Document settings**: Keep track of parameter choices for reproducibility

## Advanced Use Cases

### Comparative Analysis
```r
# Compare different approaches
approaches <- list(
  "Default" = sero_optimal(data),
  "Accident Priority" = sero_optimal(data, 
    weights = list(accident_weight = 0.7, population_weight = 0.15, road_weight = 0.15)),
  "Custom Scoring" = sero_optimal(data, 
    custom_scoring_function = sero_scoring_exponential)
)

# Compare results
for (name in names(approaches)) {
  cat(name, ":", approaches[[name]]$summary$locations_found, "locations\n")
}
```

### Sensitivity Analysis
```r
# Test different grid sizes
grid_sizes <- c(50, 100, 200)
results <- lapply(grid_sizes, function(size) {
  sero_optimal(data, grid_size = size, verbose = FALSE)
})
```

This comprehensive customization framework makes the SERO package highly flexible and adaptable to different analysis needs while maintaining ease of use through parameter validation and recommendation systems.
