# SERO Heatmap Parameter Customization Guide

This guide demonstrates how to use the customizable parameters in the SERO heatmap functions to create accident density visualizations tailored to your specific analysis needs.

## Basic Usage

```r
library(SERO)

# Load the data
data <- sero_load_data()

# Create a basic heatmap with default parameters
heatmap_basic <- sero_heatmap(data$accident)
print(heatmap_basic)
```

## Customizable Parameters

### 1. Risk Categories (`risk_categories`)
Filter accidents by severity level:

```r
# Only fatal accidents (category 1)
heatmap_fatal <- sero_heatmap(data$accident, risk_categories = c(1))

# Fatal and serious accidents (categories 1 and 2) - DEFAULT
heatmap_severe <- sero_heatmap(data$accident, risk_categories = c(1, 2))

# All accident categories
heatmap_all <- sero_heatmap(data$accident, risk_categories = c(1, 2, 3))
```

### 2. Bandwidth (`bandwidth`)
Control the smoothness of the density surface (in meters):

```r
# Fine-grained detail (small bandwidth)
heatmap_detailed <- sero_heatmap(data$accident, bandwidth = 300)

# Medium smoothing - DEFAULT
heatmap_medium <- sero_heatmap(data$accident, bandwidth = 1000)

# Broad overview (large bandwidth)
heatmap_overview <- sero_heatmap(data$accident, bandwidth = 2000)
```

### 3. Grid Size (`grid_size`)
Set the resolution of the heatmap grid (in meters):

```r
# High resolution (small grid cells)
heatmap_hires <- sero_heatmap(data$accident, grid_size = 50)

# Standard resolution - DEFAULT
heatmap_standard <- sero_heatmap(data$accident, grid_size = 100)

# Low resolution (large grid cells)
heatmap_lowres <- sero_heatmap(data$accident, grid_size = 200)
```

### 4. Color Scheme (`color_scheme`)
Choose from different color palettes:

```r
# Viridis (blue to yellow) - DEFAULT
heatmap_viridis <- sero_heatmap(data$accident, color_scheme = "viridis")

# Plasma (purple to yellow)
heatmap_plasma <- sero_heatmap(data$accident, color_scheme = "plasma")

# Inferno (black to yellow)
heatmap_inferno <- sero_heatmap(data$accident, color_scheme = "inferno")

# Magma (black to white)
heatmap_magma <- sero_heatmap(data$accident, color_scheme = "magma")
```

### 5. Display Options
Control what elements are shown:

```r
# Hide individual accident points
heatmap_clean <- sero_heatmap(data$accident, show_accidents = FALSE)

# Show land use overlay
heatmap_landuse <- sero_heatmap(data$accident, show_landuse = TRUE)

# Hide Münster district boundaries
heatmap_no_districts <- sero_heatmap(data$accident, show_munster = FALSE)
```

## Using Presets

The package includes preset parameter combinations for common analysis scenarios:

```r
# Available presets
preset_types <- c("urban", "regional", "overview", "detailed", "presentation", "default")

# Get preset parameters
urban_params <- sero_heatmap_presets("urban")
print(urban_params)

# Apply preset
heatmap_urban <- do.call(sero_heatmap, c(list(data$accident), urban_params))
```

### Preset Descriptions:

- **urban**: Fine detail for city-scale analysis (bandwidth=300, grid_size=25, fatal accidents only)
- **regional**: Broad view for regional analysis (bandwidth=1500, grid_size=150, all categories)
- **overview**: Quick overview of accident patterns (bandwidth=2000, grid_size=200, fatal+serious)
- **detailed**: Detailed local analysis (bandwidth=500, grid_size=50, fatal+serious)
- **presentation**: Clean visualization for presentations (bandwidth=800, grid_size=75, with landuse)
- **default**: Balanced settings for general use (bandwidth=1000, grid_size=100, fatal+serious)

## Advanced Customization

### Custom Parameter Combinations

```r
# High-detail urban analysis
heatmap_custom <- sero_heatmap(
  data$accident,
  risk_categories = c(1),          # Fatal accidents only
  bandwidth = 200,                 # Very fine detail
  grid_size = 25,                  # High resolution
  show_accidents = TRUE,           # Show individual points
  show_landuse = TRUE,             # Show land use context
  color_scheme = "inferno"         # High-contrast colors
)

# Regional overview with context
heatmap_regional <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2, 3),    # All accident types
  bandwidth = 1800,                # Broad smoothing
  grid_size = 150,                 # Lower resolution
  show_accidents = FALSE,          # Clean visualization
  show_landuse = FALSE,            # Focus on accidents
  color_scheme = "viridis"         # Professional colors
)
```

### Batch Comparison

Create multiple heatmaps with different settings for comparison:

```r
# Compare different analysis types
comparison_heatmaps <- sero_heatmap_compare(
  data$accident, 
  data, 
  c("urban", "regional", "detailed")
)

# Access individual heatmaps
print(comparison_heatmaps$urban)
print(comparison_heatmaps$regional)
print(comparison_heatmaps$detailed)
```

## Parameter Guidelines

### Bandwidth Selection:
- **Small (200-500m)**: For detailed local analysis, urban areas
- **Medium (800-1200m)**: For balanced city-wide analysis
- **Large (1500-3000m)**: For regional overviews, broad patterns

### Grid Size Selection:
- **Small (25-75m)**: For high-resolution analysis, detailed mapping
- **Medium (100-150m)**: For standard analysis, balanced performance
- **Large (200-300m)**: For quick overviews, low computational cost

### Risk Category Selection:
- **Fatal only (1)**: Focus on most severe accidents
- **Fatal + Serious (1,2)**: Standard high-risk analysis
- **All categories (1,2,3)**: Complete accident picture

### Color Scheme Selection:
- **viridis**: Professional, colorblind-friendly
- **plasma**: High contrast, good for presentations
- **inferno**: Dramatic, good for highlighting hotspots
- **magma**: Subtle, good for overlays

## Examples by Use Case

### Traffic Safety Analysis
```r
# Focus on fatal accidents with fine detail
safety_heatmap <- sero_heatmap(
  data$accident,
  risk_categories = c(1),
  bandwidth = 400,
  grid_size = 50,
  color_scheme = "inferno",
  show_accidents = TRUE
)
```

### Urban Planning
```r
# Include land use context
planning_heatmap <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 800,
  grid_size = 75,
  show_landuse = TRUE,
  color_scheme = "viridis"
)
```

### Emergency Services
```r
# Broad overview for service planning
emergency_heatmap <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2, 3),
  bandwidth = 1200,
  grid_size = 100,
  show_accidents = FALSE,
  color_scheme = "plasma"
)
```

### Research/Academic
```r
# Detailed analysis with full context
research_heatmap <- sero_heatmap(
  data$accident,
  risk_categories = c(1, 2),
  bandwidth = 600,
  grid_size = 50,
  show_accidents = TRUE,
  show_landuse = TRUE,
  color_scheme = "viridis"
)
```

This comprehensive parameter system allows you to create accident density heatmaps tailored to your specific analysis needs, from fine-grained local studies to broad regional overviews.
