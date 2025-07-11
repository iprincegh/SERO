# SERO Heatmap Parameter Customization - Complete Implementation

## Overview
The SERO package now provides comprehensive parameter customization for heatmap generation, allowing users to create accident density visualizations tailored to their specific analysis needs.

## Implemented Functions

### 1. `sero_heatmap()` - Main heatmap function
**Customizable Parameters:**
- `risk_categories`: Filter accidents by severity (default: c(1,2) = fatal + serious)
- `bandwidth`: Kernel bandwidth for density estimation in meters (default: 1000)
- `grid_size`: Resolution of heatmap grid in meters (default: 100)
- `color_scheme`: Color palette - "viridis", "plasma", "inferno", "magma" (default: "viridis")
- `show_accidents`: Whether to show individual accident points (default: TRUE)
- `show_munster`: Whether to show Münster district boundaries (default: TRUE)
- `show_landuse`: Whether to show land use overlay (default: FALSE)
- `data`: Original data for context (optional)

### 2. `sero_heatmap_presets()` - Preset parameter combinations
**Available Presets:**
- `"urban"`: Fine detail for city analysis (bandwidth=300, grid_size=25, fatal only, inferno colors)
- `"regional"`: Broad view for regional analysis (bandwidth=1500, grid_size=150, all categories)
- `"overview"`: Quick overview (bandwidth=2000, grid_size=200, fatal+serious)
- `"detailed"`: Detailed local analysis (bandwidth=500, grid_size=50, fatal+serious, plasma colors)
- `"presentation"`: Clean visualization (bandwidth=800, grid_size=75, with landuse, plasma colors)
- `"default"`: Balanced settings (bandwidth=1000, grid_size=100, fatal+serious, viridis colors)

### 3. `sero_heatmap_compare()` - Batch heatmap generation
Create multiple heatmaps with different preset combinations for comparison.

## Usage Examples

### Basic customization:
```r
library(SERO)
data <- sero_load_data()

# Custom parameters
heatmap <- sero_heatmap(
  data$accident,
  risk_categories = c(1),      # Fatal accidents only
  bandwidth = 500,             # Fine detail
  grid_size = 50,              # High resolution
  color_scheme = "plasma"      # High contrast colors
)
```

### Using presets:
```r
# Get preset parameters
params <- sero_heatmap_presets("urban")

# Apply preset
heatmap <- do.call(sero_heatmap, c(list(data$accident), params))
```

### Batch comparison:
```r
# Generate multiple heatmaps
heatmaps <- sero_heatmap_compare(
  data$accident, 
  data, 
  c("urban", "regional", "overview")
)
```

## Parameter Guidelines

### Risk Categories:
- `c(1)`: Fatal accidents only (most severe)
- `c(1,2)`: Fatal + serious accidents (high-risk analysis)
- `c(1,2,3)`: All accident categories (complete picture)

### Bandwidth (smoothness):
- **Small (200-500m)**: Detailed local analysis, urban areas
- **Medium (800-1200m)**: Balanced city-wide analysis
- **Large (1500-3000m)**: Regional overviews, broad patterns

### Grid Size (resolution):
- **Small (25-75m)**: High-resolution, detailed mapping
- **Medium (100-150m)**: Standard analysis, balanced performance
- **Large (200-300m)**: Quick overviews, faster computation

### Color Schemes:
- **viridis**: Professional, colorblind-friendly
- **plasma**: High contrast, good for presentations
- **inferno**: Dramatic, good for highlighting hotspots
- **magma**: Subtle, good for overlays

## Technical Implementation

### Features:
✓ **Fully customizable parameters** - All key parameters can be modified by users
✓ **Preset combinations** - Ready-to-use parameter sets for common scenarios
✓ **Batch processing** - Generate multiple heatmaps with different settings
✓ **Validation** - Input validation with helpful error messages
✓ **Integration** - Works seamlessly with existing SERO functions
✓ **Documentation** - Comprehensive help and examples
✓ **Basemap integration** - Always uses Münster districts as context
✓ **Optional overlays** - Land use data can be added optionally

### Package Structure:
- **Functions exported**: `sero_heatmap`, `sero_heatmap_presets`, `sero_heatmap_compare`
- **Documentation**: Roxygen2 documentation for all functions
- **Examples**: Working examples in documentation and separate demo scripts
- **Guidelines**: Comprehensive parameter selection guide

## Files Created/Updated:
- `R/hotspots.R`: Enhanced with all heatmap functions
- `NAMESPACE`: Updated to export new functions
- `HEATMAP_PARAMETER_GUIDE.md`: Comprehensive usage guide
- `heatmap_parameter_examples.R`: Working examples script
- `test_heatmap_features.R`: Comprehensive testing script

## Testing Results:
✓ All parameter combinations tested successfully
✓ Preset functions work correctly
✓ Batch comparison generates multiple heatmaps
✓ Input validation catches errors appropriately
✓ Integration with existing SERO functions works seamlessly
✓ Documentation and examples are complete and functional

## Impact for Users:
Users can now create highly customized accident density heatmaps by:
1. **Adjusting risk categories** to focus on specific accident severities
2. **Modifying bandwidth** to control the level of detail vs. smoothness
3. **Changing grid size** to balance resolution vs. performance
4. **Selecting color schemes** appropriate for their visualization needs
5. **Using presets** for common analysis scenarios
6. **Comparing multiple configurations** using batch processing

This implementation provides the flexibility needed for various use cases while maintaining simplicity through presets and clear documentation.
