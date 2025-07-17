# SERO Package Examples

This directory contains comprehensive demonstrations of the SERO (Spatial Emergency Response Optimization) package functionality.

## Available Demonstrations

### 1. `SERO_comprehensive_demo.Rmd`
**Comprehensive RMarkdown Demonstration**
- Complete package functionality showcase
- Professional HTML/PDF output
- Interactive code chunks
- Detailed explanations and interpretations
- Performance metrics and analysis

**To run:**
```r
# In R/RStudio
rmarkdown::render("SERO_comprehensive_demo.Rmd")
# Or use the "Knit" button in RStudio
```

**Output:** 
- `SERO_comprehensive_demo.html` - Interactive HTML report
- `SERO_comprehensive_demo.pdf` - Professional PDF report (if PDF output enabled)

### 2. `SERO_demo.R`
**Complete R Script Demonstration**
- All core functionality in a single script
- Console-based output
- Easy to modify and experiment with
- Step-by-step analysis workflow

**To run:**
```r
# In R/RStudio
source("SERO_demo.R")
# Or run line by line for interactive exploration
```

## What's Demonstrated

Both demonstrations cover:

1. **Data Loading & Validation**
   - Loading Münster spatial dataset
   - Data structure validation
   - Dataset summary statistics

2. **Hotspot Analysis**
   - Identifying accident concentration areas
   - Spatial clustering with customizable parameters
   - Professional visualization

3. **Kernel Density Analysis**
   - Smooth risk surface generation
   - Heatmap visualization
   - Risk category comparisons

4. **Optimal Location Finding**
   - Strategic emergency service placement
   - K-means optimization
   - Coverage analysis

5. **Route Optimization**
   - Emergency response route calculation
   - Distance and time estimation
   - Route visualization

6. **Complete Workflow**
   - Integrated analysis pipeline
   - Performance metrics
   - Comprehensive visualizations

## System Requirements

- R >= 4.0.0
- SERO package installed
- Required dependencies: sf, ggplot2, dplyr

## Quick Start

1. **For HTML Report:**
```r
library(SERO)
rmarkdown::render("inst/examples/SERO_comprehensive_demo.Rmd")
```

2. **For Interactive Analysis:**
```r
library(SERO)
source("inst/examples/SERO_demo.R")
```

## Output Examples

The demonstrations generate:
- Professional maps and visualizations
- Performance metrics tables
- Comprehensive analysis reports
- System optimization recommendations

## Support

For questions or issues:
- Check the package documentation: `help(package = "SERO")`
- Review function help: `?sero_function_name`
- Examine the demonstration code for usage examples

---

*SERO Package - Spatial Emergency Response Optimization*
