---
title: "SERO Emergency Response Optimization: Example Workflow"
author: "SERO Package Team"
date: "2025-07-13"
output:
  pdf_document:
    toc: true
    number_sections: true
---

```{r setup, include=TRUE}
library(SERO)
library(sf)
library(ggplot2)
library(MASS)
library(raster)
min <- base::min
max <- base::max
cat("[DIAG] Final check - min is base::min:", identical(min, base::min), "\n")
cat("[DIAG] Final check - max is base::max:", identical(max, base::max), "\n")
```

#' SERO User-Friendly Workflow Functions
#'
#' Simple, fast workflow functions optimized for user experience

#' Quick SERO demo (fast and user-friendly)
#'
#' @param interactive logical whether to launch interactive mode
#' @return results list
#' @export
sero_quick_demo <- function(interactive = FALSE) {
  
  cat("SERO Quick Demo\n")
  cat("==================\n")
  cat("Running fast emergency response optimization...\n")
  
  # Use the new optimized workflow
  results <- sero_emergency_workflow(
    interactive = interactive, 
    quick = TRUE, 
    num_locations = 5
  )
  
  return(invisible(results))
}

#' Complete SERO workflow (comprehensive analysis)
#'
#' @param launch_interactive logical whether to launch interactive mode
#' @param quick logical whether to use fast calculations (default: TRUE)
#' @param num_locations number of emergency bases to find (default: 5)
#' @return results list
#' @export
sero_complete_workflow <- function(launch_interactive = FALSE, quick = TRUE, num_locations = 5) {
  
  cat("SERO: Complete Emergency Response Workflow\n")
  cat(strrep("=", 50), "\n")
  
  # Use the optimized workflow
  results <- sero_emergency_workflow(
    interactive = launch_interactive, 
    quick = quick, 
    num_locations = num_locations
  )
  
  cat("\nAdvanced Features Available:\n")
  cat("   - Hotspot analysis: sero_hotspot_analysis()\n")
  cat("   - Custom optimization: sero_find_optimal_locations()\n") 
  cat("   - Professional mapping: sero_plot_optimal_quick()\n")
  cat("   - Interactive system: sero_interactive_routing()\n")
  
  return(results)
}

#' SERO Package Setup
#'
#' This function loads the required packages and performs diagnostic checks.
#' It is called automatically when the package is loaded.
#'
#' @export
sero_setup <- function() {
  # Load required packages
  library(SERO)
  library(sf)
  library(ggplot2)
  library(MASS)
  library(raster)
  
  # Restore base min/max functions AFTER loading all packages
  min <- base::min
  max <- base::max
  cat("[DIAG] Final check - min is base::min:", identical(min, base::min), "\n")
  cat("[DIAG] Final check - max is base::max:", identical(max, base::max), "\n")
}
