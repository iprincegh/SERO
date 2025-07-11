#' Complete SERO spatial analysis workflow
#'
#' Performs the complete SERO analysis workflow: hotspot identification,
#' optimal location computation using multi-criteria analysis, and route calculation.
#' Uses built-in data from the inst/gpkg folder.
#'
#' Implements 6 multi-criteria analysis criteria:
#' 1. High-Risk Accidents: Only accidents with UKATEGORIE = 1 (fatal) or 2 (serious)
#' 2. Suitable Land Use: Residential, commercial, or industrial areas only
#' 3. Proximity to Roads: 500m minimum, 1000m maximum from roads
#' 4. Accident Density: 100m grid cells using ST_SnapToGrid equivalent
#' 5. Population Density: Higher density areas prioritized
#' 6. Centroid Calculation: ST_Centroid equivalent for grid cells
#'
#' @param risk_categories High-risk accident categories (default=c(1,2))
#' @param suitable_landuse Suitable land use classes (default=c("residential", "commercial", "industrial"))
#' @param max_locations Maximum number of service locations (default=10)
#' @param max_routes Maximum number of routes to calculate (default=20)
#' @return List with hotspots, locations, and routes objects
#' @export
#' @examples
#' \dontrun{
#' results <- sero_analyze()
#' plot(results$hotspots)
#' plot(results$locations)
#' plot(results$routes)
#' }
sero_analyze <- function(risk_categories = c(1, 2), 
                        suitable_landuse = c("residential", "commercial", "industrial"),
                        max_locations = 10, max_routes = 20) {
  
  # Load built-in data
  cat("Loading built-in spatial data...\n")
  data <- sero_load_data()
  
  # Step 1: Identify hotspots
  cat("Step 1: Identifying accident hotspots using point pattern analysis...\n")
  hotspots <- sero_identify_hotspots(data$accident, risk_categories = risk_categories)
  
  # Step 2: Compute optimal locations using enhanced multi-criteria analysis
  cat("Step 2: Computing optimal service locations using enhanced multi-criteria analysis...\n")
  locations <- sero_calculate_optimal_locations(data, 
                                               risk_categories = risk_categories,
                                               suitable_landuse = suitable_landuse,
                                               max_locations = max_locations)
  
  # Step 3: Calculate routes
  cat("Step 3: Calculating emergency routes...\n")
  routes <- sero_calculate_routes(locations, data$accident, max_routes = max_routes)
  
  # Create combined results
  results <- list(
    hotspots = hotspots,
    locations = locations,
    routes = routes,
    data = data,
    summary = list(
      total_accidents = nrow(data$accident),
      high_risk_accidents = sum(data$accident$UKATEGORIE %in% risk_categories),
      hotspots_found = nrow(hotspots$hotspots),
      locations_found = nrow(locations$locations),
      routes_calculated = nrow(routes$routes),
      avg_response_time = ifelse(nrow(routes$routes) > 0, 
                                mean(routes$routes$estimated_time), 0),
      criteria_applied = c(
        "1. High-Risk Accidents (UKATEGORIE = 1 or 2)",
        "2. Suitable Land Use (residential, commercial, industrial)", 
        "3. Proximity to Roads (500m-1000m from roads)",
        "4. Accident Density (100m grid cells)",
        "5. Population Density (higher density prioritized)",
        "6. Centroid Calculation (ST_Centroid equivalent)"
      )
    )
  )
  
  class(results) <- "sero_analysis"
  
  cat("Analysis complete!\n")
  cat("==================\n")
  cat("Data Summary:\n")
  cat("- Total accidents:", results$summary$total_accidents, "\n")
  cat("- High-risk accidents:", results$summary$high_risk_accidents, "\n")
  cat("- Available layers:", paste(names(data), collapse = ", "), "\n\n")
  
  cat("Results:\n")
  cat("- Hotspots found:", results$summary$hotspots_found, "\n")
  cat("- Optimal locations:", results$summary$locations_found, "\n")
  cat("- Routes calculated:", results$summary$routes_calculated, "\n")
  cat("- Average response time:", round(results$summary$avg_response_time, 1), "minutes\n\n")
  
  cat("Multi-criteria analysis applied:\n")
  for (i in seq_along(results$summary$criteria_applied)) {
    cat(paste0("  ", i, ". ", results$summary$criteria_applied[i], "\n"))
  }
  
  return(results)
}

#' Print method for sero_analysis
#'
#' @param x sero_analysis object
#' @param ... additional arguments (unused)
#' @export
print.sero_analysis <- function(x, ...) {
  cat("SERO Complete Spatial Analysis Results\n")
  cat("======================================\n\n")
  
  cat("Summary:\n")
  cat("- Total accidents analyzed:", x$summary$total_accidents, "\n")
  cat("- Hotspots identified:", x$summary$hotspots_found, "\n")
  cat("- Optimal locations found:", x$summary$locations_found, "\n")
  cat("- Routes calculated:", x$summary$routes_calculated, "\n")
  cat("- Average response time:", round(x$summary$avg_response_time, 1), "minutes\n\n")
  
  cat("Use plot() on individual components:\n")
  cat("- plot(results$hotspots)  # Show hotspots\n")
  cat("- plot(results$locations) # Show optimal locations\n")
  cat("- plot(results$routes)    # Show routes\n")
}
