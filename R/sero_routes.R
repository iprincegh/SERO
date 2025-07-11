#' Calculate fastest routes to accident scenes
#'
#' Computes fastest routes from emergency service locations to accident scenes.
#' Uses simple straight-line distance calculations for speed.
#'
#' @param locations sero_optimal_locations object or sf object with service locations
#' @param accidents sf object with accident locations
#' @param max_routes Maximum number of routes to calculate (default=10)
#' @return sero_routes S3 object
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' routes <- sero_routes(locations, data$accident)
#' plot(routes)
#' }
sero_routes <- function(locations, accidents, max_routes = 10) {
  
  # Handle input types
  if (inherits(locations, "sero_optimal_locations")) {
    service_locations <- locations$locations
  } else if (inherits(locations, "sf")) {
    service_locations <- locations
  } else {
    stop("locations must be sero_optimal_locations object or sf object")
  }
  
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  if (nrow(service_locations) == 0 || nrow(accidents) == 0) {
    return(create_empty_routes())
  }
  
  # Ensure projected CRS for accurate distance calculations
  if (sf::st_is_longlat(service_locations)) {
    service_locations <- sf::st_transform(service_locations, 32632)
  }
  if (sf::st_is_longlat(accidents)) {
    accidents <- sf::st_transform(accidents, 32632)
  }
  
  # Calculate distance matrix
  distance_matrix <- sf::st_distance(service_locations, accidents)
  
  # Find optimal routes (nearest service location to each accident)
  routes_data <- data.frame()
  
  for (i in seq_len(min(max_routes, nrow(accidents)))) {
    # Find closest service location for this accident
    accident_idx <- i
    service_distances <- distance_matrix[, accident_idx]
    closest_service_idx <- which.min(service_distances)
    
    # Create route data
    route_data <- data.frame(
      route_id = i,
      service_location_id = closest_service_idx,
      accident_id = accident_idx,
      distance = as.numeric(service_distances[closest_service_idx]),
      estimated_time = as.numeric(service_distances[closest_service_idx]) / 50 * 3.6 # Assume 50 km/h avg speed
    )
    
    routes_data <- rbind(routes_data, route_data)
  }
  
  # Create route geometries (straight lines)
  route_geometries <- list()
  for (i in seq_len(nrow(routes_data))) {
    service_idx <- routes_data$service_location_id[i]
    accident_idx <- routes_data$accident_id[i]
    
    # Create line between service location and accident
    service_point <- sf::st_geometry(service_locations[service_idx, ])
    accident_point <- sf::st_geometry(accidents[accident_idx, ])
    
    route_line <- sf::st_linestring(rbind(
      sf::st_coordinates(service_point),
      sf::st_coordinates(accident_point)
    ))
    
    route_geometries[[i]] <- route_line
  }
  
  # Create routes sf object
  routes_sf <- sf::st_sf(
    routes_data,
    geometry = sf::st_sfc(route_geometries, crs = sf::st_crs(service_locations))
  )
  
  # Create summary statistics
  summary_stats <- list(
    total_routes = nrow(routes_sf),
    avg_distance = mean(routes_sf$distance),
    avg_time = mean(routes_sf$estimated_time),
    max_distance = max(routes_sf$distance),
    max_time = max(routes_sf$estimated_time)
  )
  
  # Create S3 object
  result <- structure(
    list(
      routes = routes_sf,
      service_locations = service_locations,
      accidents = accidents,
      parameters = list(
        max_routes = max_routes
      ),
      summary = summary_stats,
      crs = sf::st_crs(service_locations)
    ),
    class = "sero_routes"
  )
  
  return(result)
}

#' Create empty routes object
#' @return empty sero_routes object
create_empty_routes <- function() {
  structure(
    list(
      routes = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      service_locations = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      accidents = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      parameters = list(),
      summary = list(
        total_routes = 0,
        avg_distance = 0,
        avg_time = 0,
        max_distance = 0,
        max_time = 0
      )
    ),
    class = "sero_routes"
  )
}

#' Plot method for sero_routes using ggplot2
#'
#' @param x sero_routes object
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_routes <- function(x, ...) {
  if (nrow(x$routes) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No routes found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  routes_wgs84 <- sf::st_transform(x$routes, 4326)
  locations_wgs84 <- sf::st_transform(x$service_locations, 4326)
  accidents_wgs84 <- sf::st_transform(x$accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    # Add routes as lines
    ggplot2::geom_sf(data = routes_wgs84,
                    ggplot2::aes(color = .data$estimated_time),
                    size = 1.2, alpha = 0.7) +
    # Add service locations
    ggplot2::geom_sf(data = locations_wgs84,
                    color = "blue", fill = "lightblue",
                    shape = 21, size = 4, alpha = 0.8) +
    # Add accident locations
    ggplot2::geom_sf(data = accidents_wgs84[seq_len(nrow(routes_wgs84)), ],
                    color = "red", size = 2, alpha = 0.8) +
    # Color scale for travel time
    ggplot2::scale_color_viridis_c(name = "Travel Time\n(minutes)", 
                                  option = "plasma", trans = "sqrt") +
    # Styling
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Emergency Response Routes",
                 subtitle = paste("Showing", x$summary$total_routes, "fastest routes"),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Print method for sero_routes
#'
#' @param x sero_routes object
#' @param ... additional arguments (unused)
#' @export
print.sero_routes <- function(x, ...) {
  cat("SERO Emergency Response Routes\n")
  cat("==============================\n\n")
  
  cat("Summary:\n")
  cat("- Total routes calculated:", x$summary$total_routes, "\n")
  cat("- Average distance:", round(x$summary$avg_distance, 0), "meters\n")
  cat("- Average travel time:", round(x$summary$avg_time, 1), "minutes\n")
  cat("- Maximum distance:", round(x$summary$max_distance, 0), "meters\n")
  cat("- Maximum travel time:", round(x$summary$max_time, 1), "minutes\n\n")
  
  cat("Parameters:\n")
  cat("- Maximum routes:", x$parameters$max_routes, "\n\n")
  
  if (x$summary$total_routes > 0) {
    cat("Route Details:\n")
    for (i in seq_len(min(5, nrow(x$routes)))) {
      cat(sprintf("  %d. Distance: %dm, Time: %.1f min\n", 
                  i, 
                  round(x$routes$distance[i]), 
                  x$routes$estimated_time[i]))
    }
    if (nrow(x$routes) > 5) {
      cat("  ... and", nrow(x$routes) - 5, "more routes\n")
    }
  }
  
  cat("\nUse plot() to visualize with ggplot2.\n")
}
