#' Interactive accident selection and routing for SERO package
#'
#' This file contains functions for interactive accident selection and routing
#' from optimal emergency service locations to selected accident sites.

#' Create an interactive map for accident selection
#'
#' Creates an interactive map where users can click on accident locations
#' to select them for routing analysis. This function uses plotly for interactivity.
#'
#' @param data List containing spatial data layers
#' @param optimal_locations sero_optimal_locations object with calculated optimal locations
#' @param max_accidents Maximum number of accidents to display (default=100)
#' @return Interactive plotly map
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' optimal_locs <- sero_calculate_optimal_locations(data)
#' interactive_map <- sero_create_interactive_map(data, optimal_locs)
#' interactive_map
#' }
sero_create_interactive_map <- function(data, optimal_locations, max_accidents = 100) {
  
  # Check if plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for interactive maps. Please install it with: install.packages('plotly')")
  }
  
  # Input validation
  if (!"accident" %in% names(data)) {
    stop("Accident data not found in input data")
  }
  
  if (!inherits(optimal_locations, "sero_optimal_locations")) {
    stop("optimal_locations must be a sero_optimal_locations object")
  }
  
  # Transform data to WGS84 for plotting
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  optimal_wgs84 <- sf::st_transform(optimal_locations$locations, 4326)
  
  # Limit number of accidents for performance
  if (nrow(accidents_wgs84) > max_accidents) {
    accidents_wgs84 <- accidents_wgs84[1:max_accidents, ]
  }
  
  # Extract coordinates
  accident_coords <- sf::st_coordinates(accidents_wgs84)
  optimal_coords <- sf::st_coordinates(optimal_wgs84)
  
  # Create base map with districts if available
  base_plot <- ggplot2::ggplot()
  
  if ("districts" %in% names(data)) {
    districts_wgs84 <- sf::st_transform(data$districts, 4326)
    base_plot <- base_plot +
      ggplot2::geom_sf(data = districts_wgs84, 
                      fill = "lightgray", 
                      color = "darkgray", 
                      alpha = 0.3)
  }
  
  # Add optimal locations
  base_plot <- base_plot +
    ggplot2::geom_point(data = data.frame(
      x = optimal_coords[,1],
      y = optimal_coords[,2],
      type = "Optimal Location",
      label = paste("Optimal Location", seq_len(nrow(optimal_coords)))
    ), ggplot2::aes(x = .data$x, y = .data$y, color = .data$type, text = .data$label),
    size = 4, shape = 17) +
    
    # Add accidents
    ggplot2::geom_point(data = data.frame(
      x = accident_coords[,1],
      y = accident_coords[,2],
      type = "Accident",
      label = paste("Accident", seq_len(nrow(accident_coords)), 
                   ifelse("UKATEGORIE" %in% names(accidents_wgs84), 
                          paste("(Severity:", accidents_wgs84$UKATEGORIE, ")"), ""))
    ), ggplot2::aes(x = .data$x, y = .data$y, color = .data$type, text = .data$label),
    size = 2, alpha = 0.7) +
    
    ggplot2::scale_color_manual(values = c("Optimal Location" = "blue", "Accident" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Interactive Accident Selection Map",
                 subtitle = "Click on accidents to select for routing",
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  # Convert to plotly
  interactive_map <- plotly::ggplotly(base_plot, tooltip = "text")
  
  # Add click event handling instructions
  interactive_map <- plotly::layout(interactive_map,
    title = list(text = "Interactive Accident Selection Map<br><sup>Click on accident points to select for routing</sup>"))
  
  return(interactive_map)
}

#' Select accident by coordinates
#'
#' Selects the nearest accident to the provided coordinates. This function
#' is typically used after a user clicks on the interactive map.
#'
#' @param data List containing spatial data layers
#' @param longitude Longitude of selected point
#' @param latitude Latitude of selected point
#' @param max_distance Maximum distance in meters to search for accidents (default=1000)
#' @return Selected accident as sf object or NULL if none found
#' @export
sero_select_accident_by_coords <- function(data, longitude, latitude, max_distance = 1000) {
  
  if (!"accident" %in% names(data)) {
    stop("Accident data not found in input data")
  }
  
  # Create point from coordinates
  selected_point <- sf::st_point(c(longitude, latitude))
  selected_sf <- sf::st_sf(geometry = sf::st_sfc(selected_point, crs = 4326))
  
  # Transform to projected CRS for distance calculation
  selected_proj <- sf::st_transform(selected_sf, 32632)
  accidents_proj <- sf::st_transform(data$accident, 32632)
  
  # Calculate distances
  distances <- sf::st_distance(selected_proj, accidents_proj)
  
  # Find nearest accident within max_distance
  min_distance <- min(distances)
  if (as.numeric(min_distance) > max_distance) {
    warning("No accidents found within ", max_distance, " meters of selected point")
    return(NULL)
  }
  
  nearest_idx <- which.min(distances)
  selected_accident <- data$accident[nearest_idx, ]
  
  return(selected_accident)
}

#' Calculate route from optimal locations to selected accident
#'
#' Calculates the optimal route from the nearest emergency service location
#' to a selected accident location.
#'
#' @param optimal_locations sero_optimal_locations object
#' @param selected_accident sf object with single accident location
#' @param show_all_routes Logical, whether to show routes from all optimal locations (default=FALSE)
#' @return sero_route_to_accident S3 object
#' @export
sero_route_to_selected_accident <- function(optimal_locations, selected_accident, show_all_routes = FALSE) {
  
  # Input validation
  if (!inherits(optimal_locations, "sero_optimal_locations")) {
    stop("optimal_locations must be a sero_optimal_locations object")
  }
  
  if (!inherits(selected_accident, "sf") || nrow(selected_accident) != 1) {
    stop("selected_accident must be a single-row sf object")
  }
  
  if (nrow(optimal_locations$locations) == 0) {
    stop("No optimal locations available")
  }
  
  # Transform to projected CRS for distance calculation
  service_locations <- sf::st_transform(optimal_locations$locations, 32632)
  accident_location <- sf::st_transform(selected_accident, 32632)
  
  # Calculate distances from all service locations to the accident
  distances <- sf::st_distance(service_locations, accident_location)
  
  # Find the nearest service location
  nearest_idx <- which.min(distances)
  nearest_distance <- as.numeric(distances[nearest_idx])
  
  # Calculate routes
  if (show_all_routes) {
    # Create routes from all optimal locations
    routes_data <- data.frame(
      route_id = seq_len(nrow(service_locations)),
      service_location_id = seq_len(nrow(service_locations)),
      distance = as.numeric(distances),
      estimated_time = as.numeric(distances) / 50 * 3.6, # Assume 50 km/h avg speed
      is_optimal = seq_len(nrow(service_locations)) == nearest_idx
    )
    
    # Create route geometries
    route_geometries <- list()
    for (i in seq_len(nrow(service_locations))) {
      service_point <- sf::st_geometry(service_locations[i, ])
      accident_point <- sf::st_geometry(accident_location)
      
      route_line <- sf::st_linestring(rbind(
        sf::st_coordinates(service_point),
        sf::st_coordinates(accident_point)
      ))
      
      route_geometries[[i]] <- route_line
    }
    
  } else {
    # Create route only from nearest service location
    routes_data <- data.frame(
      route_id = 1,
      service_location_id = nearest_idx,
      distance = nearest_distance,
      estimated_time = nearest_distance / 50 * 3.6,
      is_optimal = TRUE
    )
    
    # Create route geometry
    service_point <- sf::st_geometry(service_locations[nearest_idx, ])
    accident_point <- sf::st_geometry(accident_location)
    
    route_line <- sf::st_linestring(rbind(
      sf::st_coordinates(service_point),
      sf::st_coordinates(accident_point)
    ))
    
    route_geometries <- list(route_line)
  }
  
  # Create routes sf object
  routes_sf <- sf::st_sf(
    routes_data,
    geometry = sf::st_sfc(route_geometries, crs = sf::st_crs(service_locations))
  )
  
  # Create summary statistics
  optimal_route <- routes_sf[routes_sf$is_optimal, ]
  summary_stats <- list(
    selected_accident_id = 1,
    nearest_service_id = nearest_idx,
    optimal_distance = optimal_route$distance,
    optimal_time = optimal_route$estimated_time,
    total_routes = nrow(routes_sf),
    show_all_routes = show_all_routes
  )
  
  # Create S3 object
  result <- structure(
    list(
      routes = routes_sf,
      selected_accident = selected_accident,
      service_locations = optimal_locations$locations,
      nearest_service_location = optimal_locations$locations[nearest_idx, ],
      summary = summary_stats,
      crs = sf::st_crs(service_locations)
    ),
    class = "sero_route_to_accident"
  )
  
  return(result)
}

#' Plot method for sero_route_to_accident
#'
#' @param x sero_route_to_accident object
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_route_to_accident <- function(x, ...) {
  
  if (nrow(x$routes) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No routes found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  routes_wgs84 <- sf::st_transform(x$routes, 4326)
  accident_wgs84 <- sf::st_transform(x$selected_accident, 4326)
  locations_wgs84 <- sf::st_transform(x$service_locations, 4326)
  nearest_location_wgs84 <- sf::st_transform(x$nearest_service_location, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    # Add all routes
    ggplot2::geom_sf(data = routes_wgs84,
                    ggplot2::aes(color = .data$is_optimal, size = .data$is_optimal),
                    alpha = 0.7) +
    # Add all service locations
    ggplot2::geom_sf(data = locations_wgs84,
                    color = "blue", fill = "lightblue",
                    shape = 21, size = 3, alpha = 0.6) +
    # Highlight nearest service location
    ggplot2::geom_sf(data = nearest_location_wgs84,
                    color = "darkblue", fill = "blue",
                    shape = 21, size = 5, alpha = 0.9) +
    # Add selected accident
    ggplot2::geom_sf(data = accident_wgs84,
                    color = "red", fill = "yellow",
                    shape = 23, size = 6, alpha = 0.9) +
    # Styling
    ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray"),
                               labels = c("TRUE" = "Optimal Route", "FALSE" = "Other Routes"),
                               name = "Route Type") +
    ggplot2::scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.8),
                              guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Route to Selected Accident",
                 subtitle = paste("Distance:", round(x$summary$optimal_distance, 0), "m,",
                                 "Time:", round(x$summary$optimal_time, 1), "min"),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Print method for sero_route_to_accident
#'
#' @param x sero_route_to_accident object
#' @param ... additional arguments (unused)
#' @export
print.sero_route_to_accident <- function(x, ...) {
  cat("SERO Route to Selected Accident\n")
  cat("===============================\n\n")
  
  cat("Selected Accident:\n")
  cat("- Coordinates:", paste(round(sf::st_coordinates(x$selected_accident), 4), collapse = ", "), "\n")
  if ("UKATEGORIE" %in% names(x$selected_accident)) {
    cat("- Severity Category:", x$selected_accident$UKATEGORIE, "\n")
  }
  cat("\n")
  
  cat("Optimal Route:\n")
  cat("- Distance:", round(x$summary$optimal_distance, 0), "meters\n")
  cat("- Estimated Time:", round(x$summary$optimal_time, 1), "minutes\n")
  cat("- Service Location ID:", x$summary$nearest_service_id, "\n\n")
  
  cat("Analysis Summary:\n")
  cat("- Total routes calculated:", x$summary$total_routes, "\n")
  cat("- Show all routes:", x$summary$show_all_routes, "\n\n")
  
  cat("Use plot() to visualize the route.\n")
}

#' Complete workflow for interactive accident selection and routing
#'
#' This function combines all the interactive functionality into a single workflow.
#' It creates an interactive map, allows accident selection, and calculates routes.
#'
#' @param data List containing spatial data layers (from sero_load_data())
#' @param optimal_locations sero_optimal_locations object (from sero_calculate_optimal_locations())
#' @param longitude Longitude of selected accident (from interactive map click)
#' @param latitude Latitude of selected accident (from interactive map click)
#' @param show_all_routes Logical, whether to show routes from all optimal locations (default=FALSE)
#' @return sero_route_to_accident object
#' @export
#' @examples
#' \dontrun{
#' # Step 1: Load data and calculate optimal locations
#' data <- sero_load_data()
#' optimal_locs <- sero_calculate_optimal_locations(data)
#' 
#' # Step 2: Create interactive map
#' interactive_map <- sero_create_interactive_map(data, optimal_locs)
#' interactive_map  # Click on an accident to get coordinates
#' 
#' # Step 3: Use coordinates from map click to calculate route
#' route <- sero_interactive_routing_workflow(data, optimal_locs, 
#'                                           longitude = 7.625, latitude = 51.962)
#' plot(route)
#' }
sero_interactive_routing_workflow <- function(data, optimal_locations, longitude, latitude, show_all_routes = FALSE) {
  
  # Step 1: Select accident by coordinates
  selected_accident <- sero_select_accident_by_coords(data, longitude, latitude)
  
  if (is.null(selected_accident)) {
    stop("No accident found at the selected coordinates")
  }
  
  # Step 2: Calculate route to selected accident
  route <- sero_route_to_selected_accident(optimal_locations, selected_accident, show_all_routes)
  
  return(route)
}
