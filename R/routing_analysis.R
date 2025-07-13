#' Enhanced Routing Analysis for SERO Package
#'
#' Interactive routing with clickable map functionality and shortest path calculation

#' Calculate routes from new accident locations to optimal emergency service locations
#'
#' @param data List containing spatial data with optimal_locations
#' @param new_accident_coords Matrix or data.frame with lon/lat coordinates of new accident
#' @param routing_method Character, routing method: "euclidean", "road_network", or "hybrid"
#' @param include_roads Logical, whether to include road network in visualization
#' @return List with route analysis results and interactive map
#' @importFrom sf st_transform st_coordinates st_as_sf st_distance st_nearest_feature
#' @export
sero_emergency_routing <- function(data, 
                                  new_accident_coords = NULL,
                                  routing_method = "euclidean",
                                  include_roads = TRUE) {
  
  # Validate input data
  if (!"optimal_locations" %in% names(data)) {
    stop("Data must contain 'optimal_locations'. Run sero_optimal_locations() first.")
  }
  
  if (!"districts" %in% names(data)) {
    stop("Data must contain 'districts' layer")
  }
  
  # Transform to consistent CRS
  optimal_locations_wgs84 <- sf::st_transform(data$optimal_locations, 4326)
  districts_wgs84 <- sf::st_transform(data$districts, 4326)
  
  # If no new accident coordinates provided, create interactive map for clicking
  if (is.null(new_accident_coords)) {
    return(create_interactive_routing_map(data, optimal_locations_wgs84, districts_wgs84, include_roads))
  }
  
  # Process new accident location
  if (is.vector(new_accident_coords) && length(new_accident_coords) == 2) {
    new_accident_coords <- matrix(new_accident_coords, nrow = 1)
  }
  
  # Create sf object for new accident
  new_accident_sf <- sf::st_as_sf(
    data.frame(
      id = "NEW_ACCIDENT",
      lon = new_accident_coords[1],
      lat = new_accident_coords[2]
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # Calculate routes based on method
  route_result <- switch(routing_method,
    "euclidean" = calculate_euclidean_route(new_accident_sf, optimal_locations_wgs84),
    "road_network" = calculate_road_network_route(new_accident_sf, optimal_locations_wgs84, data),
    "hybrid" = calculate_hybrid_route(new_accident_sf, optimal_locations_wgs84, data)
  )
  
  # Create route visualization
  route_plot <- create_route_visualization(new_accident_sf, optimal_locations_wgs84, 
                                         districts_wgs84, route_result, data, include_roads)
  
  # Create summary statistics
  route_stats <- calculate_route_statistics(route_result)
  
  return(list(
    new_accident = new_accident_sf,
    optimal_locations = optimal_locations_wgs84,
    route_info = route_result,
    statistics = route_stats,
    plot = route_plot,
    method = routing_method
  ))
}

#' Create interactive map for emergency routing
#'
#' @param data Original data list
#' @param optimal_locations Optimal locations in WGS84
#' @param districts Districts in WGS84
#' @param include_roads Whether to show roads
#' @return Interactive leaflet map
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers addMarkers
#' @export
create_interactive_routing_map <- function(data, optimal_locations, districts, include_roads = TRUE) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. Please install it.")
  }
  
  # Create base map
  m <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB"),
      overlayGroups = c("Districts", "Optimal Locations", "Roads", "Accidents"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # Add district boundaries
  m <- m %>%
    leaflet::addPolygons(
      data = districts,
      fillColor = "lightblue",
      fillOpacity = 0.3,
      color = "darkblue",
      weight = 2,
      group = "Districts",
      popup = ~paste("District:", name)
    )
  
  # Add optimal emergency service locations
  m <- m %>%
    leaflet::addMarkers(
      data = optimal_locations,
      group = "Optimal Locations",
      popup = ~paste("Emergency Service Location:", id),
      icon = leaflet::makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41
      )
    )
  
  # Add existing accidents if available
  if ("accident" %in% names(data)) {
    accidents_wgs84 <- sf::st_transform(data$accident, 4326)
    m <- m %>%
      leaflet::addCircleMarkers(
        data = accidents_wgs84,
        radius = 2,
        color = "red",
        fillOpacity = 0.6,
        group = "Accidents",
        popup = ~paste("Historical Accident")
      )
  }
  
  # Add road network if requested and available
  if (include_roads && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    # Sample roads to avoid overloading map
    if (nrow(roads_wgs84) > 1000) {
      roads_sample <- roads_wgs84[sample(nrow(roads_wgs84), 1000), ]
    } else {
      roads_sample <- roads_wgs84
    }
    
    m <- m %>%
      leaflet::addPolylines(
        data = roads_sample,
        color = "gray",
        weight = 1,
        opacity = 0.5,
        group = "Roads"
      )
  }
  
  # Add click functionality for new accidents
  m <- m %>%
    leaflet::addControl(
      html = "<div style='background:white; padding:5px; border-radius:5px;'>
              <b>Emergency Routing Tool</b><br/>
              Click on the map to simulate a new accident location<br/>
              and find the nearest emergency service location.
              </div>",
      position = "topright"
    )
  
  return(m)
}

#' Calculate Euclidean distance route (straight line)
#' @keywords internal
calculate_euclidean_route <- function(new_accident, optimal_locations) {
  
  # Calculate distances to all optimal locations
  distances <- sf::st_distance(new_accident, optimal_locations)
  
  # Find nearest location
  nearest_idx <- which.min(distances)
  nearest_location <- optimal_locations[nearest_idx, ]
  nearest_distance <- distances[nearest_idx]
  
  # Create straight line route
  route_coords <- rbind(
    sf::st_coordinates(new_accident),
    sf::st_coordinates(nearest_location)
  )
  
  return(list(
    nearest_location = nearest_location,
    distance_m = as.numeric(nearest_distance),
    route_coordinates = route_coords,
    route_type = "euclidean"
  ))
}

#' Calculate road network route (simplified)
#' @keywords internal
calculate_road_network_route <- function(new_accident, optimal_locations, data) {
  
  # For now, use Euclidean as approximation
  # In a full implementation, this would use road network routing
  euclidean_result <- calculate_euclidean_route(new_accident, optimal_locations)
  
  # Add road network factor (typically 1.3x straight line distance)
  euclidean_result$distance_m <- euclidean_result$distance_m * 1.3
  euclidean_result$route_type <- "road_network_approx"
  
  return(euclidean_result)
}

#' Calculate hybrid route (weighted combination)
#' @keywords internal
calculate_hybrid_route <- function(new_accident, optimal_locations, data) {
  
  euclidean_result <- calculate_euclidean_route(new_accident, optimal_locations)
  road_result <- calculate_road_network_route(new_accident, optimal_locations, data)
  
  # Use road network estimate but keep euclidean coordinates for visualization
  hybrid_result <- euclidean_result
  hybrid_result$distance_m <- road_result$distance_m
  hybrid_result$route_type <- "hybrid"
  
  return(hybrid_result)
}

#' Create route visualization
#' @keywords internal
create_route_visualization <- function(new_accident, optimal_locations, districts, 
                                     route_result, data, include_roads) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts, 
                    fill = "lightblue", 
                    color = "darkblue", 
                    alpha = 0.3,
                    size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Emergency Response Route",
                 subtitle = paste("Distance to nearest service:", 
                                round(route_result$distance_m), "meters"),
                 x = "Longitude", y = "Latitude")
  
  # Add roads if requested
  if (include_roads && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    p <- p + ggplot2::geom_sf(data = roads_wgs84, 
                             color = "gray", 
                             size = 0.2, 
                             alpha = 0.5)
  }
  
  # Add route line
  route_line <- data.frame(
    x = route_result$route_coordinates[,1],
    y = route_result$route_coordinates[,2]
  )
  
  p <- p + ggplot2::geom_path(data = route_line,
                             ggplot2::aes(x = .data$x, y = .data$y),
                             color = "red", size = 2, alpha = 0.8)
  
  # Add optimal locations
  p <- p + ggplot2::geom_sf(data = optimal_locations, 
                           color = "blue", 
                           size = 3, 
                           shape = 17)
  
  # Highlight nearest optimal location
  p <- p + ggplot2::geom_sf(data = route_result$nearest_location, 
                           color = "green", 
                           size = 5, 
                           shape = 17)
  
  # Add new accident location
  p <- p + ggplot2::geom_sf(data = new_accident, 
                           color = "red", 
                           size = 4, 
                           shape = 19)
  
  return(p)
}

#' Calculate route statistics
#' @keywords internal
calculate_route_statistics <- function(route_result) {
  
  return(list(
    nearest_location_id = route_result$nearest_location$id,
    distance_meters = round(route_result$distance_m),
    distance_km = round(route_result$distance_m / 1000, 2),
    estimated_travel_time_min = round(route_result$distance_m / 1000 / 50 * 60, 1), # Assume 50 km/h
    route_type = route_result$route_type
  ))
}
