#' Enhanced Hotspot Analysis for SERO Package
#'
#' Advanced hotspot detection with customizable intensity and visualization options

#' Perform comprehensive hotspot analysis with intensity options
#'
#' @param data List containing spatial data layers (must include 'accident' and 'districts')
#' @param intensity Character, intensity level: "low", "medium", "high", or "custom"
#' @param bandwidth Numeric, bandwidth for kernel density (meters). If NULL, auto-calculated
#' @param include_landuse Logical, whether to include landuse layer in visualization
#' @param contour_levels Numeric vector, contour levels for hotspots (0-1 scale)
#' @return List with hotspot analysis results and plots
#' @importFrom sf st_transform st_coordinates st_crs st_bbox
#' @importFrom ggplot2 ggplot geom_sf geom_point stat_density_2d scale_fill_viridis_c
#' @importFrom dplyr filter
#' @export
sero_hotspot_analysis <- function(data, 
                                 intensity = "medium",
                                 bandwidth = NULL,
                                 include_landuse = FALSE,
                                 contour_levels = c(0.1, 0.3, 0.5, 0.7, 0.9)) {
  
  # Validate input data
  if (!"accident" %in% names(data) || !"districts" %in% names(data)) {
    stop("Data must contain 'accident' and 'districts' layers")
  }
  
  # Transform all data to consistent CRS (WGS84 for visualization)
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  districts_wgs84 <- sf::st_transform(data$districts, 4326)
  
  # Set bandwidth based on intensity
  if (is.null(bandwidth)) {
    bbox <- sf::st_bbox(districts_wgs84)
    map_width <- abs(bbox[3] - bbox[1]) * 111320  # Convert degrees to meters approx
    
    bandwidth <- switch(intensity,
                       "low" = map_width * 0.05,
                       "medium" = map_width * 0.03,
                       "high" = map_width * 0.015,
                       "custom" = map_width * 0.03)
  }
  
  # Get accident coordinates
  accident_coords <- sf::st_coordinates(accidents_wgs84)
  
  # Create base map
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_wgs84, 
                    fill = "white", 
                    color = "black", 
                    alpha = 0.2,
                    size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Accident Hotspots - Munster Districts (", intensity, "intensity)"),
                 subtitle = paste("Total accidents:", nrow(accidents_wgs84), "| Bandwidth:", round(bandwidth), "m"),
                 x = "Longitude", y = "Latitude")
  
  # Add landuse layer if requested
  if (include_landuse && "landuse" %in% names(data)) {
    landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
    p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                             fill = "lightgreen", 
                             color = "darkgreen", 
                             alpha = 0.1, 
                             size = 0.2)
  }
  
  # Add hotspot density contours
  accident_df <- data.frame(x = accident_coords[,1], y = accident_coords[,2])
  p <- p + ggplot2::stat_density_2d_filled(
    data = accident_df,
    ggplot2::aes(x = .data$x, y = .data$y),
    alpha = 0.6,
    h = bandwidth / 111320,  # Convert back to degrees
    contour_var = "ndensity",
    breaks = contour_levels
  ) +
  ggplot2::scale_fill_viridis_c(name = "Density\nLevel", option = "plasma")
  
  # Add accident points
  p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                           color = "red", 
                           size = 0.3, 
                           alpha = 0.7)
  
  # Add district labels
  if ("name" %in% names(districts_wgs84)) {
    district_centroids <- sf::st_centroid(districts_wgs84)
    centroid_coords <- sf::st_coordinates(district_centroids)
    
    district_labels <- data.frame(
      x = centroid_coords[,1],
      y = centroid_coords[,2],
      name = districts_wgs84$name
    )
    
    p <- p + ggplot2::geom_text(
      data = district_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3, fontface = "bold", color = "darkblue"
    )
  }
  
  # Calculate hotspot statistics
  hotspot_stats <- list(
    total_accidents = nrow(accidents_wgs84),
    density_bandwidth = bandwidth,
    intensity_level = intensity,
    bbox = sf::st_bbox(districts_wgs84),
    crs = sf::st_crs(districts_wgs84)
  )
  
  return(list(
    plot = p,
    stats = hotspot_stats,
    accident_coords = accident_coords,
    districts = districts_wgs84
  ))
}

#' Create interactive hotspot map
#'
#' @param hotspot_result Result from sero_hotspot_analysis()
#' @param data Original data list
#' @return leaflet map object
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers
#' @export
sero_interactive_hotspots <- function(hotspot_result, data) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. Please install it.")
  }
  
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  districts_wgs84 <- sf::st_transform(data$districts, 4326)
  
  # Create leaflet map
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      data = districts_wgs84,
      fillColor = "lightblue",
      fillOpacity = 0.3,
      color = "darkblue",
      weight = 2,
      popup = ~paste("District:", name)
    ) %>%
    leaflet::addCircleMarkers(
      data = accidents_wgs84,
      radius = 2,
      color = "red",
      fillOpacity = 0.7,
      popup = ~paste("Accident ID:", row.names(accidents_wgs84))
    )
  
  return(m)
}
