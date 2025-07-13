#' Professional Accident Visualization with Severity Filtering
#'
#' Enhanced accident plotting with OSM basemap, severity filtering, and landuse integration

#' Plot accidents with severity filtering and professional styling
#'
#' @param accidents sf object containing accident data
#' @param districts sf object containing district boundaries
#' @param landuse sf object containing landuse data (optional)
#' @param severity_levels vector of severity levels to display (e.g., c(1,2,3))
#' @param severity_column character name of severity column (default: "UKATEGORIE")
#' @param use_osm_basemap logical whether to use OpenStreetMap basemap
#' @return ggplot2 or leaflet map object
#' @importFrom sf st_transform st_bbox
#' @importFrom ggplot2 ggplot geom_sf scale_color_manual theme_minimal labs
#' @importFrom dplyr filter
#' @export
sero_plot_accidents <- function(accidents, 
                               districts, 
                               landuse = NULL,
                               severity_levels = c(1, 2, 3),
                               severity_column = "UKATEGORIE",
                               use_osm_basemap = TRUE) {
  
  # Transform to WGS84 for plotting
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  
  # Filter by severity if column exists
  if (severity_column %in% names(accidents_wgs84)) {
    accidents_filtered <- accidents_wgs84[accidents_wgs84[[severity_column]] %in% severity_levels, ]
    cat(sprintf("Filtered to %d accidents with severity levels: %s\n", 
               nrow(accidents_filtered), paste(severity_levels, collapse = ", ")))
  } else {
    accidents_filtered <- accidents_wgs84
    cat(sprintf("Severity column '%s' not found, showing all %d accidents\n", 
               severity_column, nrow(accidents_filtered)))
  }
  
  if (use_osm_basemap && requireNamespace("leaflet", quietly = TRUE)) {
    return(create_professional_accident_map(accidents_filtered, districts_wgs84, landuse))
  } else {
    return(create_static_accident_plot(accidents_filtered, districts_wgs84, landuse, severity_column))
  }
}

#' Create professional leaflet map for accidents
#' @keywords internal
create_professional_accident_map <- function(accidents, districts, landuse) {
  
  # Define severity colors
  severity_colors <- c("1" = "#FF0000", "2" = "#FF8C00", "3" = "#FFD700")
  
  m <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
      overlayGroups = c("Districts", "Accidents", "Landuse"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # Add district boundaries
  m <- m %>%
    leaflet::addPolygons(
      data = districts,
      fillColor = "transparent",
      color = "#2E4057",
      weight = 2,
      opacity = 0.8,
      group = "Districts",
      popup = ~ifelse(exists("name"), paste("District:", name), "District boundary")
    )
  
  # Add landuse if provided
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    m <- m %>%
      leaflet::addPolygons(
        data = landuse_wgs84,
        fillColor = "#90EE90",
        fillOpacity = 0.3,
        color = "#228B22",
        weight = 0.5,
        group = "Landuse",
        popup = ~ifelse(exists("type"), paste("Landuse:", type), "Landuse area")
      )
  }
  
  # Add accidents with severity-based styling
  if ("UKATEGORIE" %in% names(accidents)) {
    for (severity in unique(accidents$UKATEGORIE)) {
      severity_accidents <- accidents[accidents$UKATEGORIE == severity, ]
      severity_label <- switch(as.character(severity),
                              "1" = "Fatal",
                              "2" = "Serious Injury", 
                              "3" = "Minor Injury",
                              paste("Severity", severity))
      
      m <- m %>%
        leaflet::addCircleMarkers(
          data = severity_accidents,
          radius = ifelse(severity == 1, 6, ifelse(severity == 2, 4, 2)),
          color = severity_colors[as.character(severity)],
          fillColor = severity_colors[as.character(severity)],
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          group = "Accidents",
          popup = ~paste(
            "Severity:", severity_label, "<br>",
            "Location: (", round(st_coordinates(.)[,1], 4), ",", 
            round(st_coordinates(.)[,2], 4), ")"
          ),
          label = ~severity_label
        )
    }
  } else {
    m <- m %>%
      leaflet::addCircleMarkers(
        data = accidents,
        radius = 3,
        color = "#FF0000",
        fillColor = "#FF0000",
        fillOpacity = 0.7,
        group = "Accidents",
        popup = ~paste("Accident location")
      )
  }
  
  # Center map on districts
  bbox <- sf::st_bbox(districts)
  m <- m %>%
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  
  return(m)
}

#' Create static ggplot for accidents
#' @keywords internal
create_static_accident_plot <- function(accidents, districts, landuse, severity_column) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts, 
                    fill = "lightblue", 
                    color = "#2E4057", 
                    alpha = 0.3,
                    size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Accident Locations by Severity",
                 subtitle = paste("Total accidents:", nrow(accidents)),
                 x = "Longitude", y = "Latitude")
  
  # Add landuse if provided
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                             fill = "#90EE90", 
                             color = "#228B22", 
                             alpha = 0.2, 
                             size = 0.1)
  }
  
  # Add accidents with severity coloring
  if (severity_column %in% names(accidents)) {
    p <- p + ggplot2::geom_sf(data = accidents, 
                             ggplot2::aes(color = factor(.data[[severity_column]])),
                             size = 1.5, alpha = 0.8) +
      ggplot2::scale_color_manual(
        name = "Severity",
        values = c("1" = "#FF0000", "2" = "#FF8C00", "3" = "#FFD700"),
        labels = c("1" = "Fatal", "2" = "Serious", "3" = "Minor")
      )
  } else {
    p <- p + ggplot2::geom_sf(data = accidents, 
                             color = "#FF0000", 
                             size = 1, alpha = 0.7)
  }
  
  return(p)
}

#' Enhanced optimal location calculation with spatialite database support
#'
#' @param data list containing spatial data layers
#' @param grid_size numeric grid size for location search (meters)
#' @param risk_categories vector of risk categories to consider
#' @param min_road_distance minimum distance to roads (meters)
#' @param max_road_distance maximum distance to roads (meters)
#' @param save_to_db logical whether to save to spatialite database
#' @param db_path path for spatialite database
#' @return sf object with optimal locations
#' @importFrom sf st_transform st_buffer st_intersection st_distance
#' @export
sero_calculate_optimal_locations <- function(data,
                                           grid_size = 100,
                                           risk_categories = c(1, 2),
                                           min_road_distance = 500,
                                           max_road_distance = 1000,
                                           save_to_db = TRUE,
                                           db_path = "sero_optimal_locations.sqlite") {
  
  # Validate input data
  required_layers <- c("accident", "districts", "roads")
  missing_layers <- setdiff(required_layers, names(data))
  if (length(missing_layers) > 0) {
    stop(paste("Missing required data layers:", paste(missing_layers, collapse = ", ")))
  }
  
  # Transform to consistent CRS (UTM for distance calculations)
  crs_utm <- 32632  # UTM Zone 32N for Germany
  accidents_utm <- sf::st_transform(data$accident, crs_utm)
  districts_utm <- sf::st_transform(data$districts, crs_utm)
  roads_utm <- sf::st_transform(data$roads, crs_utm)
  
  # Filter accidents by risk categories if available
  if ("UKATEGORIE" %in% names(accidents_utm)) {
    accidents_filtered <- accidents_utm[accidents_utm$UKATEGORIE %in% risk_categories, ]
    cat(sprintf("Using %d high-risk accidents (categories %s)\n", 
               nrow(accidents_filtered), paste(risk_categories, collapse = ", ")))
  } else {
    accidents_filtered <- accidents_utm
    cat(sprintf("Using all %d accidents\n", nrow(accidents_filtered)))
  }
  
  # Create analysis grid
  bbox <- sf::st_bbox(districts_utm)
  grid_points <- expand.grid(
    x = seq(bbox[1], bbox[3], by = grid_size),
    y = seq(bbox[2], bbox[4], by = grid_size)
  )
  
  grid_sf <- sf::st_as_sf(grid_points, coords = c("x", "y"), crs = crs_utm)
  
  # Filter grid points within districts
  grid_within <- sf::st_intersection(grid_sf, districts_utm)
  cat(sprintf("Evaluating %d grid points within districts\n", nrow(grid_within)))
  
  # Calculate accessibility scores for each grid point
  cat("Calculating accessibility scores...\n")
  accessibility_scores <- calculate_accessibility_scores(
    grid_within, accidents_filtered, roads_utm, 
    min_road_distance, max_road_distance
  )
  
  # Select optimal locations
  optimal_indices <- select_optimal_locations(accessibility_scores, num_locations = 5)
  optimal_locations_utm <- grid_within[optimal_indices, ]
  
  # Add metadata
  optimal_locations_utm$location_id <- paste0("BASE_", seq_len(nrow(optimal_locations_utm)))
  optimal_locations_utm$accessibility_score <- accessibility_scores[optimal_indices]
  optimal_locations_utm$accident_count_500m <- calculate_accident_counts(
    optimal_locations_utm, accidents_filtered, 500
  )
  optimal_locations_utm$accident_count_1000m <- calculate_accident_counts(
    optimal_locations_utm, accidents_filtered, 1000
  )
  
  # Transform back to WGS84 for output
  optimal_locations <- sf::st_transform(optimal_locations_utm, 4326)
  
  # Save to spatialite database if requested
  if (save_to_db) {
    sero_save_optimal_locations(optimal_locations, db_path)
  }
  
  cat(sprintf("Found %d optimal emergency service locations\n", nrow(optimal_locations)))
  return(optimal_locations)
}

#' Calculate accessibility scores for grid points
#' @keywords internal
calculate_accessibility_scores <- function(grid_points, accidents, roads, 
                                         min_road_dist, max_road_dist) {
  
  scores <- numeric(nrow(grid_points))
  
  for (i in seq_len(nrow(grid_points))) {
    point <- grid_points[i, ]
    
    # Distance to accidents (inverse distance weighting)
    accident_distances <- as.numeric(sf::st_distance(point, accidents))
    accident_score <- sum(1 / pmax(accident_distances, 1))
    
    # Distance to roads (penalty for being too far or too close)
    road_distances <- as.numeric(sf::st_distance(point, roads))
    min_road_dist_actual <- min(road_distances)
    
    if (min_road_dist_actual < min_road_dist) {
      road_penalty <- (min_road_dist - min_road_dist_actual) / min_road_dist
    } else if (min_road_dist_actual > max_road_dist) {
      road_penalty <- (min_road_dist_actual - max_road_dist) / max_road_dist
    } else {
      road_penalty <- 0
    }
    
    scores[i] <- accident_score * (1 - road_penalty)
  }
  
  return(scores)
}

#' Select optimal locations from scored grid
#' @keywords internal
select_optimal_locations <- function(scores, num_locations) {
  # Select top scoring locations with minimum separation
  selected_indices <- c()
  remaining_indices <- seq_along(scores)
  
  for (i in seq_len(num_locations)) {
    if (length(remaining_indices) == 0) break
    
    # Select highest scoring remaining location
    best_idx <- remaining_indices[which.max(scores[remaining_indices])]
    selected_indices <- c(selected_indices, best_idx)
    
    # Remove nearby locations to ensure separation
    # (simplified - in full implementation would use spatial distance)
    nearby_indices <- which(abs(remaining_indices - best_idx) < 10)
    remaining_indices <- setdiff(remaining_indices, nearby_indices)
  }
  
  return(selected_indices)
}

#' Calculate accident counts within buffer
#' @keywords internal
calculate_accident_counts <- function(locations, accidents, buffer_distance) {
  counts <- numeric(nrow(locations))
  
  for (i in seq_len(nrow(locations))) {
    buffer <- sf::st_buffer(locations[i, ], buffer_distance)
    within_buffer <- sf::st_intersects(accidents, buffer, sparse = FALSE)
    counts[i] <- sum(within_buffer)
  }
  
  return(counts)
}

#' Save optimal locations to spatialite database
#'
#' @param optimal_locations sf object with optimal locations
#' @param db_path path to spatialite database file
#' @return logical indicating success
#' @export
sero_save_optimal_locations <- function(optimal_locations, db_path) {
  
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    warning("RSQLite package required for database functionality")
    return(FALSE)
  }
  
  tryCatch({
    # Create/connect to database
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    
    # Write spatial data (simplified - would use proper spatialite in production)
    coords <- sf::st_coordinates(optimal_locations)
    optimal_df <- data.frame(
      location_id = optimal_locations$location_id,
      longitude = coords[, 1],
      latitude = coords[, 2],
      accessibility_score = optimal_locations$accessibility_score,
      accident_count_500m = optimal_locations$accident_count_500m,
      accident_count_1000m = optimal_locations$accident_count_1000m,
      created_date = Sys.time()
    )
    
    RSQLite::dbWriteTable(con, "optimal_locations", optimal_df, overwrite = TRUE)
    RSQLite::dbDisconnect(con)
    
    cat(sprintf("Optimal locations saved to: %s\n", db_path))
    return(TRUE)
    
  }, error = function(e) {
    warning(paste("Failed to save to database:", e$message))
    return(FALSE)
  })
}

#' Plot optimal locations with professional styling
#'
#' @param optimal_locs sf object containing optimal locations
#' @param districts sf object containing district boundaries  
#' @param roads sf object containing road network
#' @param landuse sf object containing landuse data (optional)
#' @param accidents sf object containing accidents for context (optional)
#' @return leaflet or ggplot map object
#' @importFrom sf st_transform st_bbox
#' @export
sero_plot_optimal_locations <- function(optimal_locs, 
                                       districts, 
                                       roads, 
                                       landuse = NULL,
                                       accidents = NULL) {
  
  # Transform to WGS84
  optimal_locs_wgs84 <- sf::st_transform(optimal_locs, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  roads_wgs84 <- sf::st_transform(roads, 4326)
  
  if (requireNamespace("leaflet", quietly = TRUE)) {
    return(create_professional_optimal_map(optimal_locs_wgs84, districts_wgs84, 
                                         roads_wgs84, landuse, accidents))
  } else {
    return(create_static_optimal_plot(optimal_locs_wgs84, districts_wgs84, 
                                    roads_wgs84, landuse, accidents))
  }
}

#' Create professional leaflet map for optimal locations
#' @keywords internal  
create_professional_optimal_map <- function(optimal_locs, districts, roads, landuse, accidents) {
  
  m <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light"),
      overlayGroups = c("Districts", "Roads", "Optimal Locations", "Landuse", "Accidents"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # Add district boundaries
  m <- m %>%
    leaflet::addPolygons(
      data = districts,
      fillColor = "transparent",
      color = "#2E4057",
      weight = 2,
      group = "Districts"
    )
  
  # Sample roads to avoid performance issues
  if (nrow(roads) > 2000) {
    roads_sample <- roads[sample(nrow(roads), 2000), ]
  } else {
    roads_sample <- roads
  }
  
  # Add road network
  m <- m %>%
    leaflet::addPolylines(
      data = roads_sample,
      color = "#888",
      opacity = 0.6,
      weight = 1,
      group = "Roads"
    )
  
  # Add landuse if provided
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    m <- m %>%
      leaflet::addPolygons(
        data = landuse_wgs84,
        fillColor = "#90EE90",
        fillOpacity = 0.2,
        color = "#228B22",
        weight = 0.5,
        group = "Landuse"
      )
  }
  
  # Add accidents for context if provided
  if (!is.null(accidents)) {
    accidents_wgs84 <- sf::st_transform(accidents, 4326)
    m <- m %>%
      leaflet::addCircleMarkers(
        data = accidents_wgs84,
        radius = 2,
        color = "#FF6B6B",
        fillOpacity = 0.6,
        group = "Accidents"
      )
  }
  
  # Add optimal locations with sizing based on accident count
  if ("accident_count_500m" %in% names(optimal_locs)) {
    max_count <- max(optimal_locs$accident_count_500m, na.rm = TRUE)
    marker_sizes <- pmax(8, pmin(20, 8 + (optimal_locs$accident_count_500m / max_count) * 12))
  } else {
    marker_sizes <- rep(12, nrow(optimal_locs))
  }
  
  m <- m %>%
    leaflet::addCircleMarkers(
      data = optimal_locs,
      radius = marker_sizes,
      color = "#0066CC",
      fillColor = "#4A90E2",
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2,
      group = "Optimal Locations",
      popup = ~paste(
        "<b>Emergency Base:</b>", location_id, "<br>",
        "<b>Accidents (500m):</b>", ifelse(exists("accident_count_500m"), accident_count_500m, "N/A"), "<br>",
        "<b>Accessibility Score:</b>", round(accessibility_score, 2)
      ),
      label = ~location_id
    )
  
  # Center map
  bbox <- sf::st_bbox(districts)
  m <- m %>%
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  
  return(m)
}

#' Create static ggplot for optimal locations  
#' @keywords internal
create_static_optimal_plot <- function(optimal_locs, districts, roads, landuse, accidents) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts, 
                    fill = "lightblue", 
                    color = "#2E4057", 
                    alpha = 0.3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Optimal Emergency Service Locations",
                 subtitle = paste("Total locations:", nrow(optimal_locs)))
  
  # Add roads
  roads_sample <- if (nrow(roads) > 1000) roads[sample(nrow(roads), 1000), ] else roads
  p <- p + ggplot2::geom_sf(data = roads_sample, color = "#888", size = 0.3, alpha = 0.6)
  
  # Add landuse
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    p <- p + ggplot2::geom_sf(data = landuse_wgs84, fill = "#90EE90", alpha = 0.2, size = 0.1)
  }
  
  # Add accidents for context
  if (!is.null(accidents)) {
    accidents_wgs84 <- sf::st_transform(accidents, 4326)
    p <- p + ggplot2::geom_sf(data = accidents_wgs84, color = "#FF6B6B", size = 0.5, alpha = 0.6)
  }
  
  # Add optimal locations
  if ("accident_count_500m" %in% names(optimal_locs)) {
    p <- p + ggplot2::geom_sf(data = optimal_locs, 
                             ggplot2::aes(size = .data$accident_count_500m),
                             color = "#0066CC", alpha = 0.8) +
      ggplot2::scale_size_continuous(name = "Accidents\n(500m)", range = c(2, 8))
  } else {
    p <- p + ggplot2::geom_sf(data = optimal_locs, color = "#0066CC", size = 4, alpha = 0.8)
  }
  
  return(p)
}
