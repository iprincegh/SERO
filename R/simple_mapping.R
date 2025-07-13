#' Simple Emergency Location Optimization
#'
#' Fast and user-friendly emergency location optimization functions

## Plot accidents with severity filtering (static version for speed)
#'
#' @param accidents sf object containing accident data
#' @param districts sf object containing district boundaries
#' @param landuse sf object containing landuse data (optional)
#' @param severity_levels vector of severity levels to display (numeric or character, e.g. c(1,2) or c("fatal","serious"))
#' @param severity_column character name of severity column
#' @param use_osm_basemap logical whether to use OpenStreetMap basemap
#' @param district_border_color Color for district boundary (default="darkblue")
#' @param accident_color Color for accident points (default="red")
#' @param accident_shape Shape for accident points (default=19)
#' @param landuse_border_color Color for land use polygon borders (default="green")
#' @param landuse_fill_color Fill color for land use polygons (default="lightgreen")
#' @param landuse_sample_size Number of land use polygons to sample (default=200)
#' @param export_path Optional file path to export plot (PNG)
#' @return ggplot2 map object
#' @export
sero_plot_accidents <- function(accidents, 
                               districts, 
                               landuse = NULL,
                               severity_levels = c(1, 2, 3),
                               severity_column = "UKATEGORIE",
                               use_osm_basemap = FALSE,
                               district_border_color = "darkblue",
                               accident_color = "red",
                               accident_shape = 19,
                               landuse_border_color = "green",
                               landuse_fill_color = "lightgreen",
                               landuse_sample_size = 200,
                               export_path = NULL) {
  
  # Error handling
  if (missing(accidents) || missing(districts)) {
    stop("Both 'accidents' and 'districts' must be provided.")
  }
  # Transform to WGS84 for plotting
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  # Support risk category by name
  if (is.character(severity_levels)) {
    name_map <- c(fatal=1, serious=2, slight=3)
    severity_levels <- unname(name_map[tolower(severity_levels)])
    if (any(is.na(severity_levels))) {
      warning("Invalid severity names. Use 'fatal', 'serious', or 'slight'.")
      severity_levels <- c(1,2,3)
    }
  }
  # Filter by severity if column exists
  if (severity_column %in% names(accidents_wgs84)) {
    accidents_filtered <- accidents_wgs84[accidents_wgs84[[severity_column]] %in% severity_levels, ]
    message(sprintf("Filtered to %d accidents with severity levels: %s", 
               nrow(accidents_filtered), paste(severity_levels, collapse = ", ")))
  } else {
    accidents_filtered <- accidents_wgs84
    warning("Severity column not found; using all accidents.")
  }
  
  # Create static ggplot (fast and reliable)
  cat("[DIAG] typeof(min):", typeof(min), "\n")
  print(min)
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_wgs84, 
                    fill = "lightblue", 
                    color = district_border_color, 
                    alpha = 0.3,
                    size = 1) +
    ggplot2::geom_sf(data = accidents_filtered, 
                    color = accident_color, 
                    size = 1, 
                    alpha = 0.7,
                    shape = accident_shape) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Emergency Response: Accident Locations",
      subtitle = paste("Showing", nrow(accidents_filtered), "high-risk accidents"),
      x = "Longitude", 
      y = "Latitude"
    )
  
  # Add landuse if provided (sample for performance)
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    sample_size <- base::min(landuse_sample_size, nrow(landuse_wgs84))
    if (nrow(landuse_wgs84) > sample_size) {
      landuse_sample <- landuse_wgs84[sample(nrow(landuse_wgs84), sample_size), ]
      message(sprintf("Sampled %d land use polygons for performance.", sample_size))
    } else {
      landuse_sample <- landuse_wgs84
    }
    p <- p + ggplot2::geom_sf(data = landuse_sample, 
                             fill = landuse_fill_color, 
                             color = landuse_border_color, 
                             alpha = 0.2, 
                             size = 0.2)
  }
  # Export plot if requested
  if (!is.null(export_path)) {
    ggplot2::ggsave(export_path, plot = p, width = 8, height = 6)
    message(sprintf("Plot exported to %s", export_path))
  }
  
  return(p)
}

## Plot optimal locations with professional styling (fast version)
#'
#' @param optimal_locs sf object with emergency service locations
#' @param districts sf object with district boundaries
#' @param roads sf object with road network (optional)
#' @param landuse sf object with landuse data (optional)  
#' @param accidents sf object with accident data (optional)
#' @param district_border_color Color for district boundary (default="black")
#' @param road_border_color Color for road lines (default="gray")
#' @param landuse_border_color Color for land use polygon borders (default="green")
#' @param landuse_fill_color Fill color for land use polygons (default="lightgreen")
#' @param landuse_sample_size Number of land use polygons to sample (default=200)
#' @param road_sample_size Number of road segments to sample (default=500)
#' @param optimal_color Color for optimal locations (default="red")
#' @param optimal_shape Shape for optimal locations (default=17)
#' @param export_path Optional file path to export plot (PNG)
#' @return ggplot2 object
#' @export
sero_plot_optimal_locations <- function(optimal_locs, 
                                       districts, 
                                       roads = NULL, 
                                       landuse = NULL, 
                                       accidents = NULL,
                                       district_border_color = "black",
                                       road_border_color = "gray",
                                       landuse_border_color = "green",
                                       landuse_fill_color = "lightgreen",
                                       landuse_sample_size = 200,
                                       road_sample_size = 500,
                                       optimal_color = "red",
                                       optimal_shape = 17,
                                       export_path = NULL) {
  
  # Error handling
  if (missing(optimal_locs) || missing(districts)) {
    stop("Both 'optimal_locs' and 'districts' must be provided.")
  }
  # Transform all to WGS84
  optimal_wgs84 <- sf::st_transform(optimal_locs, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  # Create base map
  cat("[DIAG] typeof(min):", typeof(min), "\n")
  print(min)
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_wgs84, 
                    fill = "lightgray", 
                    color = district_border_color, 
                    alpha = 0.3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Optimal Emergency Service Locations",
      subtitle = paste("Found", nrow(optimal_wgs84), "strategic locations"),
      x = "Longitude", 
      y = "Latitude"
    )
  
  # Add roads if provided (sample for performance)
  if (!is.null(roads)) {
    roads_wgs84 <- sf::st_transform(roads, 4326)
    sample_size <- base::min(road_sample_size, nrow(roads_wgs84))
    if (nrow(roads_wgs84) > sample_size) {
      roads_sample <- roads_wgs84[sample(nrow(roads_wgs84), sample_size), ]
      message(sprintf("Sampled %d road segments for performance.", sample_size))
    } else {
      roads_sample <- roads_wgs84
    }
    p <- p + ggplot2::geom_sf(data = roads_sample, 
                             color = road_border_color, 
                             size = 0.3, 
                             alpha = 0.5)
  }
  
  # Add landuse if provided
  if (!is.null(landuse)) {
    landuse_wgs84 <- sf::st_transform(landuse, 4326)
    sample_size <- base::min(landuse_sample_size, nrow(landuse_wgs84))
    if (nrow(landuse_wgs84) > sample_size) {
      landuse_sample <- landuse_wgs84[sample(nrow(landuse_wgs84), sample_size), ]
      message(sprintf("Sampled %d land use polygons for performance.", sample_size))
    } else {
      landuse_sample <- landuse_wgs84
    }
    p <- p + ggplot2::geom_sf(data = landuse_sample, 
                             fill = landuse_fill_color, 
                             color = landuse_border_color, 
                             alpha = 0.2, 
                             size = 0.2)
  }
  
  # Add accidents if provided
  if (!is.null(accidents)) {
    accidents_wgs84 <- sf::st_transform(accidents, 4326)
    p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                             color = "orange", 
                             size = 0.3, 
                             alpha = 0.6)
  }
  
  # Add optimal locations with size based on accident count if available
  if ("accident_count_500m" %in% names(optimal_wgs84)) {
    p <- p + ggplot2::geom_sf(data = optimal_wgs84, 
                             ggplot2::aes(size = .data$accident_count_500m),
                             color = optimal_color, 
                             shape = optimal_shape) +
      ggplot2::scale_size_continuous(name = "Accidents\n(500m radius)", 
                                    range = c(3, 8))
  } else {
    p <- p + ggplot2::geom_sf(data = optimal_wgs84, 
                             color = optimal_color, 
                             size = 5, 
                             shape = optimal_shape)
  }
  
  p <- p + ggplot2::labs(caption = "Red triangles = Emergency bases")
  # Export plot if requested
  if (!is.null(export_path)) {
    ggplot2::ggsave(export_path, plot = p, width = 8, height = 6)
    message(sprintf("Plot exported to %s", export_path))
  }
  return(p)
}

#' Save optimal locations to spatialite database
#'
#' @param optimal_locations sf object with optimal locations
#' @param db_path path to sqlite database file
#' @export
sero_save_optimal_locations <- function(optimal_locations, db_path) {
  
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    warning("RSQLite package required for database operations")
    return(invisible(NULL))
  }
  
  tryCatch({
    # Convert to data frame for storage
    df <- optimal_locations
    df$lon <- sf::st_coordinates(optimal_locations)[, 1]
    df$lat <- sf::st_coordinates(optimal_locations)[, 2]
    df$geometry <- NULL
    
    # Save to database
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbWriteTable(conn, "optimal_locations", df, overwrite = TRUE)
    RSQLite::dbDisconnect(conn)
    
    cat("Optimal locations saved to database:", db_path, "\n")
    
  }, error = function(e) {
    warning("Database save failed: ", e$message)
  })
}
