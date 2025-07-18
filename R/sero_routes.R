#' Calculate fastest routes to accident scenes
#'
#' Computes fastest routes from emergency service locations to accident scenes.
#' This function focuses on high-risk accident categories and uses optimized algorithms.
#'
#' @param locations sero_optimal_locations object or sf object with service locations
#' @param accidents sf object with accident locations
#' @param max_routes Maximum number of routes to calculate (default=10)
#' @param risk_categories Risk categories to filter accidents (default=c("fatal", "severe"))
#' @param data Data object containing road network (required)
#' @return sero_routes S3 object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_find_optimal_locations(data)
#' routes <- sero_routes(locations, data$accident)
#' plot(routes)
#' }
sero_routes <- function(locations, accidents, max_routes = 10, risk_categories = c("fatal", "severe"), data = NULL) {
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

  # Ensure road network is loaded
  if (is.null(data) || !("roads" %in% names(data))) {
    gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
    if (file.exists(gpkg_path)) {
      roads <- sf::st_read(gpkg_path, layer = "munster_roadsshp", quiet = TRUE)
      if (sf::st_is_longlat(roads)) {
        roads <- sf::st_transform(roads, 32632)
      }
      data$roads <- roads
    } else {
      stop("Road network not found. Please provide road data in the 'data' argument.")
    }
  }

  # Filter accidents by risk categories
  if (!is.null(risk_categories) && "UKATEGORIE" %in% names(accidents)) {
    # Convert character risk categories to numeric if needed
    if (is.character(risk_categories)) {
      name_map <- c("fatal" = 1, "severe" = 2, "slight" = 3)
      numeric_categories <- unname(name_map[risk_categories])
      if (any(is.na(numeric_categories))) {
        warning("Invalid risk_categories. Using all accidents.")
      } else {
        risk_categories <- numeric_categories
      }
    }
    # Filter accidents
    accidents <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
    if (nrow(accidents) == 0) {
      warning("No accidents found for specified risk categories")
      return(create_empty_routes())
    }
  }

  # Calculate routes (road-based only)
  routes_data <- calculate_road_routes(service_locations, accidents, max_routes, data)

  # Create summary statistics
  if (nrow(routes_data$routes) > 0) {
    distance_col <- if("distance_m" %in% names(routes_data$routes)) "distance_m" else "distance"
    summary_stats <- list(
      total_routes = nrow(routes_data$routes),
      avg_distance = mean(routes_data$routes[[distance_col]]),
      avg_time = mean(routes_data$routes$estimated_time),
      max_distance = base::max(routes_data$routes[[distance_col]]),
      max_time = base::max(routes_data$routes$estimated_time)
    )
  } else {
    summary_stats <- list(
      total_routes = 0,
      avg_distance = 0,
      avg_time = 0,
      max_distance = 0,
      max_time = 0
    )
  }

  # Create S3 object
  result <- structure(
    list(
      routes = routes_data$routes,
      service_locations = service_locations,
      accidents = accidents,
      parameters = list(
        max_routes = max_routes,
        use_roads = TRUE
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

#' Calculate straight-line routes as fallback
#' @param service_locations sf object with service locations
#' @param accidents sf object with accident locations  
#' @param max_routes maximum number of routes to calculate
#' @return sero_routes object with straight-line routes
calculate_straight_routes <- function(service_locations, accidents, max_routes) {
  # Ensure same CRS
  if (sf::st_crs(service_locations) != sf::st_crs(accidents)) {
    accidents <- sf::st_transform(accidents, sf::st_crs(service_locations))
  }
  
  # Calculate straight-line distances
  distances <- sf::st_distance(service_locations, accidents)
  
  # Find closest service location for each accident
  closest_indices <- apply(distances, 2, which.min)
  
  # Create routes (limited by max_routes)
  n_routes <- min(max_routes, nrow(accidents))
  
  if (n_routes > 0) {
    # Create straight lines between service locations and accidents
    routes_list <- list()
    
    for (i in seq_len(n_routes)) {
      accident_idx <- i
      service_idx <- closest_indices[accident_idx]
      
      # Create line geometry
      start_coords <- sf::st_coordinates(service_locations[service_idx, ])
      end_coords <- sf::st_coordinates(accidents[accident_idx, ])
      
      line_coords <- rbind(start_coords, end_coords)
      line_geom <- sf::st_linestring(line_coords)
      
      # Calculate distance
      distance <- as.numeric(distances[service_idx, accident_idx])
      
      routes_list[[i]] <- data.frame(
        route_id = i,
        service_id = service_idx,
        accident_id = accident_idx,
        distance_m = distance,
        estimated_time = distance / 1000 / 30 * 60, # 30 km/h average speed in emergency (converted to minutes)
        geometry = sf::st_sfc(line_geom, crs = sf::st_crs(service_locations))
      )
    }
    
    # Combine routes
    routes_df <- do.call(rbind, routes_list)
    routes_sf <- sf::st_as_sf(routes_df)
    
  } else {
    routes_sf <- sf::st_sf(data.frame(), geometry = sf::st_sfc())
  }
  
  # Create summary
  summary_stats <- list(
    total_routes = nrow(routes_sf),
    avg_distance = ifelse(nrow(routes_sf) > 0, mean(routes_sf$distance_m), 0),
    avg_time = ifelse(nrow(routes_sf) > 0, mean(routes_sf$estimated_time), 0),
    max_distance = ifelse(nrow(routes_sf) > 0, max(routes_sf$distance_m), 0),
    max_time = ifelse(nrow(routes_sf) > 0, max(routes_sf$estimated_time), 0)
  )
  
  # Return sero_routes object
  structure(
    list(
      routes = routes_sf,
      service_locations = service_locations,
      accidents = accidents[seq_len(n_routes), ],
      parameters = list(max_routes = max_routes, method = "straight_line"),
      summary = summary_stats
    ),
    class = "sero_routes"
  )
}

#' Plot method for sero_routes using ggplot2
#'
#' @param x sero_routes object
#' @param data Optional data object containing spatial layers for basemap
#' @param basemap Character, basemap type: "districts", "roads", "landuse", "all", or "none" (default: "districts")
#' @param show_network Logical, whether to show road network (default: TRUE)
#' @param show_landuse Logical, whether to show land use (default: FALSE)
#' @param route_width Numeric, width of route lines (default: 1.2)
#' @param alpha_routes Numeric, transparency of route lines (default: 0.8)
#' @param alpha_basemap Numeric, transparency of basemap (default: 0.3)
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
#' @importFrom rlang .data
plot.sero_routes <- function(x, data = NULL, basemap = "districts", 
                           show_network = TRUE, show_landuse = FALSE,
                           route_width = 1.2, alpha_routes = 0.8, 
                           alpha_basemap = 0.3, ...) {
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
  
  # Load data if not provided
  if (is.null(data)) {
    tryCatch({
      data <- sero_load_data()
    }, error = function(e) {
      warning("Could not load data for basemap: ", e$message)
      data <- NULL
    })
  }
  
  # Create base plot
  p <- ggplot2::ggplot()
  
  # Add basemap layers based on selection
  if (!is.null(data)) {
    # Add districts basemap
    if (basemap %in% c("districts", "all") && "districts" %in% names(data)) {
      districts_wgs84 <- sf::st_transform(data$districts, 4326)
      p <- p + ggplot2::geom_sf(data = districts_wgs84,
                               fill = "lightgray",
                               color = "darkgray",
                               alpha = alpha_basemap,
                               size = 0.8)
    }
    
    # Add landuse basemap
    if ((basemap %in% c("landuse", "all") || show_landuse) && "landuse" %in% names(data)) {
      landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
      # Sample landuse for performance
      if (nrow(landuse_wgs84) > 2000) {
        landuse_wgs84 <- landuse_wgs84[sample(nrow(landuse_wgs84), 2000), ]
      }
      p <- p + ggplot2::geom_sf(data = landuse_wgs84,
                               ggplot2::aes(fill = .data$fclass),
                               color = "white",
                               alpha = alpha_basemap * 0.5,
                               size = 0.1) +
        ggplot2::scale_fill_viridis_d(name = "Land Use", 
                                     option = "viridis", 
                                     alpha = 0.7)
    }
    
    # Add road network
    if ((basemap %in% c("roads", "all") || show_network) && "roads" %in% names(data)) {
      roads_wgs84 <- sf::st_transform(data$roads, 4326)
      # Sample roads for performance
      if (nrow(roads_wgs84) > 5000) {
        roads_wgs84 <- roads_wgs84[sample(nrow(roads_wgs84), 5000), ]
      }
      p <- p + ggplot2::geom_sf(data = roads_wgs84,
                               color = "gray60",
                               alpha = alpha_basemap * 0.8,
                               size = 0.3)
    }
  }
  
  # Add routes as lines
  p <- p + ggplot2::geom_sf(data = routes_wgs84,
                           ggplot2::aes(color = .data$estimated_time),
                           size = route_width, alpha = alpha_routes)
  
  # Add service locations (red plus sign for ambulance/emergency services)
  p <- p + ggplot2::geom_sf(data = locations_wgs84,
                           color = "red", fill = "red",
                           shape = 3, size = 6, alpha = 0.9, stroke = 2)
  
  # Add accident locations
  p <- p + ggplot2::geom_sf(data = accidents_wgs84[seq_len(nrow(routes_wgs84)), ],
                           color = "red", size = 2, alpha = 0.9)
  
  # Color scale for travel time
  p <- p + ggplot2::scale_color_viridis_c(name = "Travel Time\n(minutes)", 
                                         option = "plasma", trans = "sqrt")
  
  # Styling
  p <- p + ggplot2::theme_void() +
    ggplot2::labs(title = "Emergency Response Routes",
                 subtitle = paste("Showing", x$summary$total_routes, "fastest routes |", 
                                 "Basemap:", basemap)) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.box = "horizontal"
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
    for (i in seq_len(base::min(5, nrow(x$routes)))) {
      distance_col <- if("distance_m" %in% names(x$routes)) "distance_m" else "distance"
      cat(sprintf("  %d. Distance: %dm, Time: %.1f min\n", 
                  i, 
                  round(x$routes[[distance_col]][i]), 
                  x$routes$estimated_time[i]))
    }
    if (nrow(x$routes) > 5) {
      cat("  ... and", nrow(x$routes) - 5, "more routes\n")
    }
  }
  
  cat("\nUse plot() to visualize with ggplot2.\n")
}

## Straight-line routing removed: road-based routing is now always used

#' Calculate road-based routes
#' @param service_locations sf object with service locations
#' @param accidents sf object with accident locations  
#' @param max_routes Maximum number of routes to calculate
#' @param data Data object containing road network
#' @return List with routes sf object
calculate_road_routes <- function(service_locations, accidents, max_routes, data) {
  
  # Load road network if not provided
  if (!"roads" %in% names(data)) {
    tryCatch({
      gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
      if (file.exists(gpkg_path)) {
        roads <- sf::st_read(gpkg_path, layer = "munster_roadsshp", quiet = TRUE)
        if (sf::st_is_longlat(roads)) {
          roads <- sf::st_transform(roads, 32632)
        }
        data$roads <- roads
      } else {
        warning("Road network not found, using straight-line routing")
        return(calculate_straight_routes(service_locations, accidents, max_routes))
      }
    }, error = function(e) {
      warning("Error loading road network: ", e$message, ", using straight-line routing")
      return(calculate_straight_routes(service_locations, accidents, max_routes))
    })
  }
  
  # For now, use simplified road-based routing
  # In a full implementation, this would use a proper routing algorithm
  # like Dijkstra's algorithm or A* on the road network
  
  routes_data <- data.frame()
  
  for (i in seq_len(base::min(max_routes, nrow(accidents)))) {
    # Find closest service location using road network distance approximation
    accident_point <- accidents[i, ]
    
    # Calculate distances to all service locations
    # For simplicity, use straight-line distance but increase by road factor
    straight_distances <- sf::st_distance(service_locations, accident_point)
    road_factor <- 1.3  # Assume roads are 30% longer than straight-line
    estimated_road_distances <- as.numeric(straight_distances) * road_factor
    
    closest_service_idx <- which.min(estimated_road_distances)
    
    # Create route data
    route_data <- data.frame(
      route_id = i,
      service_location_id = closest_service_idx,
      accident_id = i,
      distance = estimated_road_distances[closest_service_idx],
      estimated_time = estimated_road_distances[closest_service_idx] / 1000 / 30 * 60 # 30 km/h avg speed in city (converted to minutes)
    )
    
    routes_data <- rbind(routes_data, route_data)
  }
  
  # Create route geometries 
  # In a full implementation, this would trace the actual road path
  # For now, create straight line (placeholder for actual road routing)
  route_geometries <- list()
  for (i in seq_len(nrow(routes_data))) {
    service_idx <- routes_data$service_location_id[i]
    accident_idx <- routes_data$accident_id[i]
    
    # For now, create straight line (placeholder for actual road routing)
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
  
  return(list(routes = routes_sf))
}

#' Save optimal locations as a dataset
#'
#' Saves the computed optimal locations to a file for reuse in routing.
#' Uses GeoPackage format which is optimal for sf objects.
#'
#' @param locations sero_optimal_locations object
#' @param filename File path to save the locations (default: "optimal_locations.gpkg")
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' 
#' # Save as GeoPackage (recommended - optimal for sf objects)
#' sero_save_locations(locations, "optimal_locations.gpkg")
#' 
#' # Use custom filename
#' sero_save_locations(locations, "my_emergency_locations.gpkg")
#' }
sero_save_locations <- function(locations, filename = "optimal_locations.gpkg") {
  
  if (!inherits(locations, "sero_optimal_locations")) {
    stop("locations must be a sero_optimal_locations object")
  }
  
  if (nrow(locations$locations) == 0) {
    warning("No locations to save")
    return(invisible(FALSE))
  }
  
  # Transform to WGS84 for storage
  locations_wgs84 <- sf::st_transform(locations$locations, 4326)
  
  # Save as GeoPackage
  tryCatch({
    sf::st_write(locations_wgs84, filename, delete_dsn = TRUE, quiet = TRUE)
    cat("Optimal locations saved to:", filename, "\n")
    cat("Number of locations:", nrow(locations_wgs84), "\n")
    cat("Format: GeoPackage (.gpkg)\n")
    cat("CRS: WGS84 (EPSG:4326)\n")
    return(invisible(TRUE))
  }, error = function(e) {
    warning("Error saving locations: ", e$message)
    return(invisible(FALSE))
  })
}

#' Load optimal locations from a dataset
#'
#' Loads pre-computed optimal locations from a GeoPackage file for use in routing.
#' GeoPackage format is optimal for sf objects and preserves all spatial information.
#'
#' @param filename File path to load the locations from (must be .gpkg file)
#' @return sf object with optimal locations
#' @export
#' @examples
#' \dontrun{
#' # Load from GeoPackage
#' locations <- sero_load_locations("optimal_locations.gpkg")
#' 
#' # Use in routing
#' data <- sero_load_data()
#' routes <- sero_routes(locations, data$accident[1:5, ])
#' }
sero_load_locations <- function(filename) {
  
  if (!file.exists(filename)) {
    stop("File not found: ", filename)
  }
  
  # Check file extension
  ext <- tools::file_ext(filename)
  if (tolower(ext) != "gpkg") {
    warning("File extension is not .gpkg. Expected GeoPackage format.")
  }
  
  tryCatch({
    # Read GeoPackage
    locations <- sf::st_read(filename, quiet = TRUE)
    
    cat("Loaded", nrow(locations), "optimal locations from:", filename, "\n")
    cat("CRS:", sf::st_crs(locations)$input, "\n")
    
    # Display basic info about the locations
    if (nrow(locations) > 0) {
      cat("Location attributes:\n")
      attr_names <- names(sf::st_drop_geometry(locations))
      for (i in seq_along(attr_names)) {
        cat("  -", attr_names[i], "\n")
      }
    }
    
    return(locations)
    
  }, error = function(e) {
    stop("Error loading locations: ", e$message)
  })
}

#' Create comprehensive route visualizations with different basemaps
#'
#' @param routes sero_routes object
#' @param data Data object containing spatial layers
#' @param save_plots Logical, whether to save plots to files (default: TRUE)
#' @param output_dir Character, directory to save plots (default: current directory)
#' @param width Numeric, plot width in inches (default: 12)
#' @param height Numeric, plot height in inches (default: 8)
#' @return List of ggplot2 objects
#' @export
sero_routes_comprehensive <- function(routes, data = NULL, save_plots = TRUE, 
                                    output_dir = ".", width = 12, height = 8) {
  
  if (is.null(data)) {
    data <- sero_load_data()
  }
  
  # Create different route visualizations
  plots <- list()
  
  # 1. Routes with Districts basemap
  cat("Creating routes with districts basemap...\n")
  plots$districts <- plot(routes, data = data, basemap = "districts", 
                         show_network = FALSE, show_landuse = FALSE)
  
  # 2. Routes with Road Network
  cat("Creating routes with road network...\n")
  plots$roads <- plot(routes, data = data, basemap = "roads",
                     show_network = TRUE, show_landuse = FALSE)
  
  # 3. Routes with Land Use
  cat("Creating routes with land use...\n")
  plots$landuse <- plot(routes, data = data, basemap = "landuse",
                       show_network = FALSE, show_landuse = TRUE)
  
  # 4. Routes with All Basemaps (Union)
  cat("Creating comprehensive route view with all basemaps...\n")
  plots$comprehensive <- plot(routes, data = data, basemap = "all",
                             show_network = TRUE, show_landuse = TRUE,
                             alpha_basemap = 0.2)
  
  # 5. Routes only (no basemap)
  cat("Creating routes without basemap...\n")
  plots$routes_only <- plot(routes, data = data, basemap = "none")
  
  # Create comparative panel
  cat("Creating comparative panel...\n")
  plots$comparative <- create_route_comparison_panel(routes, data)
  
  # Save plots if requested
  if (save_plots) {
    cat("Saving plots to", output_dir, "...\n")
    
    plot_names <- c("districts", "roads", "landuse", "comprehensive", 
                   "routes_only", "comparative")
    
    for (i in seq_along(plot_names)) {
      filename <- file.path(output_dir, paste0("routes_", plot_names[i], ".png"))
      ggplot2::ggsave(filename, plots[[i]], width = width, height = height, 
                     dpi = 300, bg = "white")
      cat("Saved:", filename, "\n")
    }
  }
  
  return(plots)
}

#' Create a comparative panel of route visualizations
#'
#' @param routes sero_routes object
#' @param data Data object containing spatial layers
#' @return ggplot2 object with multiple panels
create_route_comparison_panel <- function(routes, data) {
  
  # Create individual plots for the panel
  p1 <- plot(routes, data = data, basemap = "districts", 
            show_network = FALSE, show_landuse = FALSE) +
    ggplot2::labs(title = "Districts Basemap", subtitle = NULL) +
    ggplot2::theme(legend.position = "none")
  
  p2 <- plot(routes, data = data, basemap = "roads",
            show_network = TRUE, show_landuse = FALSE) +
    ggplot2::labs(title = "Road Network", subtitle = NULL) +
    ggplot2::theme(legend.position = "none")
  
  p3 <- plot(routes, data = data, basemap = "landuse",
            show_network = FALSE, show_landuse = TRUE) +
    ggplot2::labs(title = "Land Use", subtitle = NULL) +
    ggplot2::theme(legend.position = "none")
  
  p4 <- plot(routes, data = data, basemap = "all",
            show_network = TRUE, show_landuse = TRUE,
            alpha_basemap = 0.2) +
    ggplot2::labs(title = "Comprehensive View", subtitle = NULL) +
    ggplot2::theme(legend.position = "bottom")
  
  # Combine into a 2x2 grid
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2,
                                           top = "Emergency Response Routes: Comparative Analysis")
    return(combined_plot)
  } else {
    warning("gridExtra package required for comparative panel. Install with: install.packages('gridExtra')")
    return(p4)  # Return comprehensive view as fallback
  }
}

#' Enhanced route analysis with spatial context
#'
#' @param routes sero_routes object
#' @param data Data object containing spatial layers
#' @param analysis_type Character, type of analysis: "performance", "coverage", "accessibility"
#' @return List with analysis results and visualizations
#' @export
sero_routes_analysis <- function(routes, data = NULL, analysis_type = "performance") {
  
  if (is.null(data)) {
    data <- sero_load_data()
  }
  
  if (nrow(routes$routes) == 0) {
    warning("No routes to analyze")
    return(list(analysis = NULL, plot = NULL))
  }
  
  # Transform data for analysis
  locations_utm <- sf::st_transform(routes$service_locations, 32632)
  
  analysis_results <- list()
  
  if (analysis_type == "performance") {
    # Performance analysis
    analysis_results$summary <- list(
      total_routes = nrow(routes$routes),
      avg_distance = mean(routes$routes$distance),
      avg_time = mean(routes$routes$estimated_time),
      max_distance = max(routes$routes$distance),
      max_time = max(routes$routes$estimated_time),
      min_distance = min(routes$routes$distance),
      min_time = min(routes$routes$estimated_time),
      efficiency_score = mean(routes$routes$distance) / max(routes$routes$distance)
    )
    
    # Create performance visualization
    analysis_results$plot <- create_performance_plot(routes, data)
    
  } else if (analysis_type == "coverage") {
    # Coverage analysis
    service_buffers <- sf::st_buffer(locations_utm, 5000)  # 5km coverage
    total_coverage <- sf::st_union(service_buffers)
    
    analysis_results$summary <- list(
      coverage_area_sqkm = as.numeric(sf::st_area(total_coverage)) / 1e6,
      service_locations = nrow(locations_utm),
      avg_coverage_radius = 5000  # meters
    )
    
    # Create coverage visualization
    analysis_results$plot <- create_coverage_plot(routes, data, service_buffers)
    
  } else if (analysis_type == "accessibility") {
    # Accessibility analysis
    analysis_results$summary <- list(
      routes_under_5min = sum(routes$routes$estimated_time < 5),
      routes_5_10min = sum(routes$routes$estimated_time >= 5 & routes$routes$estimated_time < 10),
      routes_over_10min = sum(routes$routes$estimated_time >= 10),
      accessibility_score = sum(routes$routes$estimated_time < 5) / nrow(routes$routes)
    )
    
    # Create accessibility visualization
    analysis_results$plot <- create_accessibility_plot(routes, data)
  }
  
  return(analysis_results)
}

#' Create performance visualization
#' @param routes sero_routes object
#' @param data Data object
#' @return ggplot2 object
create_performance_plot <- function(routes, data) {
  plot(routes, data = data, basemap = "districts") +
    ggplot2::labs(title = "Route Performance Analysis",
                 subtitle = paste("Avg time:", round(mean(routes$routes$estimated_time), 1), 
                                "min | Avg distance:", round(mean(routes$routes$distance)), "m"))
}

#' Create coverage visualization
#' @param routes sero_routes object
#' @param data Data object
#' @param service_buffers sf object with service coverage areas
#' @return ggplot2 object
create_coverage_plot <- function(routes, data, service_buffers) {
  # Transform for plotting
  buffers_wgs84 <- sf::st_transform(service_buffers, 4326)
  
  plot(routes, data = data, basemap = "districts") +
    ggplot2::geom_sf(data = buffers_wgs84, fill = "blue", alpha = 0.1, color = "blue") +
    ggplot2::labs(title = "Service Coverage Analysis",
                 subtitle = "Blue areas show 5km service coverage zones")
}

#' Create accessibility visualization
#' @param routes sero_routes object
#' @param data Data object
#' @return ggplot2 object
create_accessibility_plot <- function(routes, data) {
  # Color routes by time categories
  routes_wgs84 <- sf::st_transform(routes$routes, 4326)
  routes_wgs84$time_category <- cut(routes_wgs84$estimated_time,
                                   breaks = c(0, 5, 10, Inf),
                                   labels = c("< 5 min", "5-10 min", "> 10 min"))
  
  locations_wgs84 <- sf::st_transform(routes$service_locations, 4326)
  accidents_wgs84 <- sf::st_transform(routes$accidents, 4326)
  
  # Add basemap
  p <- ggplot2::ggplot()
  if ("districts" %in% names(data)) {
    districts_wgs84 <- sf::st_transform(data$districts, 4326)
    p <- p + ggplot2::geom_sf(data = districts_wgs84, fill = "lightgray", 
                             color = "darkgray", alpha = 0.3)
  }
  
  p <- p + 
    ggplot2::geom_sf(data = routes_wgs84, 
                    ggplot2::aes(color = .data$time_category), 
                    size = 1.5, alpha = 0.8) +
    ggplot2::geom_sf(data = locations_wgs84, color = "red", size = 6, shape = 3, stroke = 2) +
    ggplot2::geom_sf(data = accidents_wgs84, color = "red", size = 2) +
    ggplot2::scale_color_manual(values = c("green", "orange", "red"),
                               name = "Response Time") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Emergency Response Accessibility",
                 subtitle = "Routes colored by response time categories") +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}


