## Fast Emergency Location Optimization
##
## Optimized functions for quick emergency service location finding

## Quick optimal location calculation (optimized for speed)
#' Find optimal emergency service locations
#'
#' This function finds optimal locations for emergency services using various optimization methods.
#' It can use fast algorithms, k-means clustering, or centroid-based approaches.
#'
#' @param data list containing spatial data layers
#' @param num_locations number of emergency bases to locate (default: 5)
#' @param method optimization method: "fast", "kmeans", "centroid"
#' @param risk_categories vector of accident severity categories to focus on
#' @param risk_category_names vector of accident severity category names (e.g., "fatal", "serious", "slight")
#' @param quick logical for ultra-fast mode (default: TRUE)
#' @param save_to_db logical whether to save results to database
#' @param db_path path for database file
#' @param kmeans_nstart number of random starts for k-means (default: 20)
#' @param runif_min minimum offset for random uniform spread (default: -0.01)
#' @param runif_max maximum offset for random uniform spread (default: 0.01)
#' @param seed random seed for reproducibility (default: 123)
#' @param ... additional arguments passed to underlying optimization functions
#' @return sf object with optimal emergency service locations
#' @export
sero_find_optimal_locations <- function(data, 
                                       num_locations = 5,
                                       method = "fast",
                                       risk_categories = c("fatal", "severe"),
                                       risk_category_names = NULL,
                                       quick = TRUE,
                                       save_to_db = FALSE,
                                       db_path = "optimal_locations.sqlite",
                                       kmeans_nstart = 20,
                                       runif_min = -0.01,
                                       runif_max = 0.01,
                                       seed = 123,
                                       ...) {
  # Validate inputs
  if (!is.list(data) || !all(c("accident", "districts") %in% names(data))) {
    stop("Data must contain 'accident' and 'districts' layers")
  }
  
  accidents <- data$accident
  districts <- data$districts
  
  # Convert character risk categories to numeric if needed
  if (is.character(risk_categories)) {
    name_map <- c("fatal" = 1, "severe" = 2, "slight" = 3)
    numeric_categories <- unname(name_map[risk_categories])
    if (any(is.na(numeric_categories))) {
      stop("Invalid risk_categories. Use 'fatal', 'severe', or 'slight'.")
    }
    risk_categories <- numeric_categories
  }
  
  # Handle deprecated risk_category_names parameter
  if (!is.null(risk_category_names)) {
    warning("Parameter 'risk_category_names' is deprecated. Use 'risk_categories' with descriptive names.")
    name_map <- c(fatal=1, serious=2, slight=3)
    risk_categories <- unname(name_map[risk_category_names])
    if (any(is.na(risk_categories))) {
      stop("Invalid risk_category_names. Use 'fatal', 'serious', or 'slight'.")
    }
  }
  
  # Filter accidents by risk categories if specified
  if ("UKATEGORIE" %in% names(accidents) && !is.null(risk_categories)) {
    accidents <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
    cat("Using", nrow(accidents), "high-risk accidents (categories", paste(risk_categories, collapse = ", "), ")\n")
  }
  
  # Choose optimization method
  optimal_locations <- switch(method,
    "fast" = find_locations_kmeans(accidents, districts, num_locations, kmeans_nstart = kmeans_nstart, runif_min = runif_min, runif_max = runif_max, seed = seed, ...),
    "kmeans" = find_locations_kmeans(accidents, districts, num_locations, kmeans_nstart = kmeans_nstart, runif_min = runif_min, runif_max = runif_max, seed = seed, ...),
    "centroid" = find_locations_centroid(accidents, districts, num_locations),
    find_locations_kmeans(accidents, districts, num_locations, kmeans_nstart = kmeans_nstart, runif_min = runif_min, runif_max = runif_max, seed = seed, ...)  # default
  )
  
  # Add performance metrics
  optimal_locations <- add_performance_metrics(optimal_locations, accidents)
  
  # Save to database if requested
  if (save_to_db) {
    sero_save_optimal_locations(optimal_locations, db_path)
    cat("Saved to database:", db_path, "\n")
  }
  
  cat("Found", nrow(optimal_locations), "optimal emergency service locations\n")
  
  return(optimal_locations)
}

## K-means based location finding (fast)
#' @keywords internal
find_locations_kmeans <- function(accidents, districts, num_locations, kmeans_nstart = 20, runif_min = -0.01, runif_max = 0.01, seed = 123, ...) {
  # Transform to WGS84 for consistent calculations
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  
  # Get accident coordinates
  accident_coords <- sf::st_coordinates(accidents_wgs84)
  
  # Perform k-means clustering for speed
  if (nrow(accident_coords) >= num_locations) {
    set.seed(seed)  # For reproducible results
    kmeans_result <- stats::kmeans(accident_coords, centers = num_locations, nstart = kmeans_nstart, ...)
    centers <- kmeans_result$centers
  } else {
    # If few accidents, use all as centers and add some spread
    centers <- accident_coords[seq_len(base::min(num_locations, nrow(accident_coords))), ]
    if (nrow(centers) < num_locations) {
      # Add some nearby points
      for (i in (nrow(centers) + 1):num_locations) {
        new_center <- centers[1, ] + stats::runif(2, runif_min, runif_max)
        centers <- rbind(centers, new_center)
      }
    }
  }
  
  # Create optimal locations sf object
  optimal_locs <- sf::st_as_sf(
    data.frame(
      location_id = paste0("BASE_", seq_len(nrow(centers))),
      lon = centers[, 1],
      lat = centers[, 2],
      optimization_method = "k-means"
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # Add district information
  optimal_locs <- sf::st_join(optimal_locs, districts_wgs84)
  
  return(optimal_locs)
}

#' Centroid based location finding (very fast)
#' @keywords internal
find_locations_centroid <- function(accidents, districts, num_locations) {
  
  # Transform to WGS84
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  
  # Calculate overall centroid
  accident_coords <- sf::st_coordinates(accidents_wgs84)
  center_x <- mean(accident_coords[, 1])
  center_y <- mean(accident_coords[, 2])
  
  # Create multiple locations around centroid
  angles <- seq(0, 2*pi, length.out = num_locations + 1)[1:num_locations]
  radius <- 0.02  # Approximately 2km spread
  
  locs <- data.frame(
    location_id = paste0("BASE_", 1:num_locations),
    lon = center_x + radius * cos(angles),
    lat = center_y + radius * sin(angles),
    optimization_method = "centroid"
  )
  
  # Make sure main location is at centroid
  locs$lon[1] <- center_x
  locs$lat[1] <- center_y
  
  optimal_locs <- sf::st_as_sf(locs, coords = c("lon", "lat"), crs = 4326)
  
  # Add district information
  optimal_locs <- sf::st_join(optimal_locs, districts_wgs84)
  
  return(optimal_locs)
}

#' Add performance metrics to optimal locations
#' @keywords internal
add_performance_metrics <- function(optimal_locations, accidents) {
  
  # Transform to same CRS
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  
  # Calculate accident counts within different radii
  optimal_locations$accident_count_500m <- 0
  optimal_locations$accident_count_1000m <- 0
  optimal_locations$accessibility_score <- 0
  
  for (i in seq_len(nrow(optimal_locations))) {
    # Calculate distances to all accidents
    distances <- sf::st_distance(optimal_locations[i, ], accidents_wgs84)
    distances_m <- as.numeric(distances)
    
    # Count accidents within radii
    optimal_locations$accident_count_500m[i] <- sum(distances_m <= 500)
    optimal_locations$accident_count_1000m[i] <- sum(distances_m <= 1000)
    
    # Simple accessibility score (higher = better)
    optimal_locations$accessibility_score[i] <- round(
      optimal_locations$accident_count_1000m[i] / base::max(distances_m) * 1000, 3
    )
  }
  
  return(optimal_locations)
}

#' Quick visualization of optimal locations
#'
#' @param optimal_locations sf object with emergency service locations
#' @param districts sf object with district boundaries
#' @param accidents sf object with accident data (optional)
#' @param roads sf object with road network (optional)
#' @return ggplot2 object
#' @export
sero_plot_optimal_quick <- function(optimal_locations, 
                                   districts, 
                                   accidents = NULL, 
                                   roads = NULL) {
  
  # Transform all to WGS84
  optimal_wgs84 <- sf::st_transform(optimal_locations, 4326)
  districts_wgs84 <- sf::st_transform(districts, 4326)
  
  # Create base map
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_wgs84, 
                    fill = "lightgray", 
                    color = "black", 
                    alpha = 0.3) +
    ggplot2::geom_sf(data = optimal_wgs84, 
                    color = "red", 
                    size = 6, 
                    shape = 3, 
                    stroke = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Optimal Emergency Service Locations",
      subtitle = paste("Found", nrow(optimal_wgs84), "strategic locations"),
      caption = "Red plus signs = Emergency ambulance stations"
    )
  
  # Add accidents if provided
  if (!is.null(accidents)) {
    accidents_wgs84 <- sf::st_transform(accidents, 4326)
    p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                             color = "orange", 
                             size = 0.3, 
                             alpha = 0.6)
  }
  
  # Add roads if provided (sample for performance)
  if (!is.null(roads)) {
    roads_wgs84 <- sf::st_transform(roads, 4326)
    # Sample roads for performance
    if (nrow(roads_wgs84) > 1000) {
      roads_sample <- roads_wgs84[sample(nrow(roads_wgs84), 1000), ]
    } else {
      roads_sample <- roads_wgs84
    }
    p <- p + ggplot2::geom_sf(data = roads_sample, 
                             color = "gray", 
                             size = 0.2, 
                             alpha = 0.4)
  }
  
  return(p)
}

#' Plot accidents with severity filtering (static version for speed)
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

#' Save Optimal Locations to Database
#'
#' Saves optimal emergency service locations to a database or GeoPackage file.
#'
#' @param locations sf object containing optimal locations
#' @param db_path character, path to database file
#' @return NULL (saves to file)
#' @export
#' @examples
#' \dontrun{
#' optimal_locs <- sero_find_optimal_locations(accident_data)
#' sero_save_optimal_locations(optimal_locs, "optimal_locations.gpkg")
#' }
sero_save_optimal_locations <- function(locations, db_path) {
  # Input validation
  if (!inherits(locations, "sf")) {
    stop("locations must be an sf object")
  }
  
  if (missing(db_path) || is.null(db_path)) {
    stop("db_path must be provided")
  }
  
  # Create directory if it doesn't exist
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  
  # Determine file format and save
  if (grepl("\\.gpkg$", db_path, ignore.case = TRUE)) {
    # Save as GeoPackage
    sf::st_write(locations, db_path, layer = "optimal_locations", 
                 append = FALSE, quiet = TRUE)
  } else if (grepl("\\.sqlite$", db_path, ignore.case = TRUE)) {
    # Save as SQLite with spatial extension
    sf::st_write(locations, db_path, layer = "optimal_locations", 
                 append = FALSE, quiet = TRUE)
  } else if (grepl("\\.shp$", db_path, ignore.case = TRUE)) {
    # Save as Shapefile
    sf::st_write(locations, db_path, append = FALSE, quiet = TRUE)
  } else {
    # Default to GeoPackage
    sf::st_write(locations, db_path, layer = "optimal_locations", 
                 append = FALSE, quiet = TRUE)
  }
  
  invisible(NULL)
}

#' Enhanced workflow function for users
#'
#' @param interactive logical whether to launch interactive mode
#' @param quick logical whether to use fast calculations
#' @param num_locations number of emergency bases to find
#' @param risk_categories vector of accident severity categories to focus on
#' @param risk_category_names vector of accident severity category names (e.g., "fatal", "serious", "slight")
#' @param ... additional arguments passed to underlying functions
#' @return list with results and visualizations
#' @export
sero_emergency_workflow <- function(interactive = FALSE, quick = TRUE, num_locations = 5, risk_categories = c("fatal", "severe"), risk_category_names = NULL, ...) {
  cat("SERO: Emergency Response Optimization Workflow\n")
  cat(strrep("=", 50), "\n")
  # Step 1: Load data
  cat("Step 1: Loading spatial data...\n")
  data <- sero_load_data()
  cat("Loaded Munster dataset with", length(data), "layers\n")
  
  # Allow risk categories by name
  if (!is.null(risk_category_names)) {
    name_map <- c(fatal=1, serious=2, slight=3)
    risk_categories <- unname(name_map[risk_category_names])
    if (any(is.na(risk_categories))) {
      stop("Invalid risk_category_names. Use 'fatal', 'serious', or 'slight'.")
    }
  }
  
  # Convert character risk categories to numeric if needed
  if (is.character(risk_categories)) {
    name_map <- c("fatal" = 1, "severe" = 2, "slight" = 3)
    numeric_categories <- unname(name_map[risk_categories])
    if (any(is.na(numeric_categories))) {
      stop("Invalid risk_categories. Use 'fatal', 'severe', or 'slight'.")
    }
    risk_categories <- numeric_categories
  }
  
  # Step 2: Accident visualization
  cat("\nStep 2: Creating accident visualization...\n")
  accident_map <- sero_plot_accidents(
    accidents = data$accident,
    districts = data$districts,
    severity_levels = risk_categories,
    use_osm_basemap = FALSE  # Use static for speed
  )
  cat("Accident map created\n")
  
  # Step 3: Find optimal locations
  cat("\nStep 3: Finding optimal emergency locations...\n")
  optimal_locations <- sero_find_optimal_locations(
    data = data,
    num_locations = num_locations,
    method = ifelse(quick, "fast", "kmeans"),
    quick = quick,
    risk_categories = risk_categories,
    risk_category_names = risk_category_names,
    ...
  )
  
  # Step 4: Calculate routes to optimal locations
  cat("\nStep 4: Calculating routes to accident scenes...\n")
  routes <- sero_routes(
    locations = optimal_locations,
    accidents = data$accident,
    max_routes = 5,
    risk_categories = risk_categories,
    data = data
  )
  cat("Routes calculated\n")
  
  # Step 5: Visualize optimal locations and routes
  cat("\nStep 5: Creating visualizations...\n")
  optimal_map <- sero_plot_optimal_quick(
    optimal_locations = optimal_locations,
    districts = data$districts,
    accidents = data$accident
  )
  cat("Optimal location map created\n")
  
  # Create route visualization
  route_map <- plot(routes)
  cat("Route map created\n")
  
  # Step 6: Performance summary
  cat("\nStep 6: Performance Summary\n")
  cat("============================\n")
  cat("Emergency bases found:", nrow(optimal_locations), "\n")
  if ("accessibility_score" %in% names(optimal_locations)) {
    cat("Average accessibility score:", round(mean(optimal_locations$accessibility_score), 3), "\n")
    cat("Best accessibility score:", round(base::max(optimal_locations$accessibility_score), 3), "\n")
  }
  if ("accident_count_500m" %in% names(optimal_locations)) {
    cat("Average accidents covered (500m):", round(mean(optimal_locations$accident_count_500m), 1), "\n")
    cat("Maximum accidents covered (500m):", base::max(optimal_locations$accident_count_500m), "\n")
  }
  
  # Route performance summary
  if (nrow(routes$routes) > 0) {
    cat("Routes calculated:", nrow(routes$routes), "\n")
    cat("Average response time:", round(mean(routes$routes$estimated_time), 1), "minutes\n")
    cat("Maximum response time:", round(max(routes$routes$estimated_time), 1), "minutes\n")
  }
  
  # Display maps
  print(accident_map)
  print(optimal_map)
  print(route_map)
  
  # Package simplified - interactive features removed
  # Returning results without interactive mode
  results <- list(
    data = data,
    optimal_locations = optimal_locations,
    routes = routes,
    accident_map = accident_map,
    optimal_map = optimal_map,
    route_map = route_map,
    summary = list(
      num_locations = nrow(optimal_locations),
      avg_accessibility = ifelse("accessibility_score" %in% names(optimal_locations),
                                mean(optimal_locations$accessibility_score), NA),
      total_accidents = nrow(data$accident),
      num_routes = nrow(routes$routes),
      avg_response_time = ifelse(nrow(routes$routes) > 0, 
                                mean(routes$routes$estimated_time), NA)
    )
  )
  
  cat("\nWorkflow completed successfully!\n")
  return(invisible(results))
}
