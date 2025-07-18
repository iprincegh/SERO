# Enhanced Location Optimization using Stars
# This file contains optimized location finding functions using stars library

# Declare global variables for CRAN compliance
utils::globalVariables(c("x", "y", "density", "risk_level", "hotspot_value", "location_id"))

#' Enhanced Optimal Location Finding using Stars
#'
#' Finds optimal emergency service locations using raster-based analysis
#' with stars library for improved performance and scalability.
#'
#' @param accidents sf object containing accident data
#' @param existing_stations sf object containing existing emergency stations
#' @param n_locations integer, number of new locations to find
#' @param method character, optimization method ("stars_kmeans", "stars_coverage", "stars_hybrid")
#' @param risk_categories numeric vector of risk categories to consider
#' @param response_time_target numeric, target response time in minutes
#' @param bandwidth numeric, bandwidth for kernel density estimation
#' @param resolution numeric, raster resolution in meters
#' @param weight_existing numeric, weight for existing stations (0-1)
#' @param coverage_radius numeric, coverage radius in meters
#' @param data list, SERO data object containing road network and other layers
#' @return sf object with optimal locations and performance metrics
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' optimal_locs <- sero_find_optimal_locations_stars(
#'   data$accident, 
#'   data$existing_stations, 
#'   n_locations = 3,
#'   method = "stars_hybrid"
#' )
#' }
sero_find_optimal_locations_stars <- function(accidents,
                                             existing_stations = NULL,
                                             n_locations = 5,
                                             method = "stars_hybrid",
                                             risk_categories = c(1, 2),
                                             response_time_target = 8,
                                             bandwidth = 1000,
                                             resolution = 100,
                                             weight_existing = 0.3,
                                             coverage_radius = 5000,
                                             data = NULL) {
  
  # Input validation
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  if (!is.null(existing_stations) && !inherits(existing_stations, "sf")) {
    stop("existing_stations must be an sf object")
  }
  
  if (!method %in% c("stars_kmeans", "stars_coverage", "stars_hybrid")) {
    stop("method must be one of: 'stars_kmeans', 'stars_coverage', 'stars_hybrid'")
  }
  
  # Create density surface using stars
  message("Creating density surface...")
  density_surface <- sero_heatmap_stars(
    accidents = accidents,
    risk_categories = risk_categories,
    bandwidth = bandwidth,
    resolution = resolution,
    data = data
  )
  
  # Apply optimization method
  switch(method,
    "stars_kmeans" = {
      locations <- optimize_locations_stars_kmeans(
        density_surface, n_locations, existing_stations, weight_existing
      )
    },
    "stars_coverage" = {
      locations <- optimize_locations_stars_coverage(
        density_surface, n_locations, existing_stations, coverage_radius, data
      )
    },
    "stars_hybrid" = {
      locations <- optimize_locations_stars_hybrid(
        density_surface, n_locations, existing_stations, 
        coverage_radius, weight_existing, data
      )
    }
  )
  
  # Calculate performance metrics
  message("Calculating performance metrics...")
  performance_metrics <- calculate_performance_metrics_stars(
    locations, accidents, existing_stations, response_time_target, data
  )
  
  # Add performance metrics to locations
  locations$expected_response_time <- performance_metrics$response_times
  locations$coverage_area <- performance_metrics$coverage_areas
  locations$accidents_served <- performance_metrics$accidents_served
  
  # Add metadata
  attr(locations, "method") <- method
  attr(locations, "density_surface") <- density_surface
  attr(locations, "performance_summary") <- performance_metrics$summary
  attr(locations, "parameters") <- list(
    n_locations = n_locations,
    risk_categories = risk_categories,
    response_time_target = response_time_target,
    bandwidth = bandwidth,
    resolution = resolution,
    weight_existing = weight_existing,
    coverage_radius = coverage_radius
  )
  
  class(locations) <- c("sero_optimal_locations_stars", class(locations))
  
  return(locations)
}

#' Stars-based K-means Optimization
#'
#' Optimizes locations using k-means clustering on density surface.
#'
#' @param density_surface stars object with density surface
#' @param n_locations integer, number of locations
#' @param existing_stations sf object, existing stations
#' @param weight_existing numeric, weight for existing stations
#' @return sf object with optimal locations
#' @keywords internal
optimize_locations_stars_kmeans <- function(density_surface, n_locations, 
                                           existing_stations, weight_existing) {
  
  # Convert density surface to weighted points
  density_df <- as.data.frame(density_surface, xy = TRUE)
  density_df <- density_df[!is.na(density_df$density) & density_df$density > 0, ]
  
  if (nrow(density_df) == 0) {
    stop("No valid density values found")
  }
  
  # Create weighted sample based on density
  n_sample <- min(10000, nrow(density_df))  # Limit sample size for performance
  sample_weights <- density_df$density / sum(density_df$density)
  sample_indices <- sample(nrow(density_df), n_sample, 
                          replace = TRUE, prob = sample_weights)
  sample_points <- density_df[sample_indices, c("x", "y")]
  
  # Add existing stations to the mix if provided
  if (!is.null(existing_stations) && nrow(existing_stations) > 0) {
    existing_coords <- sf::st_coordinates(existing_stations)
    existing_df <- data.frame(x = existing_coords[, 1], y = existing_coords[, 2])
    
    # Weight existing stations
    n_existing_repeat <- round(nrow(sample_points) * weight_existing / n_locations)
    existing_repeated <- existing_df[rep(seq_len(nrow(existing_df)), 
                                        each = n_existing_repeat), ]
    
    sample_points <- rbind(sample_points, existing_repeated)
  }
  
  # Perform k-means clustering
  set.seed(42)  # For reproducibility
  kmeans_result <- stats::kmeans(sample_points, centers = n_locations, 
                                nstart = 25, iter.max = 100)
  
  # Extract cluster centers
  centers <- kmeans_result$centers
  
  # Convert to sf object
  locations <- sf::st_as_sf(
    data.frame(
      location_id = paste0("LOC_", sprintf("%02d", 1:n_locations)),
      x = centers[, 1],
      y = centers[, 2],
      method = "stars_kmeans"
    ),
    coords = c("x", "y"),
    crs = sf::st_crs(density_surface)
  )
  
  return(locations)
}

#' Stars-based Coverage Optimization
#'
#' Optimizes locations using coverage analysis on density surface.
#'
#' @param density_surface stars object with density surface
#' @param n_locations integer, number of locations
#' @param existing_stations sf object, existing stations
#' @param coverage_radius numeric, coverage radius in meters
#' @param data list, SERO data object
#' @return sf object with optimal locations
#' @keywords internal
optimize_locations_stars_coverage <- function(density_surface, n_locations, 
                                             existing_stations, coverage_radius, data) {
  
  # Convert density surface to candidate points
  density_df <- as.data.frame(density_surface, xy = TRUE)
  density_df <- density_df[!is.na(density_df$density) & density_df$density > 0, ]
  
  if (nrow(density_df) == 0) {
    stop("No valid density values found")
  }
  
  # Create candidate locations (sample high-density areas)
  high_density_threshold <- quantile(density_df$density, 0.8, na.rm = TRUE)
  candidates <- density_df[density_df$density >= high_density_threshold, ]
  
  # Limit candidates for performance
  if (nrow(candidates) > 1000) {
    candidates <- candidates[sample(nrow(candidates), 1000), ]
  }
  
  # Convert to sf
  candidates_sf <- sf::st_as_sf(
    candidates,
    coords = c("x", "y"),
    crs = sf::st_crs(density_surface)
  )
  
  # Initialize coverage tracking
  covered_areas <- matrix(FALSE, nrow = nrow(density_df), ncol = n_locations)
  selected_locations <- vector("list", n_locations)
  
  # Greedy coverage algorithm
  for (i in 1:n_locations) {
    message(paste("Selecting location", i, "of", n_locations))
    
    best_score <- -Inf
    best_location <- NULL
    
    for (j in seq_len(nrow(candidates_sf))) {
      # Calculate coverage for this candidate
      candidate_geom <- sf::st_geometry(candidates_sf[j, ])
      
      # Create coverage buffer
      coverage_buffer <- sf::st_buffer(candidate_geom, coverage_radius)
      
      # Find covered density points
      density_points_sf <- sf::st_as_sf(
        density_df,
        coords = c("x", "y"),
        crs = sf::st_crs(density_surface)
      )
      
      covered_mask <- sf::st_intersects(density_points_sf, coverage_buffer, sparse = FALSE)
      
      # Calculate uncovered density (avoid double counting)
      uncovered_mask <- covered_mask & !apply(covered_areas[, 1:(i-1), drop = FALSE], 1, any)
      uncovered_density <- sum(density_df$density[uncovered_mask])
      
      # Update best if this is better
      if (uncovered_density > best_score) {
        best_score <- uncovered_density
        best_location <- j
      }
    }
    
    # Store selected location
    selected_locations[[i]] <- candidates_sf[best_location, ]
    
    # Update covered areas
    candidate_geom <- sf::st_geometry(candidates_sf[best_location, ])
    coverage_buffer <- sf::st_buffer(candidate_geom, coverage_radius)
    density_points_sf <- sf::st_as_sf(
      density_df,
      coords = c("x", "y"),
      crs = sf::st_crs(density_surface)
    )
    covered_areas[, i] <- sf::st_intersects(density_points_sf, coverage_buffer, sparse = FALSE)
  }
  
  # Combine selected locations
  locations <- do.call(rbind, selected_locations)
  locations$location_id <- paste0("LOC_", sprintf("%02d", 1:n_locations))
  locations$method <- "stars_coverage"
  locations$coverage_score <- sapply(1:n_locations, function(i) {
    sum(density_df$density[covered_areas[, i]])
  })
  
  return(locations)
}

#' Stars-based Hybrid Optimization
#'
#' Optimizes locations using hybrid approach combining k-means and coverage.
#'
#' @param density_surface stars object with density surface
#' @param n_locations integer, number of locations
#' @param existing_stations sf object, existing stations
#' @param coverage_radius numeric, coverage radius in meters
#' @param weight_existing numeric, weight for existing stations
#' @param data list, SERO data object
#' @return sf object with optimal locations
#' @keywords internal
optimize_locations_stars_hybrid <- function(density_surface, n_locations, 
                                           existing_stations, coverage_radius, 
                                           weight_existing, data) {
  
  # Step 1: Get initial locations using k-means
  initial_locations <- optimize_locations_stars_kmeans(
    density_surface, n_locations, existing_stations, weight_existing
  )
  
  # Step 2: Refine using coverage optimization
  # Create candidate points around initial locations
  candidates <- vector("list", n_locations)
  
  for (i in 1:n_locations) {
    center <- sf::st_geometry(initial_locations[i, ])
    
    # Create candidate grid around center
    buffer_area <- sf::st_buffer(center, coverage_radius * 0.5)
    bbox <- sf::st_bbox(buffer_area)
    
    # Generate grid of candidates
    x_seq <- seq(bbox[1], bbox[3], length.out = 10)
    y_seq <- seq(bbox[2], bbox[4], length.out = 10)
    grid_points <- expand.grid(x = x_seq, y = y_seq)
    
    grid_sf <- sf::st_as_sf(
      grid_points,
      coords = c("x", "y"),
      crs = sf::st_crs(density_surface)
    )
    
    # Filter candidates within buffer
    candidates[[i]] <- grid_sf[sf::st_intersects(grid_sf, buffer_area, sparse = FALSE), ]
  }
  
  # Step 3: Local optimization for each location
  density_df <- as.data.frame(density_surface, xy = TRUE)
  density_df <- density_df[!is.na(density_df$density) & density_df$density > 0, ]
  
  optimized_locations <- vector("list", n_locations)
  
  for (i in 1:n_locations) {
    best_score <- -Inf
    best_location <- initial_locations[i, ]
    
    for (j in seq_len(nrow(candidates[[i]]))) {
      candidate_geom <- sf::st_geometry(candidates[[i]][j, ])
      
      # Calculate coverage score
      coverage_buffer <- sf::st_buffer(candidate_geom, coverage_radius)
      density_points_sf <- sf::st_as_sf(
        density_df,
        coords = c("x", "y"),
        crs = sf::st_crs(density_surface)
      )
      
      covered_mask <- sf::st_intersects(density_points_sf, coverage_buffer, sparse = FALSE)
      coverage_score <- sum(density_df$density[covered_mask])
      
      if (coverage_score > best_score) {
        best_score <- coverage_score
        best_location <- candidates[[i]][j, ]
        best_location$location_id <- paste0("LOC_", sprintf("%02d", i))
        best_location$method <- "stars_hybrid"
        best_location$coverage_score <- coverage_score
      }
    }
    
    optimized_locations[[i]] <- best_location
  }
  
  # Combine optimized locations
  locations <- do.call(rbind, optimized_locations)
  
  return(locations)
}

#' Calculate Performance Metrics for Stars-based Optimization
#'
#' Calculates performance metrics for optimized locations.
#'
#' @param locations sf object with optimal locations
#' @param accidents sf object with accident data
#' @param existing_stations sf object with existing stations
#' @param response_time_target numeric, target response time in minutes
#' @param data list, SERO data object
#' @return list with performance metrics
#' @keywords internal
calculate_performance_metrics_stars <- function(locations, accidents, existing_stations, 
                                               response_time_target, data) {
  
  # Combine new and existing locations
  all_stations <- locations
  if (!is.null(existing_stations) && nrow(existing_stations) > 0) {
    existing_prepared <- existing_stations
    existing_prepared$location_id <- paste0("EXISTING_", seq_len(nrow(existing_stations)))
    existing_prepared$method <- "existing"
    existing_prepared$coverage_score <- NA
    
    all_stations <- rbind(all_stations, existing_prepared[names(locations)])
  }
  
  # Calculate response times (simplified - straight-line distance)
  # In practice, would use routing network
  response_times <- numeric(nrow(locations))
  coverage_areas <- numeric(nrow(locations))
  accidents_served <- numeric(nrow(locations))
  
  for (i in seq_len(nrow(locations))) {
    # Calculate distances to all accidents
    distances <- sf::st_distance(locations[i, ], accidents)
    
    # Convert to response time (assuming average speed of 50 km/h)
    times <- as.numeric(distances) / 1000 * 60 / 50  # minutes
    
    # Calculate metrics
    response_times[i] <- mean(times)
    coverage_areas[i] <- sum(times <= response_time_target)
    accidents_served[i] <- length(times[times <= response_time_target])
  }
  
  # Overall performance summary
  all_times <- numeric(nrow(accidents))
  for (i in seq_len(nrow(accidents))) {
    # Find closest station
    distances <- sf::st_distance(accidents[i, ], all_stations)
    min_distance <- min(distances)
    all_times[i] <- as.numeric(min_distance) / 1000 * 60 / 50  # minutes
  }
  
  summary_stats <- list(
    mean_response_time = mean(all_times),
    median_response_time = median(all_times),
    coverage_rate = mean(all_times <= response_time_target),
    accidents_within_target = sum(all_times <= response_time_target),
    total_accidents = length(all_times)
  )
  
  return(list(
    response_times = response_times,
    coverage_areas = coverage_areas,
    accidents_served = accidents_served,
    summary = summary_stats
  ))
}

#' Plot Method for Stars-based Optimal Locations
#'
#' Plots optimal locations with density surface and performance metrics.
#'
#' @param x sero_optimal_locations_stars object
#' @param ... additional arguments passed to ggplot
#' @return ggplot2 object
#' @export
plot.sero_optimal_locations_stars <- function(x, ...) {
  # Get density surface
  density_surface <- attr(x, "density_surface")
  
  if (is.null(density_surface)) {
    stop("No density surface found in object")
  }
  
  # Convert density surface to data frame
  density_df <- as.data.frame(density_surface, xy = TRUE)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = density_df, 
                        ggplot2::aes(x = x, y = y, fill = density)) +
    ggplot2::scale_fill_viridis_c(name = "Density", na.value = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Optimal Emergency Service Locations (Stars Enhanced)")
  
  # Add optimal locations
  p <- p + ggplot2::geom_sf(data = x, 
                           color = "red", 
                           size = 4, 
                           shape = 17,
                           inherit.aes = FALSE) +
    ggplot2::geom_sf_text(data = x, 
                         ggplot2::aes(label = location_id),
                         color = "white",
                         size = 3,
                         inherit.aes = FALSE)
  
  return(p)
}

#' Summary Method for Stars-based Optimal Locations
#'
#' Provides summary statistics for optimal locations.
#'
#' @param object sero_optimal_locations_stars object
#' @param ... additional arguments
#' @return character vector with summary
#' @export
summary.sero_optimal_locations_stars <- function(object, ...) {
  params <- attr(object, "parameters")
  performance <- attr(object, "performance_summary")
  
  cat("SERO Optimal Locations (Stars Enhanced)\n")
  cat("=====================================\n\n")
  
  cat("Parameters:\n")
  cat(paste("- Method:", attr(object, "method"), "\n"))
  cat(paste("- Number of locations:", params$n_locations, "\n"))
  cat(paste("- Risk categories:", paste(params$risk_categories, collapse = ", "), "\n"))
  cat(paste("- Response time target:", params$response_time_target, "minutes\n"))
  cat(paste("- Bandwidth:", params$bandwidth, "meters\n"))
  cat(paste("- Resolution:", params$resolution, "meters\n\n"))
  
  cat("Performance Summary:\n")
  cat(paste("- Mean response time:", round(performance$mean_response_time, 2), "minutes\n"))
  cat(paste("- Median response time:", round(performance$median_response_time, 2), "minutes\n"))
  cat(paste("- Coverage rate:", round(performance$coverage_rate * 100, 1), "%\n"))
  cat(paste("- Accidents within target:", performance$accidents_within_target, 
            "of", performance$total_accidents, "\n\n"))
  
  cat("Location Details:\n")
  for (i in seq_len(nrow(object))) {
    cat(paste("- ", object$location_id[i], 
              ": Expected response time =", round(object$expected_response_time[i], 2), "min,",
              "Accidents served =", object$accidents_served[i], "\n"))
  }
  
  invisible(object)
}
