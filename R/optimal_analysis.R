#' Enhanced Optimal Location Analysis for SERO Package
#'
#' Advanced optimal location finding with multiple optimization algorithms

#' Find optimal emergency service locations using multiple algorithms
#'
#' @param data List containing spatial data layers (must include 'accident' and 'districts')
#' @param num_locations Integer, number of optimal locations to find (default: 3)
#' @param method Character, optimization method: "kmeans", "grid", "density", or "hybrid"
#' @param save_locations Logical, whether to save locations as separate data layer
#' @return List with optimal locations, analysis results, and visualization
#' @importFrom sf st_transform st_coordinates st_as_sf st_crs
#' @importFrom stats kmeans
#' @export
sero_optimal_locations <- function(data, 
                                  num_locations = 3,
                                  method = "hybrid",
                                  save_locations = TRUE) {
  
  # Validate input data
  if (!"accident" %in% names(data) || !"districts" %in% names(data)) {
    stop("Data must contain 'accident' and 'districts' layers")
  }
  
  # Transform to consistent CRS
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  districts_wgs84 <- sf::st_transform(data$districts, 4326)
  
  # Get accident coordinates
  accident_coords <- sf::st_coordinates(accidents_wgs84)
  
  # Find optimal locations based on method
  optimal_coords <- switch(method,
    "kmeans" = find_kmeans_locations(accident_coords, num_locations),
    "grid" = find_grid_locations(accident_coords, districts_wgs84, num_locations),
    "density" = find_density_locations(accident_coords, districts_wgs84, num_locations),
    "hybrid" = find_hybrid_locations(accident_coords, districts_wgs84, num_locations)
  )
  
  # Create sf object for optimal locations
  optimal_locations_sf <- sf::st_as_sf(
    data.frame(
      id = paste0("OPT_", seq_len(nrow(optimal_coords))),
      lon = optimal_coords[,1],
      lat = optimal_coords[,2],
      method = method
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # Calculate performance metrics
  performance <- calculate_location_performance(accident_coords, optimal_coords)
  
  # Create visualization
  plot_optimal <- create_optimal_visualization(accidents_wgs84, districts_wgs84, 
                                             optimal_locations_sf, data, method)
  
  # Save to data if requested
  if (save_locations) {
    data$optimal_locations <- optimal_locations_sf
  }
  
  return(list(
    locations = optimal_locations_sf,
    coordinates = optimal_coords,
    performance = performance,
    plot = plot_optimal,
    method = method,
    data_updated = if(save_locations) data else NULL
  ))
}

#' K-means clustering for optimal locations
#' @keywords internal
find_kmeans_locations <- function(coords, k) {
  kmeans_result <- stats::kmeans(coords, centers = k, nstart = 20)
  return(kmeans_result$centers)
}

#' Grid-based optimal location finding
#' @keywords internal
find_grid_locations <- function(coords, districts, k) {
  bbox <- sf::st_bbox(districts)
  
  # Create grid points within districts
  grid_x <- seq(bbox[1], bbox[3], length.out = sqrt(k*4))
  grid_y <- seq(bbox[2], bbox[4], length.out = sqrt(k*4))
  grid_points <- expand.grid(x = grid_x, y = grid_y)
  
  # Calculate coverage for each grid point
  coverage_scores <- apply(grid_points, 1, function(point) {
    distances <- sqrt((coords[,1] - point[1])^2 + (coords[,2] - point[2])^2)
    sum(1/pmax(distances, 0.001))  # Inverse distance weighting
  })
  
  # Select top k grid points
  top_indices <- order(coverage_scores, decreasing = TRUE)[1:k]
  return(as.matrix(grid_points[top_indices, ]))
}

#' Density-based optimal location finding
#' @keywords internal
find_density_locations <- function(coords, districts, k) {
  # Use 2D kernel density estimation
  bbox <- sf::st_bbox(districts)
  
  # Create density surface
  x_seq <- seq(bbox[1], bbox[3], length.out = 50)
  y_seq <- seq(bbox[2], bbox[4], length.out = 50)
  
  density_grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Calculate density at each grid point
  density_scores <- apply(density_grid, 1, function(point) {
    distances <- sqrt((coords[,1] - point[1])^2 + (coords[,2] - point[2])^2)
    sum(exp(-distances^2 / (2 * 0.01^2)))  # Gaussian kernel
  })
  
  # Select top k density locations
  top_indices <- order(density_scores, decreasing = TRUE)[1:k]
  return(as.matrix(density_grid[top_indices, ]))
}

#' Hybrid optimization approach
#' @keywords internal
find_hybrid_locations <- function(coords, districts, k) {
  # Combine k-means and density approaches
  kmeans_locs <- find_kmeans_locations(coords, max(1, k-1))
  density_locs <- find_density_locations(coords, districts, 1)
  
  if (k == 1) return(density_locs)
  
  return(rbind(kmeans_locs, density_locs))
}

#' Calculate performance metrics for optimal locations
#' @keywords internal
calculate_location_performance <- function(accident_coords, optimal_coords) {
  
  # Calculate distances from each accident to nearest optimal location
  min_distances <- apply(accident_coords, 1, function(acc) {
    distances <- sqrt((optimal_coords[,1] - acc[1])^2 + (optimal_coords[,2] - acc[2])^2)
    min(distances)
  })
  
  # Convert to meters (approximate)
  min_distances_m <- min_distances * 111320
  
  return(list(
    avg_distance_m = mean(min_distances_m),
    max_distance_m = max(min_distances_m),
    min_distance_m = min(min_distances_m),
    median_distance_m = median(min_distances_m),
    coverage_90_percent = quantile(min_distances_m, 0.9),
    total_accidents_covered = length(min_distances_m)
  ))
}

#' Create visualization for optimal locations
#' @keywords internal
create_optimal_visualization <- function(accidents, districts, optimal_locations, data, method) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts, 
                    fill = "lightblue", 
                    color = "darkblue", 
                    alpha = 0.3,
                    size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Optimal Emergency Service Locations -", method, "method"),
                 subtitle = paste("Accidents:", nrow(accidents), "| Optimal locations:", nrow(optimal_locations)),
                 x = "Longitude", y = "Latitude")
  
  # Add landuse if available
  if ("landuse" %in% names(data)) {
    landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
    p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                             fill = "lightgreen", 
                             color = "darkgreen", 
                             alpha = 0.1, 
                             size = 0.1)
  }
  
  # Add accidents
  p <- p + ggplot2::geom_sf(data = accidents, 
                           color = "red", 
                           size = 0.4, 
                           alpha = 0.6)
  
  # Add optimal locations
  p <- p + ggplot2::geom_sf(data = optimal_locations, 
                           color = "blue", 
                           size = 4, 
                           shape = 17) +  # Triangle shape
    ggplot2::geom_sf_text(data = optimal_locations, 
                         ggplot2::aes(label = .data$id), 
                         nudge_y = 0.005, 
                         size = 3, 
                         fontface = "bold")
  
  # Add district labels
  if ("name" %in% names(districts)) {
    district_centroids <- sf::st_centroid(districts)
    centroid_coords <- sf::st_coordinates(district_centroids)
    
    district_labels <- data.frame(
      x = centroid_coords[,1],
      y = centroid_coords[,2],
      name = districts$name
    )
    
    p <- p + ggplot2::geom_text(
      data = district_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3, fontface = "bold", color = "darkblue", alpha = 0.7
    )
  }
  
  return(p)
}
