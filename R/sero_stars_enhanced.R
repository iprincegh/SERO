# Enhanced SERO functions using stars library for improved performance
# This file contains optimized versions of core SERO functions

# Declare global variables for CRAN compliance
utils::globalVariables(c("x", "y", "density", "risk_level", "hotspot_value"))

#' Enhanced Heatmap Generation using Stars
#'
#' Creates accident heatmaps using raster-based processing with stars library
#' for improved performance and scalability.
#'
#' @param accidents sf object containing accident data
#' @param risk_categories numeric vector of risk categories to include
#' @param bandwidth numeric, bandwidth for kernel density estimation in meters
#' @param grid_size numeric, grid cell size in meters
#' @param data list, SERO data object containing spatial layers
#' @param clip_to_bounds logical, whether to clip to study area bounds
#' @param resolution numeric, raster resolution in meters (default: 100)
#' @param parallel logical, whether to use parallel processing (default: TRUE)
#' @return stars object with density surface
#' @importFrom stats quantile median
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' heatmap_stars <- sero_heatmap_stars(data$accident, risk_categories = c(1, 2))
#' }
sero_heatmap_stars <- function(accidents, 
                              risk_categories = c(1, 2), 
                              bandwidth = 1000,
                              grid_size = 100,
                              data = NULL,
                              clip_to_bounds = TRUE,
                              resolution = 100,
                              parallel = TRUE) {
  
  # Input validation
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  # Filter accidents by risk categories
  if ("UKATEGORIE" %in% names(accidents)) {
    accidents_filtered <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
  } else {
    accidents_filtered <- accidents
  }
  
  if (nrow(accidents_filtered) == 0) {
    stop("No accidents found for the specified risk categories")
  }
  
  # Transform to appropriate CRS if needed
  if (sf::st_crs(accidents_filtered)$input != "EPSG:32632") {
    accidents_filtered <- sf::st_transform(accidents_filtered, 32632)
  }
  
  # Create raster template
  bbox <- sf::st_bbox(accidents_filtered)
  
  # Expand bbox slightly for buffer
  buffer_dist <- bandwidth * 2
  bbox[1] <- bbox[1] - buffer_dist  # xmin
  bbox[2] <- bbox[2] - buffer_dist  # ymin
  bbox[3] <- bbox[3] + buffer_dist  # xmax
  bbox[4] <- bbox[4] + buffer_dist  # ymax
  
  # Create stars template
  template <- stars::st_as_stars(bbox, dx = resolution, dy = resolution)
  names(template) <- "density"
  
  # Rasterize accidents - count points per cell
  accident_raster <- stars::st_rasterize(accidents_filtered, template, 
                                        options = "ALL_TOUCHED=TRUE")
  
  # Handle missing values
  accident_raster[is.na(accident_raster)] <- 0
  
  # Create kernel for convolution
  kernel_radius <- ceiling(bandwidth / resolution)
  kernel_size <- 2 * kernel_radius + 1
  
  # Create Gaussian kernel
  kernel <- create_gaussian_kernel(kernel_size, bandwidth / resolution)
  
  # Apply kernel density using convolution
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    # Use parallel processing if available
    density_surface <- tryCatch({
      apply_kernel_parallel(accident_raster, kernel)
    }, error = function(e) {
      # Fall back to sequential processing
      apply_kernel_sequential(accident_raster, kernel)
    })
  } else {
    density_surface <- apply_kernel_sequential(accident_raster, kernel)
  }
  
  # Clip to study area bounds if requested
  if (clip_to_bounds && !is.null(data) && "districts" %in% names(data)) {
    districts_utm <- sf::st_transform(data$districts, 32632)
    study_area <- sf::st_union(districts_utm)
    density_surface <- sf::st_crop(density_surface, study_area)
  }
  
  # Add metadata
  attr(density_surface, "bandwidth") <- bandwidth
  attr(density_surface, "resolution") <- resolution
  attr(density_surface, "risk_categories") <- risk_categories
  attr(density_surface, "n_accidents") <- nrow(accidents_filtered)
  
  # Set appropriate class
  class(density_surface) <- c("sero_heatmap_stars", class(density_surface))
  
  return(density_surface)
}

#' Create Gaussian Kernel for Convolution
#'
#' Creates a Gaussian kernel matrix for spatial convolution operations.
#'
#' @param size numeric, kernel size (odd number)
#' @param sigma numeric, standard deviation of Gaussian kernel
#' @return matrix, Gaussian kernel
#' @keywords internal
create_gaussian_kernel <- function(size, sigma) {
  if (size %% 2 == 0) {
    size <- size + 1  # Ensure odd size
  }
  
  center <- (size + 1) / 2
  kernel <- matrix(0, nrow = size, ncol = size)
  
  for (i in 1:size) {
    for (j in 1:size) {
      x <- i - center
      y <- j - center
      kernel[i, j] <- exp(-(x^2 + y^2) / (2 * sigma^2))
    }
  }
  
  # Normalize kernel
  kernel <- kernel / sum(kernel)
  
  return(kernel)
}

#' Apply Kernel Convolution (Sequential)
#'
#' Applies kernel convolution to raster data using sequential processing.
#'
#' @param raster_data stars object with raster data
#' @param kernel matrix, convolution kernel
#' @return stars object with convolved data
#' @keywords internal
apply_kernel_sequential <- function(raster_data, kernel) {
  # Convert to matrix for processing
  data_matrix <- as.matrix(raster_data[[1]])
  
  # Apply convolution
  result_matrix <- convolve_2d(data_matrix, kernel)
  
  # Convert back to stars
  result_stars <- raster_data
  result_stars[[1]] <- result_matrix
  
  return(result_stars)
}

#' Apply Kernel Convolution (Parallel)
#'
#' Applies kernel convolution to raster data using parallel processing.
#'
#' @param raster_data stars object with raster data
#' @param kernel matrix, convolution kernel
#' @return stars object with convolved data
#' @keywords internal
apply_kernel_parallel <- function(raster_data, kernel) {
  # For now, fall back to sequential (can be enhanced with future/parallel)
  return(apply_kernel_sequential(raster_data, kernel))
}

#' 2D Convolution Function
#'
#' Performs 2D convolution operation on matrix data.
#'
#' @param data matrix, input data
#' @param kernel matrix, convolution kernel
#' @return matrix, convolved data
#' @keywords internal
convolve_2d <- function(data, kernel) {
  # Get dimensions
  data_rows <- nrow(data)
  data_cols <- ncol(data)
  kernel_rows <- nrow(kernel)
  kernel_cols <- ncol(kernel)
  
  # Calculate padding
  pad_rows <- floor(kernel_rows / 2)
  pad_cols <- floor(kernel_cols / 2)
  
  # Create padded matrix
  padded_data <- matrix(0, 
                       nrow = data_rows + 2 * pad_rows, 
                       ncol = data_cols + 2 * pad_cols)
  
  # Fill padded matrix
  padded_data[(pad_rows + 1):(pad_rows + data_rows), 
              (pad_cols + 1):(pad_cols + data_cols)] <- data
  
  # Initialize result
  result <- matrix(0, nrow = data_rows, ncol = data_cols)
  
  # Perform convolution
  for (i in 1:data_rows) {
    for (j in 1:data_cols) {
      # Extract neighborhood
      neighborhood <- padded_data[i:(i + kernel_rows - 1), 
                                 j:(j + kernel_cols - 1)]
      
      # Apply kernel
      result[i, j] <- sum(neighborhood * kernel)
    }
  }
  
  return(result)
}

#' Enhanced Hotspot Detection using Stars
#'
#' Detects accident hotspots using raster-based analysis with stars library.
#'
#' @param accidents sf object containing accident data
#' @param risk_categories numeric vector of risk categories to include
#' @param bandwidth numeric, bandwidth for analysis in meters
#' @param threshold numeric, threshold for hotspot detection (default: 0.95)
#' @param resolution numeric, raster resolution in meters (default: 100)
#' @param min_area numeric, minimum hotspot area in square meters (default: 10000)
#' @return list containing hotspots and density surface
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' hotspots <- sero_hotspots_stars(data$accident, risk_categories = c(1, 2))
#' }
sero_hotspots_stars <- function(accidents,
                               risk_categories = c(1, 2),
                               bandwidth = 1000,
                               threshold = 0.95,
                               resolution = 100,
                               min_area = 10000) {
  
  # Create density surface using enhanced heatmap function
  density_surface <- sero_heatmap_stars(accidents, 
                                       risk_categories = risk_categories,
                                       bandwidth = bandwidth,
                                       resolution = resolution)
  
  # Calculate threshold value
  density_values <- as.vector(density_surface[[1]])
  density_values <- density_values[!is.na(density_values) & density_values > 0]
  
  if (length(density_values) == 0) {
    stop("No density values found")
  }
  
  threshold_value <- quantile(density_values, threshold, na.rm = TRUE)
  
  # Create hotspot mask
  hotspot_mask <- density_surface
  hotspot_mask[[1]] <- ifelse(density_surface[[1]] >= threshold_value, 1, 0)
  
  # Convert to polygons
  hotspot_polygons <- sf::st_as_sf(hotspot_mask, 
                                   as_points = FALSE, 
                                   merge = TRUE)
  
  # Filter by minimum area
  if (nrow(hotspot_polygons) > 0) {
    hotspot_polygons$area <- as.numeric(sf::st_area(hotspot_polygons))
    hotspot_polygons <- hotspot_polygons[hotspot_polygons$area >= min_area, ]
    
    # Add hotspot IDs
    hotspot_polygons$hotspot_id <- seq_len(nrow(hotspot_polygons))
    
    # Calculate statistics for each hotspot
    hotspot_polygons$max_density <- extract_max_density(hotspot_polygons, density_surface)
    hotspot_polygons$mean_density <- extract_mean_density(hotspot_polygons, density_surface)
  }
  
  # Create result object
  result <- list(
    hotspots = hotspot_polygons,
    density_surface = density_surface,
    threshold = threshold_value,
    parameters = list(
      bandwidth = bandwidth,
      threshold = threshold,
      resolution = resolution,
      min_area = min_area,
      risk_categories = risk_categories
    )
  )
  
  class(result) <- c("sero_hotspots_stars", "list")
  
  return(result)
}

#' Extract Maximum Density Values
#'
#' Extracts maximum density values for hotspot polygons.
#'
#' @param polygons sf object with hotspot polygons
#' @param density_surface stars object with density surface
#' @return numeric vector of maximum density values
#' @keywords internal
extract_max_density <- function(polygons, density_surface) {
  max_values <- numeric(nrow(polygons))
  
  for (i in seq_len(nrow(polygons))) {
    # Crop density surface to polygon
    cropped <- tryCatch({
      sf::st_crop(density_surface, polygons[i, ])
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(cropped)) {
      values <- as.vector(cropped[[1]])
      max_values[i] <- max(values, na.rm = TRUE)
    } else {
      max_values[i] <- NA
    }
  }
  
  return(max_values)
}

#' Extract Mean Density Values
#'
#' Extracts mean density values for hotspot polygons.
#'
#' @param polygons sf object with hotspot polygons
#' @param density_surface stars object with density surface
#' @return numeric vector of mean density values
#' @keywords internal
extract_mean_density <- function(polygons, density_surface) {
  mean_values <- numeric(nrow(polygons))
  
  for (i in seq_len(nrow(polygons))) {
    # Crop density surface to polygon
    cropped <- tryCatch({
      sf::st_crop(density_surface, polygons[i, ])
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(cropped)) {
      values <- as.vector(cropped[[1]])
      mean_values[i] <- mean(values, na.rm = TRUE)
    } else {
      mean_values[i] <- NA
    }
  }
  
  return(mean_values)
}

#' Plot Method for Stars-based Heatmap
#'
#' Plots stars-based heatmap with ggplot2 integration.
#'
#' @param x sero_heatmap_stars object
#' @param ... additional arguments passed to ggplot
#' @return ggplot2 object
#' @export
plot.sero_heatmap_stars <- function(x, ...) {
  # Convert to data frame for ggplot
  heatmap_df <- as.data.frame(x, xy = TRUE)
  
  # Create plot
  p <- ggplot2::ggplot(heatmap_df, ggplot2::aes(x = x, y = y, fill = density)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(name = "Density", na.value = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Accident Density Heatmap (Stars Enhanced)")
  
  return(p)
}

#' Plot Method for Stars-based Hotspots
#'
#' Plots stars-based hotspots with density surface.
#'
#' @param x sero_hotspots_stars object
#' @param ... additional arguments passed to ggplot
#' @return ggplot2 object
#' @export
plot.sero_hotspots_stars <- function(x, ...) {
  # Convert density surface to data frame
  density_df <- as.data.frame(x$density_surface, xy = TRUE)
  
  # Create plot
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
    ggplot2::labs(title = "Accident Hotspots (Stars Enhanced)")
  
  # Add hotspot polygons if they exist
  if (nrow(x$hotspots) > 0) {
    p <- p + ggplot2::geom_sf(data = x$hotspots, 
                             fill = "transparent", 
                             color = "red", 
                             size = 1.5,
                             inherit.aes = FALSE)
  }
  
  return(p)
}
