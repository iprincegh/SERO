#' Identify accident hotspots using point pattern analysis
#'
#' Uses point pattern analysis to identify accident hotspots without interpolation.
#' Follows R-spatial best practices with sf and spatstat integration.
#'
#' @param accidents sf object with accident data (must have UKATEGORIE column)
#' @param risk_categories High-risk accident categories (default=c(1,2): 1=fatal, 2=serious)
#' @param buffer Kernel buffer distance for density estimation in meters (default=1000)
#' @param min_events Minimum number of events for hotspot identification (default=5)
#' @return sero_hotspots S3 object
#' @export
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' hotspots <- sero_hotspots(data$accident)
#' plot(hotspots)
#' }
sero_hotspots <- function(accidents, 
                                   risk_categories = c(1, 2),
                                   buffer = 1000,
                                   min_events = 5) {
  
  # Input validation
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  if (!"UKATEGORIE" %in% names(accidents)) {
    stop("accidents must have UKATEGORIE column")
  }
  
  # Filter high-risk accidents
  high_risk_accidents <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
  
  if (nrow(high_risk_accidents) == 0) {
    warning("No high-risk accidents found with categories: ", 
            paste(risk_categories, collapse = ", "))
    return(create_empty_hotspots())
  }
  
  # Ensure projected CRS
  if (sf::st_is_longlat(high_risk_accidents)) {
    high_risk_accidents <- sf::st_transform(high_risk_accidents, 32632)
  }
  
  # Convert to spatstat ppp object for point pattern analysis
  coords <- sf::st_coordinates(high_risk_accidents)
  bbox <- sf::st_bbox(high_risk_accidents)
  
  # Create observation window
  win <- spatstat.geom::owin(xrange = c(bbox[1], bbox[3]), 
                            yrange = c(bbox[2], bbox[4]))
  
  # Create point pattern with marks (severity)
  ppp_obj <- spatstat.geom::ppp(x = coords[,1], y = coords[,2], 
                               window = win, 
                               marks = high_risk_accidents$UKATEGORIE)
  
  # Calculate kernel density
  density_est <- spatstat.explore::density.ppp(ppp_obj, sigma = buffer)
  
  # Convert density to dataframe and find peaks
  density_df <- as.data.frame(density_est)
  density_df <- density_df[!is.na(density_df$value), ]
  
  if (nrow(density_df) == 0) {
    return(create_empty_hotspots())
  }
  
  # Find local maxima using a simple approach
  # Sort by density and take top locations that are separated by buffer
  density_df <- density_df[order(density_df$value, decreasing = TRUE), ]
  
  # Select hotspot locations
  hotspot_locations <- data.frame()
  for (i in seq_len(min(20, nrow(density_df)))) {
    candidate <- density_df[i, ]
    if (nrow(hotspot_locations) == 0) {
      hotspot_locations <- candidate
    } else {
      # Check if candidate is far enough from existing hotspots
      distances <- sqrt((hotspot_locations$x - candidate$x)^2 + 
                       (hotspot_locations$y - candidate$y)^2)
      if (all(distances > buffer/2)) {
        hotspot_locations <- rbind(hotspot_locations, candidate)
      }
    }
  }
  
  if (nrow(hotspot_locations) == 0) {
    return(create_empty_hotspots())
  }
  
  # Create hotspots as sf points
  hotspots <- sf::st_as_sf(
    data.frame(
      x = hotspot_locations$x,
      y = hotspot_locations$y,
      density = hotspot_locations$value
    ),
    coords = c("x", "y"),
    crs = sf::st_crs(high_risk_accidents)
  )
  
  # Count nearby accidents for each hotspot
  hotspot_buffers <- sf::st_buffer(hotspots, buffer)
  accident_counts <- sf::st_intersects(hotspot_buffers, high_risk_accidents)
  
  hotspots$accident_count <- lengths(accident_counts)
  hotspots$fatal_count <- sapply(accident_counts, function(idx) {
    if (length(idx) == 0) return(0)
    sum(high_risk_accidents$UKATEGORIE[idx] == 1)
  })
  hotspots$serious_count <- sapply(accident_counts, function(idx) {
    if (length(idx) == 0) return(0)
    sum(high_risk_accidents$UKATEGORIE[idx] == 2)
  })
  
  # Calculate severity score and filter
  hotspots$severity_score <- hotspots$fatal_count * 3 + hotspots$serious_count * 2
  hotspots <- hotspots[hotspots$accident_count >= min_events, ]
  
  if (nrow(hotspots) > 0) {
    hotspots <- hotspots[order(hotspots$severity_score, decreasing = TRUE), ]
    hotspots$rank <- seq_len(nrow(hotspots))
  }
  
  # Create summary statistics
  summary_stats <- list(
    total_accidents = nrow(high_risk_accidents),
    fatal_accidents = sum(high_risk_accidents$UKATEGORIE == 1),
    serious_accidents = sum(high_risk_accidents$UKATEGORIE == 2),
    hotspot_count = nrow(hotspots),
    max_density = ifelse(nrow(hotspots) > 0, max(hotspots$density), 0),
    avg_accidents_per_hotspot = ifelse(nrow(hotspots) > 0, mean(hotspots$accident_count), 0)
  )
  
  # Create hotspots S3 object
  result <- structure(
    list(
      hotspots = hotspots,
      accidents = high_risk_accidents,
      parameters = list(
        risk_categories = risk_categories,
        buffer = buffer,
        min_events = min_events
      ),
      summary = summary_stats,
      crs = sf::st_crs(high_risk_accidents)
    ),
    class = "sero_hotspots"
  )
  
  return(result)
}

#' Create empty hotspots object
#' @return empty sero_hotspots object
create_empty_hotspots <- function() {
  structure(
    list(
      hotspots = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      accidents = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      parameters = list(),
      summary = list(
        total_accidents = 0,
        hotspot_count = 0,
        max_severity = 0,
        avg_accidents_per_hotspot = 0
      )
    ),
    class = "sero_hotspots"
  )
}

#' Plot method for sero_hotspots using ggplot2
#'
#' @param x sero_hotspots object
#' @param data Original data for context (optional)
#' @param show_munster Logical, whether to show Münster city boundaries (default=TRUE)
#' @param show_landuse Logical, whether to show land use overlay (default=FALSE)
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_hotspots <- function(x, data = NULL, show_munster = TRUE, show_landuse = FALSE, ...) {
  if (nrow(x$hotspots) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No hotspots found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  hotspots_wgs84 <- sf::st_transform(x$hotspots, 4326)
  accidents_wgs84 <- sf::st_transform(x$accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot()
  
  # Add Münster city boundaries as base map (always use built-in districts)
  if (show_munster) {
    tryCatch({
      # Load built-in districts if not provided in data
      if (!is.null(data) && "districts" %in% names(data)) {
        districts_wgs84 <- sf::st_transform(data$districts, 4326)
      } else {
        # Load from package geopackage
        gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
        if (file.exists(gpkg_path)) {
          districts <- sf::st_read(gpkg_path, layer = "munster_districtsshp", quiet = TRUE)
          districts_wgs84 <- sf::st_transform(districts, 4326)
        } else {
          districts_wgs84 <- NULL
        }
      }
      
      if (!is.null(districts_wgs84)) {
        p <- p + ggplot2::geom_sf(data = districts_wgs84, 
                                 fill = "lightblue", 
                                 color = "darkblue", 
                                 alpha = 0.2,
                                 size = 0.8)
      }
    }, error = function(e) {
      warning("Could not load Münster districts: ", e$message)
    })
  }
  
  # Add landuse overlay if requested
  if (show_landuse && !is.null(data) && "landuse" %in% names(data)) {
    tryCatch({
      landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
      
      # Find land use column
      landuse_col <- NULL
      possible_cols <- c("fclass", "class", "type", "landuse")
      for (col in possible_cols) {
        if (col %in% names(landuse_wgs84)) {
          landuse_col <- col
          break
        }
      }
      
      if (!is.null(landuse_col)) {
        p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                                 ggplot2::aes(fill = !!rlang::sym(landuse_col)),
                                 color = "white", 
                                 size = 0.05,
                                 alpha = 0.2) +
          ggplot2::scale_fill_brewer(type = "qual", 
                                    palette = "Pastel1", 
                                    name = "Land Use")
      }
    }, error = function(e) {
      warning("Could not add land use overlay: ", e$message)
    })
  }
  
  # Add base plot layers
  p <- p +
    # Add accidents as small points
    ggplot2::geom_sf(data = accidents_wgs84, 
                    ggplot2::aes(color = factor(.data$UKATEGORIE)),
                    size = 0.5, alpha = 0.6) +
    # Add hotspots as larger points (most prominent - on top)
    ggplot2::geom_sf(data = hotspots_wgs84,
                    ggplot2::aes(size = .data$accident_count, fill = .data$severity_score),
                    shape = 21, color = "black", alpha = 0.8) +
    # Color scales
    ggplot2::scale_color_manual(values = c("1" = "red", "2" = "orange"),
                               name = "Accident Category",
                               labels = c("1" = "Fatal", "2" = "Serious")) +
    ggplot2::scale_fill_viridis_c(name = "Severity Score", option = "plasma") +
    ggplot2::scale_size_continuous(name = "Accident Count", range = c(3, 8)) +
    # Styling
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Accident Hotspots - Münster",
                 subtitle = paste("Total accidents:", x$summary$total_accidents,
                                 "| Hotspots:", x$summary$hotspot_count,
                                 "| Base map: Münster districts"),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Print method for sero_hotspots
#'
#' @param x sero_hotspots object
#' @param ... additional arguments (unused)
#' @export
print.sero_hotspots <- function(x, ...) {
  cat("SERO Accident Hotspots Analysis\n")
  cat("===============================\n\n")
  
  cat("Summary:\n")
  cat("- Total high-risk accidents:", x$summary$total_accidents, "\n")
  cat("- Fatal accidents:", x$summary$fatal_accidents, "\n")
  cat("- Serious injury accidents:", x$summary$serious_accidents, "\n")
  cat("- Hotspots identified:", x$summary$hotspot_count, "\n")
  cat("- Maximum density:", round(x$summary$max_density, 4), "\n")
  cat("- Average accidents per hotspot:", round(x$summary$avg_accidents_per_hotspot, 1), "\n\n")
  
  cat("Parameters:\n")
  cat("- Risk categories:", paste(x$parameters$risk_categories, collapse = ", "), "\n")
  cat("- Buffer:", x$parameters$buffer, "meters\n")
  cat("- Minimum events:", x$parameters$min_events, "\n\n")
  
  if (x$summary$hotspot_count > 0) {
    cat("Top 5 Hotspots:\n")
    top_hotspots <- head(x$hotspots, 5)
    for (i in seq_len(nrow(top_hotspots))) {
      cat(sprintf("  %d. %d accidents (severity: %d)\n", 
                  i, 
                  top_hotspots$accident_count[i], 
                  top_hotspots$severity_score[i]))
    }
  }
  
  cat("\nUse plot() to visualize with ggplot2.\n")
}

#' Create accident heatmap based on number of accidents
#'
#' Creates a continuous heatmap showing accident density across Münster.
#' Uses kernel density estimation to create smooth density surfaces.
#'
#' @param accidents sf object with accident data (must have UKATEGORIE column)
#' @param risk_categories High-risk accident categories (default=c(1,2): 1=fatal, 2=serious)
#' @param bandwidth Kernel bandwidth for density estimation in meters (default=1000)
#' @param grid_size Resolution of the heatmap grid in meters (default=100)
#' @param data Original data for context (optional)
#' @param show_munster Logical, whether to show Münster city boundaries (default=TRUE)
#' @param show_landuse Logical, whether to show land use overlay (default=FALSE)
#' @param show_accidents Logical, whether to show individual accident points (default=TRUE)
#' @param color_scheme Color scheme for heatmap: "viridis", "plasma", "inferno", "magma" (default="viridis")
#' @return ggplot2 object
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' heatmap_plot <- sero_heatmap(data$accident)
#' 
#' # With custom parameters
#' heatmap_plot <- sero_heatmap(data$accident, 
#'                             bandwidth = 800, 
#'                             grid_size = 50,
#'                             color_scheme = "plasma")
#' }
sero_heatmap <- function(accidents, 
                        risk_categories = c(1, 2),
                        bandwidth = 1000,
                        grid_size = 100,
                        data = NULL,
                        show_munster = TRUE,
                        show_landuse = FALSE,
                        show_accidents = TRUE,
                        color_scheme = "viridis") {
  
  # Input validation
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  if (!"UKATEGORIE" %in% names(accidents)) {
    stop("accidents must have UKATEGORIE column")
  }
  
  # Filter high-risk accidents
  high_risk_accidents <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
  
  if (nrow(high_risk_accidents) == 0) {
    warning("No high-risk accidents found with categories: ", 
            paste(risk_categories, collapse = ", "))
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No accidents found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Ensure projected CRS for accurate distance calculations
  if (sf::st_is_longlat(high_risk_accidents)) {
    high_risk_accidents <- sf::st_transform(high_risk_accidents, 32632)
  }
  
  # Get accident coordinates
  coords <- sf::st_coordinates(high_risk_accidents)
  bbox <- sf::st_bbox(high_risk_accidents)
  
  # Expand bbox slightly for better visualization
  bbox_expanded <- bbox
  bbox_expanded[1] <- bbox[1] - bandwidth  # xmin
  bbox_expanded[2] <- bbox[2] - bandwidth  # ymin
  bbox_expanded[3] <- bbox[3] + bandwidth  # xmax
  bbox_expanded[4] <- bbox[4] + bandwidth  # ymax
  
  # Create grid for heatmap
  x_seq <- seq(bbox_expanded[1], bbox_expanded[3], by = grid_size)
  y_seq <- seq(bbox_expanded[2], bbox_expanded[4], by = grid_size)
  
  # Create density surface using kernel density estimation
  # Use a simple approach with 2D kernel density
  density_grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Calculate density for each grid point
  density_values <- numeric(nrow(density_grid))
  
  for (i in seq_len(nrow(density_grid))) {
    # Calculate distances from grid point to all accidents
    distances <- sqrt((coords[,1] - density_grid$x[i])^2 + 
                     (coords[,2] - density_grid$y[i])^2)
    
    # Apply Gaussian kernel
    kernel_values <- exp(-(distances^2) / (2 * bandwidth^2))
    density_values[i] <- sum(kernel_values)
  }
  
  # Add density values to grid
  density_grid$density <- density_values
  
  # Remove zero density points for cleaner visualization
  density_grid <- density_grid[density_grid$density > 0.01, ]
  
  if (nrow(density_grid) == 0) {
    warning("No density surface generated")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No density surface generated"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Convert to sf object for transformation
  density_sf <- sf::st_as_sf(density_grid, coords = c("x", "y"), crs = 32632)
  density_wgs84 <- sf::st_transform(density_sf, 4326)
  
  # Extract coordinates for plotting
  coords_wgs84 <- sf::st_coordinates(density_wgs84)
  density_df <- data.frame(
    x = coords_wgs84[,1],
    y = coords_wgs84[,2],
    density = density_sf$density
  )
  
  # Transform accidents to WGS84
  accidents_wgs84 <- sf::st_transform(high_risk_accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot()
  
  # Add Münster city boundaries as base map
  if (show_munster) {
    tryCatch({
      # Load built-in districts if not provided in data
      if (!is.null(data) && "districts" %in% names(data)) {
        districts_wgs84 <- sf::st_transform(data$districts, 4326)
      } else {
        # Load from package geopackage
        gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
        if (file.exists(gpkg_path)) {
          districts <- sf::st_read(gpkg_path, layer = "munster_districtsshp", quiet = TRUE)
          districts_wgs84 <- sf::st_transform(districts, 4326)
        } else {
          districts_wgs84 <- NULL
        }
      }
      
      if (!is.null(districts_wgs84)) {
        p <- p + ggplot2::geom_sf(data = districts_wgs84, 
                                 fill = "lightgray", 
                                 color = "darkgray", 
                                 alpha = 0.3,
                                 size = 0.8)
      }
    }, error = function(e) {
      warning("Could not load Münster districts: ", e$message)
    })
  }
  
  # Add landuse overlay if requested
  if (show_landuse && !is.null(data) && "landuse" %in% names(data)) {
    tryCatch({
      landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
      
      # Find land use column
      landuse_col <- NULL
      possible_cols <- c("fclass", "class", "type", "landuse")
      for (col in possible_cols) {
        if (col %in% names(landuse_wgs84)) {
          landuse_col <- col
          break
        }
      }
      
      if (!is.null(landuse_col)) {
        p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                                 ggplot2::aes(fill = !!rlang::sym(landuse_col)),
                                 color = "white", 
                                 size = 0.05,
                                 alpha = 0.15) +
          ggplot2::scale_fill_brewer(type = "qual", 
                                    palette = "Pastel2", 
                                    name = "Land Use")
      }
    }, error = function(e) {
      warning("Could not add land use overlay: ", e$message)
    })
  }
  
  # Add heatmap
  p <- p + ggplot2::geom_point(data = density_df,
                              ggplot2::aes(x = x, y = y, color = density),
                              size = 1.5, alpha = 0.8)
  
  # Add individual accident points if requested
  if (show_accidents) {
    p <- p + ggplot2::geom_sf(data = accidents_wgs84,
                             ggplot2::aes(shape = factor(.data$UKATEGORIE)),
                             color = "black", 
                             size = 0.8, 
                             alpha = 0.7)
  }
  
  # Color scale based on chosen scheme
  if (color_scheme == "viridis") {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "viridis")
  } else if (color_scheme == "plasma") {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "plasma")
  } else if (color_scheme == "inferno") {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "inferno")
  } else if (color_scheme == "magma") {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "magma")
  } else {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "viridis")
  }
  
  # Shape scale for accidents
  if (show_accidents) {
    p <- p + ggplot2::scale_shape_manual(values = c("1" = 4, "2" = 1, "3" = 16),
                                        name = "Accident Category",
                                        labels = c("1" = "Fatal", "2" = "Serious", "3" = "Slight"))
  }
  
  # Styling
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Accident Density Heatmap - Münster",
      subtitle = paste("Total accidents:", nrow(high_risk_accidents),
                      "| Bandwidth:", bandwidth, "m",
                      "| Grid size:", grid_size, "m"),
      x = "Longitude", 
      y = "Latitude",
      caption = "Heatmap shows accident density using kernel density estimation"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 9, face = "italic")
    )
  
  return(p)
}

#' Get recommended heatmap parameters for different analysis types
#'
#' Provides preset parameter combinations for common analysis scenarios.
#' Makes it easier for users to get started with appropriate settings.
#'
#' @param analysis_type Type of analysis: "urban", "regional", "overview", "detailed", "presentation"
#' @return List of recommended parameters for sero_heatmap()
#' @export
#' @examples
#' \dontrun{
#' # Get parameters for urban analysis
#' urban_params <- sero_heatmap_presets("urban")
#' 
#' # Use with sero_heatmap
#' data <- sero_load_data()
#' heatmap <- do.call(sero_heatmap, c(list(data$accident), urban_params))
#' }
sero_heatmap_presets <- function(analysis_type = "default") {
  
  presets <- list(
    "urban" = list(
      risk_categories = c(1),
      bandwidth = 300,
      grid_size = 25,
      show_accidents = FALSE,
      color_scheme = "inferno"
    ),
    
    "regional" = list(
      risk_categories = c(1, 2, 3),
      bandwidth = 1500,
      grid_size = 150,
      show_accidents = TRUE,
      color_scheme = "viridis"
    ),
    
    "overview" = list(
      risk_categories = c(1, 2),
      bandwidth = 2000,
      grid_size = 200,
      show_accidents = FALSE,
      color_scheme = "viridis"
    ),
    
    "detailed" = list(
      risk_categories = c(1, 2),
      bandwidth = 500,
      grid_size = 50,
      show_accidents = TRUE,
      color_scheme = "plasma"
    ),
    
    "presentation" = list(
      risk_categories = c(1, 2),
      bandwidth = 800,
      grid_size = 75,
      show_landuse = TRUE,
      show_accidents = FALSE,
      color_scheme = "plasma"
    ),
    
    "default" = list(
      risk_categories = c(1, 2),
      bandwidth = 1000,
      grid_size = 100,
      show_accidents = TRUE,
      color_scheme = "viridis"
    )
  )
  
  if (!analysis_type %in% names(presets)) {
    warning("Unknown analysis type. Available types: ", 
            paste(names(presets), collapse = ", "))
    return(presets$default)
  }
  
  return(presets[[analysis_type]])
}

#' Create multiple heatmaps with different parameter settings
#'
#' Generates a series of heatmaps with different parameter combinations
#' for comparative analysis.
#'
#' @param accidents sf object with accident data
#' @param data Original data for context (optional)
#' @param analysis_types Vector of analysis types to generate
#' @return List of ggplot2 objects
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' heatmaps <- sero_heatmap_compare(data$accident, data, 
#'                                 c("urban", "regional", "overview"))
#' }
sero_heatmap_compare <- function(accidents, data = NULL, 
                                analysis_types = c("urban", "regional", "overview")) {
  
  heatmaps <- list()
  
  for (type in analysis_types) {
    params <- sero_heatmap_presets(type)
    params$data <- data
    
    tryCatch({
      heatmaps[[type]] <- do.call(sero_heatmap, c(list(accidents), params))
    }, error = function(e) {
      warning("Failed to create heatmap for type '", type, "': ", e$message)
      heatmaps[[type]] <- NULL
    })
  }
  
  return(heatmaps)
}
