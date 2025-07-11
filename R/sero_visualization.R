#' Visualization functions for SERO package data
#'
#' This file contains functions for plotting different data layers and creating
#' comprehensive maps for emergency response optimization analysis.

#' Plot Munster city boundaries/geometries
#'
#' @param data List containing spatial data layers
#' @param show_districts Logical, whether to show district boundaries (default=TRUE)
#' @return ggplot2 object
#' @export
sero_plot_munster <- function(data, show_districts = TRUE) {
  
  if (!"districts" %in% names(data)) {
    warning("No districts layer found in data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No district data available"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  districts_wgs84 <- sf::st_transform(data$districts, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_wgs84, 
                    fill = "lightblue", 
                    color = "darkblue", 
                    alpha = 0.3,
                    size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Munster City Boundaries",
                 subtitle = paste("Districts:", nrow(districts_wgs84)),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
  
  # Add district labels if requested
  if (show_districts && "name" %in% names(districts_wgs84)) {
    district_centroids <- sf::st_centroid(districts_wgs84)
    centroid_coords <- sf::st_coordinates(district_centroids)
    
    p <- p + ggplot2::geom_text(
      data = data.frame(
        x = centroid_coords[,1],
        y = centroid_coords[,2],
        name = districts_wgs84$name
      ),
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3, color = "darkblue", fontface = "bold"
    )
  }
  
  return(p)
}

#' Plot land use data with labels
#'
#' @param data List containing spatial data layers
#' @param label_landuse Logical, whether to show land use labels (default=TRUE)
#' @return ggplot2 object
#' @export
sero_plot_landuse <- function(data, label_landuse = TRUE) {
  
  if (!"landuse" %in% names(data)) {
    warning("No landuse layer found in data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No land use data available"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
  
  # Get unique land use types
  if ("fclass" %in% names(landuse_wgs84)) {
    landuse_col <- "fclass"
  } else if ("class" %in% names(landuse_wgs84)) {
    landuse_col <- "class"
  } else if ("type" %in% names(landuse_wgs84)) {
    landuse_col <- "type"
  } else {
    landuse_col <- names(landuse_wgs84)[1]
  }
  
  unique_types <- unique(landuse_wgs84[[landuse_col]])
  
  # Create color palette
  colors <- RColorBrewer::brewer.pal(min(11, max(3, length(unique_types))), "Spectral")
  if (length(unique_types) > 11) {
    colors <- grDevices::rainbow(length(unique_types))
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = landuse_wgs84, 
                    ggplot2::aes(fill = .data[[landuse_col]]),
                    color = "white", 
                    size = 0.1,
                    alpha = 0.7) +
    ggplot2::scale_fill_manual(values = colors, name = "Land Use Type") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Land Use Classification",
                 subtitle = paste("Types:", length(unique_types)),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "right"
    )
  
  # Add land use labels if requested
  if (label_landuse && length(unique_types) <= 20) {
    # Sample some polygons for labeling to avoid overcrowding
    sample_size <- min(50, nrow(landuse_wgs84))
    sample_indices <- sample(nrow(landuse_wgs84), sample_size)
    sample_landuse <- landuse_wgs84[sample_indices, ]
    
    landuse_centroids <- sf::st_centroid(sample_landuse)
    centroid_coords <- sf::st_coordinates(landuse_centroids)
    
    p <- p + ggplot2::geom_text(
      data = data.frame(
        x = centroid_coords[,1],
        y = centroid_coords[,2],
        type = sample_landuse[[landuse_col]]
      ),
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$type),
      size = 2, color = "black", alpha = 0.7
    )
  }
  
  return(p)
}

#' Plot population density
#'
#' @param data List containing spatial data layers
#' @param show_values Logical, whether to show density values (default=TRUE)
#' @return ggplot2 object
#' @export
sero_plot_population <- function(data, show_values = TRUE) {
  
  if (!"population" %in% names(data)) {
    warning("No population layer found in data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No population data available"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  population_wgs84 <- sf::st_transform(data$population, 4326)
  
  # Find density column
  density_col <- NULL
  possible_cols <- c("density", "pop_density", "population", "pop", "dens")
  for (col in possible_cols) {
    if (col %in% names(population_wgs84)) {
      density_col <- col
      break
    }
  }
  
  if (is.null(density_col)) {
    # Use first numeric column
    numeric_cols <- names(population_wgs84)[sapply(population_wgs84, is.numeric)]
    if (length(numeric_cols) > 0) {
      density_col <- numeric_cols[1]
    } else {
      warning("No numeric density column found")
      return(ggplot2::ggplot() + 
             ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No density data available"), 
                               size = 5) +
             ggplot2::theme_void())
    }
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = population_wgs84, 
                    ggplot2::aes(fill = .data[[density_col]]),
                    color = "white", 
                    size = 0.1) +
    ggplot2::scale_fill_viridis_c(name = "Population\nDensity", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Population Density",
                 subtitle = paste("Areas:", nrow(population_wgs84)),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "right"
    )
  
  # Add density values if requested
  if (show_values && nrow(population_wgs84) <= 50) {
    pop_centroids <- sf::st_centroid(population_wgs84)
    centroid_coords <- sf::st_coordinates(pop_centroids)
    
    p <- p + ggplot2::geom_text(
      data = data.frame(
        x = centroid_coords[,1],
        y = centroid_coords[,2],
        density = round(population_wgs84[[density_col]], 1)
      ),
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$density),
      size = 2.5, color = "white", fontface = "bold"
    )
  }
  
  return(p)
}

#' Plot road network
#'
#' @param data List containing spatial data layers
#' @param road_type Character, type of roads to highlight (default="all")
#' @return ggplot2 object
#' @export
sero_plot_roads <- function(data, road_type = "all") {
  
  if (!"roads" %in% names(data)) {
    warning("No roads layer found in data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No roads data available"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  roads_wgs84 <- sf::st_transform(data$roads, 4326)
  
  # Find road classification column
  road_col <- NULL
  possible_cols <- c("fclass", "class", "type", "highway", "road_type")
  for (col in possible_cols) {
    if (col %in% names(roads_wgs84)) {
      road_col <- col
      break
    }
  }
  
  # Create base plot
  if (!is.null(road_col) && road_type != "all") {
    # Filter by road type
    roads_filtered <- roads_wgs84[roads_wgs84[[road_col]] == road_type, ]
    if (nrow(roads_filtered) == 0) {
      warning("No roads found for type: ", road_type)
      roads_filtered <- roads_wgs84
    }
  } else {
    roads_filtered <- roads_wgs84
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = roads_filtered, 
                    color = "darkred", 
                    size = 0.5,
                    alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Road Network",
                 subtitle = paste("Road segments:", nrow(roads_filtered)),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
  
  # Add road type colors if classification exists
  if (!is.null(road_col) && road_type == "all") {
    unique_types <- unique(roads_filtered[[road_col]])
    if (length(unique_types) <= 10) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = roads_filtered, 
                        ggplot2::aes(color = .data[[road_col]]),
                        size = 0.5,
                        alpha = 0.7) +
        ggplot2::scale_color_brewer(type = "qual", palette = "Set3", name = "Road Type") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Road Network by Type",
                     subtitle = paste("Road types:", length(unique_types)),
                     x = "Longitude", y = "Latitude") +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 8),
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.position = "right"
        )
    }
  }
  
  return(p)
}

#' Plot accidents with severity labels
#'
#' @param data List containing spatial data layers
#' @param risk_categories Vector of risk categories to include (default=c(1,2,3))
#' @param show_labels Logical, whether to show severity labels (default=TRUE)
#' @return ggplot2 object
#' @export
sero_plot_accidents <- function(data, risk_categories = c(1, 2, 3), show_labels = TRUE) {
  
  if (!"accident" %in% names(data)) {
    warning("No accident layer found in data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No accident data available"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  
  # Filter by risk categories
  if ("UKATEGORIE" %in% names(accidents_wgs84)) {
    accidents_filtered <- accidents_wgs84[accidents_wgs84$UKATEGORIE %in% risk_categories, ]
  } else {
    accidents_filtered <- accidents_wgs84
  }
  
  if (nrow(accidents_filtered) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No accidents in selected categories"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Create severity labels
  if ("UKATEGORIE" %in% names(accidents_filtered)) {
    accidents_filtered$severity_label <- factor(
      accidents_filtered$UKATEGORIE,
      levels = c(1, 2, 3),
      labels = c("Fatal", "Serious", "Slight")
    )
    
    # Create base plot with severity colors
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = accidents_filtered, 
                      ggplot2::aes(color = .data$severity_label, size = .data$severity_label),
                      alpha = 0.7) +
      ggplot2::scale_color_manual(
        values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow"),
        name = "Severity"
      ) +
      ggplot2::scale_size_manual(
        values = c("Fatal" = 2, "Serious" = 1.5, "Slight" = 1),
        name = "Severity"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Accidents by Severity",
                   subtitle = paste("Total accidents:", nrow(accidents_filtered)),
                   x = "Longitude", y = "Latitude") +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
    
    # Add severity count labels
    if (show_labels) {
      severity_counts <- table(accidents_filtered$severity_label)
      label_text <- paste(names(severity_counts), ":", severity_counts, collapse = "\n")
      
      p <- p + ggplot2::annotation_custom(
        grob = grid::textGrob(label_text, gp = grid::gpar(fontsize = 10)),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    }
  } else {
    # Plot without severity classification
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = accidents_filtered, 
                      color = "red", 
                      size = 1,
                      alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Accident Locations",
                   subtitle = paste("Total accidents:", nrow(accidents_filtered)),
                   x = "Longitude", y = "Latitude") +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
  }
  
  return(p)
}

#' Combine multiple plots into a single visualization
#'
#' @param data List containing spatial data layers
#' @param include_layers Vector of layer names to include
#' @param alpha_background Alpha value for background layers (default=0.3)
#' @return ggplot2 object
#' @export
sero_plot_combined <- function(data, include_layers = c("districts", "landuse", "roads", "accidents"), 
                              alpha_background = 0.3) {
  
  # Start with base plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Combined Spatial Analysis",
                 subtitle = "Emergency Response Optimization Data",
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "right"
    )
  
  # Add layers in order
  if ("districts" %in% include_layers && "districts" %in% names(data)) {
    districts_wgs84 <- sf::st_transform(data$districts, 4326)
    p <- p + ggplot2::geom_sf(data = districts_wgs84, 
                             fill = "lightblue", 
                             color = "darkblue", 
                             alpha = alpha_background,
                             size = 0.5)
  }
  
  if ("landuse" %in% include_layers && "landuse" %in% names(data)) {
    landuse_wgs84 <- sf::st_transform(data$landuse, 4326)
    landuse_col <- if ("fclass" %in% names(landuse_wgs84)) "fclass" else names(landuse_wgs84)[1]
    p <- p + ggplot2::geom_sf(data = landuse_wgs84, 
                             ggplot2::aes(fill = .data[[landuse_col]]),
                             color = "white", 
                             size = 0.1,
                             alpha = alpha_background)
  }
  
  if ("roads" %in% include_layers && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    p <- p + ggplot2::geom_sf(data = roads_wgs84, 
                             color = "darkred", 
                             size = 0.3,
                             alpha = 0.6)
  }
  
  if ("accidents" %in% include_layers && "accidents" %in% names(data)) {
    accidents_wgs84 <- sf::st_transform(data$accident, 4326)
    if ("UKATEGORIE" %in% names(accidents_wgs84)) {
      accidents_wgs84$severity_label <- factor(
        accidents_wgs84$UKATEGORIE,
        levels = c(1, 2, 3),
        labels = c("Fatal", "Serious", "Slight")
      )
      p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                               ggplot2::aes(color = .data$severity_label, size = .data$severity_label),
                               alpha = 0.8) +
        ggplot2::scale_color_manual(
          values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow"),
          name = "Accident Severity"
        ) +
        ggplot2::scale_size_manual(
          values = c("Fatal" = 2, "Serious" = 1.5, "Slight" = 1),
          name = "Accident Severity"
        )
    } else {
      p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                               color = "red", 
                               size = 1,
                               alpha = 0.8)
    }
  }
  
  return(p)
}
