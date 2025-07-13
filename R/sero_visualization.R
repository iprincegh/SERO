#' Visualization functions for SERO package data (road-based routing)
#'
#' This file contains functions for plotting spatial layers and creating
#' comprehensive maps for emergency response optimization analysis.
#' All routing and analysis are road-based by default.
#'
#' User Customization:
#' - All major plotting functions allow users to customize border colors for districts, land use polygons, and roads via function arguments.
#' - Colors and label shapes for accident severity, districts, and land use types are also customizable.
#' - See individual function documentation for details on available arguments.
#'
## Plot Munster city boundaries/geometries
#'
#' @param data List containing spatial data layers
#' @param show_districts Logical, whether to show district boundaries (default=TRUE)
#' @param district_label_size Numeric, size of district label text (default=3)
#' @param district_label_color Color for district label text (default="darkblue")
#' @param district_label_fontface Font face for district label text (default="bold")
#' @param district_border_color Color for district boundary (default="darkblue"). Allows users to customize the color of district borders.
#' @return ggplot2 object
#' @export
sero_plot_base <- function(data, show_districts = TRUE, district_label_size = 3, district_label_color = "darkblue", district_label_fontface = "bold", district_border_color = "darkblue") {
  
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
                    color = district_border_color, 
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
      size = district_label_size, color = district_label_color, fontface = district_label_fontface
    )
  }
  
  return(p)
}

## Plot land use data with labels
#' Plot land use data with customizable labels and colors
#'
#' This function creates a visualization of land use data with optional labels and customizable styling.
#'
#' @param data List containing spatial data layers
#' @param label_landuse Logical, whether to show land use labels (default=TRUE)
#' @param landuse_colors Named vector of colors for land use types (optional)
#' @param landuse_label_size Numeric, size of land use label text (default=2)
#' @param landuse_label_color Color for land use label text (default="black")
#' @param landuse_label_alpha Alpha for land use label text (default=0.7)
#' @param landuse_border_color Color for land use polygon borders (default="white"). Allows users to customize the color of land use polygon borders.
#' @return ggplot2 object
#' @export
sero_plot_land <- function(data, label_landuse = TRUE, landuse_colors = NULL, landuse_label_size = 2, landuse_label_color = "black", landuse_label_alpha = 0.7, landuse_border_color = "white") {
  
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
  if (!is.null(landuse_colors)) {
    colors <- landuse_colors
  } else {
    colors <- RColorBrewer::brewer.pal(base::min(11, base::max(3, length(unique_types))), "Spectral")
    if (length(unique_types) > 11) {
      colors <- grDevices::rainbow(length(unique_types))
    }
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = landuse_wgs84, 
                    ggplot2::aes(fill = .data[[landuse_col]]),
                    color = landuse_border_color, 
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
    sample_size <- base::min(50, nrow(landuse_wgs84))
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
      size = landuse_label_size, color = landuse_label_color, alpha = landuse_label_alpha
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
sero_plot_pop <- function(data, show_values = TRUE) {
  
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

## Plot road network
#' Plot road network with customizable styling
#'
#' This function creates a visualization of the road network with customizable colors and road type filtering.
#'
#' @param data List containing spatial data layers
#' @param road_type Character, type of roads to highlight (default="all")
#' @param road_border_color Color for road lines (default="darkred"). Allows users to customize the color of road borders.
#' @return ggplot2 object
#' @export
sero_plot_roads <- function(data, road_type = "all", road_border_color = "darkred") {
  
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
                    color = road_border_color, 
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


## Plot accidents with severity labels (risk category by name supported)
#' Plot accidents with severity-based styling and labels
#'
#' This function visualizes accident data with customizable colors and labels based on severity categories.
#'
#' @param data List containing spatial data layers
#' @param risk_categories Vector of risk categories to include (numeric or character, e.g. c(1,2) or c("fatal","serious"))
#' @param show_labels Logical, whether to show severity labels (default=TRUE)
#' @param severity_colors Named vector of colors for severity levels (optional)
#' @param severity_label_size Numeric, size of severity label text (default=10)
#' @param severity_label_color Color for severity label text (default="black")
#' @return ggplot2 object
#' @export
sero_plot_acc <- function(data, risk_categories = c(1, 2, 3), show_labels = TRUE, severity_colors = NULL, severity_label_size = 10, severity_label_color = "black") {
  if (!"accident" %in% names(data)) {
    warning("No accident layer found in data. Please provide 'accident' in your data list.")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No accident data available"), size = 5) +
           ggplot2::theme_void())
  }
  accidents_wgs84 <- sf::st_transform(data$accident, 4326)
  # Support risk category by name
  if (is.character(risk_categories)) {
    name_map <- c(fatal=1, serious=2, slight=3)
    risk_categories <- unname(name_map[tolower(risk_categories)])
    if (any(is.na(risk_categories))) {
      warning("Invalid risk category names. Use 'fatal', 'serious', or 'slight'.")
      risk_categories <- c(1,2,3)
    }
  }
  # Filter by risk categories
  if ("UKATEGORIE" %in% names(accidents_wgs84)) {
    accidents_filtered <- accidents_wgs84[accidents_wgs84$UKATEGORIE %in% risk_categories, ]
  } else {
    accidents_filtered <- accidents_wgs84
  }
  if (nrow(accidents_filtered) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No accidents in selected categories"), size = 5) +
           ggplot2::theme_void())
  }
  # Create severity labels
  if ("UKATEGORIE" %in% names(accidents_filtered)) {
    accidents_filtered$severity_label <- factor(
      accidents_filtered$UKATEGORIE,
      levels = c(1, 2, 3),
      labels = c("Fatal", "Serious", "Slight")
    )
    # Allow user to customize severity colors
    if (is.null(severity_colors)) {
      severity_colors <- c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")
    }
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = accidents_filtered, ggplot2::aes(color = .data$severity_label, size = .data$severity_label), alpha = 0.7) +
      ggplot2::scale_color_manual(values = severity_colors, name = "Severity") +
      ggplot2::scale_size_manual(values = c("Fatal" = 2, "Serious" = 1.5, "Slight" = 1), name = "Severity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Accidents by Severity (Road-Based)", subtitle = paste("Total accidents:", nrow(accidents_filtered)), x = "Longitude", y = "Latitude") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 8), plot.title = ggplot2::element_text(size = 14, face = "bold"), legend.position = "right")
    if (show_labels) {
      severity_counts <- table(accidents_filtered$severity_label)
      label_text <- paste(names(severity_counts), ":", severity_counts, collapse = "\n")
      p <- p + ggplot2::annotation_custom(grob = grid::textGrob(label_text, gp = grid::gpar(fontsize = severity_label_size, col = severity_label_color)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    }
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = accidents_filtered, color = "red", size = 1, alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Accident Locations (Road-Based)", subtitle = paste("Total accidents:", nrow(accidents_filtered)), x = "Longitude", y = "Latitude") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 8), plot.title = ggplot2::element_text(size = 14, face = "bold"))
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
sero_plot_combo <- function(data, include_layers = c("districts", "landuse", "roads", "accidents"), 
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
    # Sample roads for performance if large
    if (nrow(roads_wgs84) > 2000) {
      set.seed(123)
      roads_wgs84 <- roads_wgs84[sample(nrow(roads_wgs84), 2000), ]
    }
    p <- p + ggplot2::geom_sf(data = roads_wgs84, color = "darkred", size = 0.3, alpha = 0.6)
  }
  
  if ("accidents" %in% include_layers && "accidents" %in% names(data)) {
    accidents_wgs84 <- sf::st_transform(data$accident, 4326)
    # Support risk category by name in combo
    if ("UKATEGORIE" %in% names(accidents_wgs84)) {
      accidents_wgs84$severity_label <- factor(
        accidents_wgs84$UKATEGORIE,
        levels = c(1, 2, 3),
        labels = c("Fatal", "Serious", "Slight")
      )
      p <- p + ggplot2::geom_sf(data = accidents_wgs84, ggplot2::aes(color = .data$severity_label, size = .data$severity_label), alpha = 0.8) +
        ggplot2::scale_color_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow"), name = "Accident Severity") +
        ggplot2::scale_size_manual(values = c("Fatal" = 2, "Serious" = 1.5, "Slight" = 1), name = "Accident Severity")
    } else {
      p <- p + ggplot2::geom_sf(data = accidents_wgs84, color = "red", size = 1, alpha = 0.8)
    }
  }
  
  return(p)
}
