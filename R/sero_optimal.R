#' Calculate optimal emergency response locations using enhanced multi-criteria analysis
#'
#' This function implements the complete multi-criteria analysis for finding optimal
#' emergency service locations based on the specified criteria:
#' 1. High-risk accidents (UKATEGORIE = 1 or 2)
#' 2. Suitable land use (residential, commercial, industrial)
#' 3. Road proximity (500m-1000m from roads)
#' 4. Accident density (100m grid cells)
#' 5. Population density (higher density prioritized)
#' 6. Centroid calculation (optimal points within grid cells)
#'
#' @param data List containing spatial data layers
#' @param grid_size Grid cell size in meters (default=100)
#' @param risk_categories High-risk accident categories (default=c(1,2))
#' @param suitable_landuse Suitable land use classes (default=c("residential", "commercial", "industrial"))
#' @param min_road_distance Minimum distance from roads in meters (default=500)
#' @param max_road_distance Maximum distance from roads in meters (default=1000)
#' @param max_locations Maximum number of locations to return (default=10)
#' @param target_crs Target CRS for calculations (default=25832 for UTM Zone 32N)
#' @return sero_optimal_locations S3 object
#' @export
sero_calculate_optimal_locations <- function(data,
                                           grid_size = 100,
                                           risk_categories = c(1, 2),
                                           suitable_landuse = c("residential", "commercial", "industrial"),
                                           min_road_distance = 500,
                                           max_road_distance = 1000,
                                           max_locations = 10,
                                           target_crs = 25832) {
  
  # Input validation
  required_layers <- c("accident", "roads")
  missing_layers <- setdiff(required_layers, names(data))
  if (length(missing_layers) > 0) {
    stop("Missing required layers: ", paste(missing_layers, collapse = ", "))
  }
  
  # Extract and validate data layers
  accidents <- data$accident
  roads <- data$roads
  landuse <- if ("landuse" %in% names(data)) data$landuse else NULL
  population <- if ("population" %in% names(data)) data$population else NULL
  
  # Transform to target CRS for accurate distance calculations
  accidents <- sf::st_transform(accidents, target_crs)
  roads <- sf::st_transform(roads, target_crs)
  if (!is.null(landuse)) landuse <- sf::st_transform(landuse, target_crs)
  if (!is.null(population)) population <- sf::st_transform(population, target_crs)
  
  # 1. Filter high-risk accidents (Criterion 1)
  accidents_high <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
  
  if (nrow(accidents_high) == 0) {
    warning("No high-risk accidents found with categories: ", 
            paste(risk_categories, collapse = ", "))
    return(create_empty_optimal_locations())
  }
  
  # 2. Create grid using ST_SnapToGrid equivalent (Criterion 4)
  bbox <- sf::st_bbox(accidents_high)
  
  # Create grid points
  x_range <- seq(bbox$xmin, bbox$xmax, by = grid_size)
  y_range <- seq(bbox$ymin, bbox$ymax, by = grid_size)
  
  # Create grid cells as squares
  grid_cells <- list()
  grid_id <- 1
  
  for (i in 1:(length(x_range) - 1)) {
    for (j in 1:(length(y_range) - 1)) {
      # Create square polygon
      square_coords <- matrix(c(
        x_range[i], y_range[j],
        x_range[i + 1], y_range[j],
        x_range[i + 1], y_range[j + 1],
        x_range[i], y_range[j + 1],
        x_range[i], y_range[j]
      ), ncol = 2, byrow = TRUE)
      
      square_poly <- sf::st_polygon(list(square_coords))
      grid_cells[[grid_id]] <- square_poly
      grid_id <- grid_id + 1
    }
  }
  
  # Convert to sf object
  grid <- sf::st_sf(
    grid_id = 1:length(grid_cells),
    geometry = sf::st_sfc(grid_cells, crs = target_crs)
  )
  
  # 3. Spatial joins and filtering
  
  # Join with accidents to get accident density
  accident_intersections <- sf::st_intersects(grid, accidents_high)
  grid$accident_count <- lengths(accident_intersections)
  
  # Filter grid cells with accidents
  grid_with_accidents <- grid[grid$accident_count > 0, ]
  
  if (nrow(grid_with_accidents) == 0) {
    warning("No grid cells contain accidents")
    return(create_empty_optimal_locations())
  }
  
  # 4. Calculate centroids (Criterion 6)
  grid_with_accidents$centroid <- sf::st_centroid(grid_with_accidents$geometry)
  
  # 5. Apply land use filter (Criterion 2)
  if (!is.null(landuse)) {
    # Find suitable land use column
    landuse_col <- NULL
    possible_cols <- c("fclass", "class", "type")
    for (col in possible_cols) {
      if (col %in% names(landuse)) {
        landuse_col <- col
        break
      }
    }
    
    if (!is.null(landuse_col)) {
      # Filter landuse by suitable classes
      suitable_landuse_areas <- landuse[landuse[[landuse_col]] %in% suitable_landuse, ]
      
      if (nrow(suitable_landuse_areas) > 0) {
        # Check which grid centroids are in suitable landuse areas
        landuse_intersections <- sf::st_intersects(grid_with_accidents$centroid, suitable_landuse_areas)
        grid_with_accidents$in_suitable_landuse <- lengths(landuse_intersections) > 0
        
        # Filter to only suitable landuse areas
        grid_with_accidents <- grid_with_accidents[grid_with_accidents$in_suitable_landuse, ]
      }
    }
  }
  
  if (nrow(grid_with_accidents) == 0) {
    warning("No grid cells found in suitable land use areas")
    return(create_empty_optimal_locations())
  }
  
  # 6. Calculate road distances (Criterion 3)
  nearest_road_indices <- sf::st_nearest_feature(grid_with_accidents$centroid, roads)
  grid_with_accidents$road_distance <- as.numeric(
    sf::st_distance(
      grid_with_accidents$centroid,
      roads[nearest_road_indices, ],
      by_element = TRUE
    )
  )
  
  # Apply road distance filter
  grid_filtered <- grid_with_accidents[
    grid_with_accidents$road_distance >= min_road_distance & 
    grid_with_accidents$road_distance <= max_road_distance, 
  ]
  
  if (nrow(grid_filtered) == 0) {
    warning("No grid cells found within road distance criteria")
    return(create_empty_optimal_locations())
  }
  
  # 7. Add population density (Criterion 5)
  grid_filtered$population_density <- 0
  if (!is.null(population)) {
    # Find population density column
    pop_col <- NULL
    possible_cols <- c("density", "pop_density", "population", "pop", "dens")
    for (col in possible_cols) {
      if (col %in% names(population)) {
        pop_col <- col
        break
      }
    }
    
    if (!is.null(pop_col)) {
      # Get population density for each grid cell
      pop_intersections <- sf::st_intersects(grid_filtered$centroid, population)
      grid_filtered$population_density <- sapply(pop_intersections, function(idx) {
        if (length(idx) > 0) {
          mean(population[[pop_col]][idx], na.rm = TRUE)
        } else {
          0
        }
      })
    }
  }
  
  # 8. Calculate composite score
  # Normalize metrics to 0-1 scale
  grid_filtered$accident_density_norm <- grid_filtered$accident_count / max(grid_filtered$accident_count)
  grid_filtered$population_density_norm <- if (max(grid_filtered$population_density) > 0) {
    grid_filtered$population_density / max(grid_filtered$population_density)
  } else {
    rep(0, nrow(grid_filtered))
  }
  grid_filtered$road_accessibility_norm <- 1 - ((grid_filtered$road_distance - min_road_distance) / 
                                               (max_road_distance - min_road_distance))
  
  # Composite score (weighted average)
  grid_filtered$score <- (
    0.4 * grid_filtered$accident_density_norm +
    0.3 * grid_filtered$population_density_norm +
    0.3 * grid_filtered$road_accessibility_norm
  )
  
  # 9. Select top locations
  grid_sorted <- grid_filtered[order(grid_filtered$score, decreasing = TRUE), ]
  top_locations <- head(grid_sorted, max_locations)
  
  # 10. Create final locations as points
  optimal_locations <- sf::st_sf(
    location_id = 1:nrow(top_locations),
    accident_count = top_locations$accident_count,
    population_density = top_locations$population_density,
    road_distance = top_locations$road_distance,
    score = top_locations$score,
    geometry = top_locations$centroid
  )
  
  # Add coordinates
  coords <- sf::st_coordinates(optimal_locations)
  optimal_locations$longitude <- coords[, 1]
  optimal_locations$latitude <- coords[, 2]
  
  # Create summary statistics
  summary_stats <- list(
    total_accidents = nrow(accidents_high),
    grid_cells_created = nrow(grid),
    grid_cells_with_accidents = nrow(grid_with_accidents),
    grid_cells_after_landuse = nrow(grid_with_accidents),
    grid_cells_after_roads = nrow(grid_filtered),
    locations_found = nrow(optimal_locations),
    max_score = max(optimal_locations$score),
    avg_score = mean(optimal_locations$score),
    criteria_applied = c(
      "1. High-Risk Accidents (UKATEGORIE = 1 or 2)",
      "2. Suitable Land Use (residential, commercial, industrial)",
      "3. Road Proximity (500m-1000m from roads)",
      "4. Accident Density (100m grid cells)",
      "5. Population Density (higher density prioritized)",
      "6. Centroid Calculation (optimal points within grid cells)"
    )
  )
  
  # Create result object
  result <- structure(
    list(
      locations = optimal_locations,
      grid_cells = grid_filtered,
      accidents = accidents_high,
      parameters = list(
        grid_size = grid_size,
        risk_categories = risk_categories,
        suitable_landuse = suitable_landuse,
        min_road_distance = min_road_distance,
        max_road_distance = max_road_distance,
        max_locations = max_locations,
        target_crs = target_crs
      ),
      summary = summary_stats,
      crs = target_crs
    ),
    class = "sero_optimal_locations"
  )
  
  return(result)
}

#' Create empty optimal locations object
#' @return empty sero_optimal_locations object
create_empty_optimal_locations <- function() {
  structure(
    list(
      locations = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      grid_cells = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      accidents = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      parameters = list(),
      summary = list(
        total_accidents = 0,
        locations_found = 0,
        max_score = 0,
        avg_score = 0
      )
    ),
    class = "sero_optimal_locations"
  )
}

#' Plot optimal locations with context
#'
#' @param x sero_optimal_locations object
#' @param data Original data for context (optional)
#' @param show_grid Logical, whether to show grid cells (default=FALSE)
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_optimal_locations <- function(x, data = NULL, show_grid = FALSE, ...) {
  if (nrow(x$locations) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No optimal locations found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  locations_wgs84 <- sf::st_transform(x$locations, 4326)
  accidents_wgs84 <- sf::st_transform(x$accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    # Add accidents as background
    ggplot2::geom_sf(data = accidents_wgs84, 
                    ggplot2::aes(color = factor(.data$UKATEGORIE)),
                    size = 0.5, alpha = 0.6) +
    # Add optimal locations
    ggplot2::geom_sf(data = locations_wgs84,
                    ggplot2::aes(size = .data$score, fill = .data$accident_count),
                    shape = 21, color = "darkgreen", alpha = 0.8, stroke = 2) +
    # Add location labels
    ggplot2::geom_sf_text(data = locations_wgs84,
                         ggplot2::aes(label = .data$location_id),
                         color = "white", fontface = "bold", size = 3) +
    # Color scales
    ggplot2::scale_color_manual(values = c("1" = "red", "2" = "orange", "3" = "yellow"),
                               name = "Accident Category",
                               labels = c("1" = "Fatal", "2" = "Serious", "3" = "Slight")) +
    ggplot2::scale_fill_viridis_c(name = "Accident Count", option = "viridis") +
    ggplot2::scale_size_continuous(name = "Optimization Score", range = c(4, 10)) +
    # Styling
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Optimal Emergency Service Locations",
                 subtitle = paste("Locations found:", x$summary$locations_found,
                                 "| Average score:", round(x$summary$avg_score, 3)),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = ggplot2::element_text(size = 8)
    )
  
  # Add grid cells if requested
  if (show_grid && nrow(x$grid_cells) > 0) {
    grid_wgs84 <- sf::st_transform(x$grid_cells, 4326)
    p <- p + ggplot2::geom_sf(data = grid_wgs84,
                             fill = "transparent",
                             color = "lightgray",
                             size = 0.2, alpha = 0.5)
  }
  
  return(p)
}

#' Print method for sero_optimal_locations
#'
#' @param x sero_optimal_locations object
#' @param ... additional arguments (unused)
#' @export
print.sero_optimal_locations <- function(x, ...) {
  cat("SERO Optimal Emergency Service Locations\n")
  cat("========================================\n\n")
  
  cat("Summary:\n")
  cat("- Total high-risk accidents:", x$summary$total_accidents, "\n")
  cat("- Grid cells created:", x$summary$grid_cells_created, "\n")
  cat("- Grid cells with accidents:", x$summary$grid_cells_with_accidents, "\n")
  cat("- Grid cells after road filtering:", x$summary$grid_cells_after_roads, "\n")
  cat("- Optimal locations found:", x$summary$locations_found, "\n")
  cat("- Maximum score:", round(x$summary$max_score, 3), "\n")
  cat("- Average score:", round(x$summary$avg_score, 3), "\n\n")
  
  cat("Parameters:\n")
  cat("- Grid size:", x$parameters$grid_size, "meters\n")
  cat("- Risk categories:", paste(x$parameters$risk_categories, collapse = ", "), "\n")
  cat("- Suitable land use:", paste(x$parameters$suitable_landuse, collapse = ", "), "\n")
  cat("- Road distance:", x$parameters$min_road_distance, "-", x$parameters$max_road_distance, "meters\n")
  cat("- Maximum locations:", x$parameters$max_locations, "\n\n")
  
  cat("Multi-criteria analysis applied:\n")
  for (i in seq_along(x$summary$criteria_applied)) {
    cat("  ", x$summary$criteria_applied[i], "\n")
  }
  
  if (x$summary$locations_found > 0) {
    cat("\nTop 5 Optimal Locations:\n")
    top_locations <- head(x$locations, 5)
    for (i in seq_len(nrow(top_locations))) {
      cat(sprintf("  %d. Score: %.3f, Accidents: %d, Population: %.1f\n",
                  i,
                  top_locations$score[i],
                  top_locations$accident_count[i],
                  top_locations$population_density[i]))
    }
  }
  
  cat("\nUse plot() to visualize with ggplot2.\n")
  cat("Use plot(x, show_grid=TRUE) to show grid cells.\n")
}
