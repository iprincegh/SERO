#' Calculate optimal emergency response locations using multi-criteria analysis
#'
#' Implements complete multi-criteria analysis for finding optimal emergency service locations:
#' 1. High-risk accidents (UKATEGORIE = 1 or 2)
#' 2. Areas NOT within existing land use polygons (avoiding developments)
#' 3. Road proximity (customizable distance range)
#' 4. Accident density analysis using grid cells
#' 5. Population density (higher density prioritized)
#' 6. Optimal positioning within suitable areas
#'
#' @param data List containing spatial data layers
#' @param grid_size Grid cell size in meters (default=100)
#' @param risk_categories High-risk accident categories (default=c(1,2))
#' @param min_road_distance Minimum distance from roads in meters (default=500)
#' @param max_road_distance Maximum distance from roads in meters (default=1000)
#' @param max_locations Maximum number of locations to return (default=10)
#' @param target_crs Target CRS for calculations (default=25832 for UTM Zone 32N)
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @param avoid_landuse Logical, whether to avoid existing land use areas (default=TRUE)
#' @param weights Named list of scoring weights: accident_weight, population_weight, road_weight (default: c(0.4, 0.3, 0.3))
#' @param min_accidents Minimum number of accidents per grid cell to consider (default=1)
#' @param custom_scoring_function Optional custom function for scoring (default=NULL)
#' @param landuse_types Character vector of land use types to avoid (default=NULL for all types)
#' @param population_threshold Minimum population density threshold (default=0)
#' @param verbose Logical, whether to show progress messages (default=TRUE)
#' @return sero_optimal_locations S3 object
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- sero_optimal(data)
#' 
#' # Custom weights (prioritize accidents)
#' result <- sero_optimal(data, 
#'   weights = list(accident_weight = 0.6, population_weight = 0.2, road_weight = 0.2))
#' 
#' # Urban analysis (closer to roads)
#' result <- sero_optimal(data, 
#'   min_road_distance = 200, max_road_distance = 600)
#' 
#' # Custom scoring function
#' my_scoring <- function(data) {
#'   data$weights$accident_weight * data$accident_density_norm^2 +
#'   data$weights$population_weight * data$population_density_norm +
#'   data$weights$road_weight * data$road_accessibility_norm
#' }
#' result <- sero_optimal(data, custom_scoring_function = my_scoring)
#' }
sero_optimal <- function(data,
                        grid_size = 100,
                        risk_categories = c(1, 2),
                        min_road_distance = 500,
                        max_road_distance = 1000,
                        max_locations = 10,
                        target_crs = 25832,
                        avoid_landuse = TRUE,
                        weights = list(accident_weight = 0.4, population_weight = 0.3, road_weight = 0.3),
                        min_accidents = 1,
                        custom_scoring_function = NULL,
                        landuse_types = NULL,
                        population_threshold = 0,
                        verbose = TRUE) {
  
  # Input validation
  required_layers <- c("accident", "roads")
  missing_layers <- setdiff(required_layers, names(data))
  if (length(missing_layers) > 0) {
    stop("Missing required layers: ", paste(missing_layers, collapse = ", "))
  }
  
  # Basic parameter validation
  if (!is.list(weights)) {
    stop("weights must be a named list")
  }
  
  # Set default weights if not provided
  default_weights <- list(accident_weight = 0.4, population_weight = 0.3, road_weight = 0.3)
  weights <- modifyList(default_weights, weights)
  
  # Normalize weights to sum to 1
  weight_sum <- sum(unlist(weights))
  if (weight_sum > 0) {
    weights <- lapply(weights, function(w) w / weight_sum)
  }
  
  # Basic numeric validation
  if (grid_size <= 0) stop("grid_size must be positive")
  if (min_road_distance < 0) stop("min_road_distance must be non-negative")
  if (max_road_distance < min_road_distance) stop("max_road_distance must be >= min_road_distance")
  if (max_locations <= 0) stop("max_locations must be positive")
  
  if (verbose) {
    message("Starting optimal location analysis...")
    message("Grid size: ", grid_size, "m | Weights: Accident=", round(weights$accident_weight, 2), 
            ", Population=", round(weights$population_weight, 2), ", Road=", round(weights$road_weight, 2))
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
  
  # 2. Create grid covering entire Münster area (Criterion 4)
  # Create boundary from landuse data (covering entire Münster)
  if (!is.null(landuse)) {
    münster_boundary <- sf::st_union(landuse)
    münster_boundary <- sf::st_convex_hull(münster_boundary)
    münster_boundary <- sf::st_sf(geometry = münster_boundary)
  } else {
    # Fallback to accident bbox if no landuse data
    münster_boundary <- sf::st_as_sfc(sf::st_bbox(accidents_high))
    münster_boundary <- sf::st_sf(geometry = münster_boundary)
  }
  
  # Create analysis grid covering entire Münster
  grid_cells <- sf::st_make_grid(
    münster_boundary,
    cellsize = grid_size,
    what = "polygons"
  )
  
  # Convert to sf object and filter to Münster boundary
  grid <- sf::st_sf(
    grid_id = seq_along(grid_cells),
    geometry = grid_cells
  )
  
  # Filter grid cells within Münster boundary
  within_boundary <- sf::st_within(grid, münster_boundary, sparse = FALSE)
  grid <- grid[within_boundary, ]
  
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
  
  # 5. Apply land use avoidance filter (Criterion 2)
  # NEW LOGIC: Avoid areas within ANY existing land use polygons
  if (!is.null(landuse) && avoid_landuse) {
    if (verbose) {
      message("Applying land use avoidance filter...")
    }
    
    # Transform landuse to target CRS
    landuse_transformed <- sf::st_transform(landuse, target_crs)
    
    # Filter by specific land use types if provided
    if (!is.null(landuse_types)) {
      # Try to find land use type column (common names)
      type_cols <- c("type", "landuse", "land_use", "category", "class", "LANDUSE", "TYPE")
      landuse_col <- NULL
      
      for (col in type_cols) {
        if (col %in% names(landuse_transformed)) {
          landuse_col <- col
          break
        }
      }
      
      if (!is.null(landuse_col)) {
        landuse_transformed <- landuse_transformed[landuse_transformed[[landuse_col]] %in% landuse_types, ]
        if (verbose) {
          message("  Filtering for specific land use types: ", paste(landuse_types, collapse=", "))
          message("  Land use polygons to avoid: ", nrow(landuse_transformed))
        }
      } else {
        if (verbose) {
          message("  Warning: Could not find land use type column. Using all land use polygons.")
        }
      }
    }
    
    # Create union of land use polygons to avoid
    if (nrow(landuse_transformed) > 0) {
      landuse_union <- sf::st_union(landuse_transformed)
      
      # Check which grid centroids are NOT within any land use areas
      landuse_intersections <- sf::st_intersects(grid_with_accidents$centroid, landuse_union)
      grid_with_accidents$avoids_landuse <- lengths(landuse_intersections) == 0
      
      # Filter to only areas that avoid existing land use
      grid_with_accidents <- grid_with_accidents[grid_with_accidents$avoids_landuse, ]
      
      if (verbose) {
        message("  Grid cells after land use avoidance: ", nrow(grid_with_accidents))
      }
    } else {
      if (verbose) {
        message("  No land use polygons found to avoid")
      }
    }
  }
  
  if (nrow(grid_with_accidents) == 0) {
    warning("No grid cells found that avoid existing land use areas")
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
  
  cat("Grid cells after road proximity filter:", nrow(grid_filtered), "\n")
  
  if (nrow(grid_filtered) == 0) {
    warning("No grid cells found within road distance criteria (", min_road_distance, "m - ", max_road_distance, "m)")
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
  
  # Apply custom scoring function if provided
  if (!is.null(custom_scoring_function)) {
    if (verbose) {
      message("Applying custom scoring function...")
    }
    
    # Prepare data for custom scoring function
    scoring_data <- list(
      accident_density_norm = grid_filtered$accident_density_norm,
      population_density_norm = grid_filtered$population_density_norm,
      road_accessibility_norm = grid_filtered$road_accessibility_norm,
      accident_count = grid_filtered$accident_count,
      population_density = grid_filtered$population_density,
      road_distance = grid_filtered$road_distance,
      weights = weights
    )
    
    # Apply custom scoring function
    tryCatch({
      grid_filtered$score <- custom_scoring_function(scoring_data)
    }, error = function(e) {
      stop("Error in custom scoring function: ", e$message)
    })
    
    # Validate custom scoring output
    if (!is.numeric(grid_filtered$score) || any(is.na(grid_filtered$score))) {
      stop("Custom scoring function must return a numeric vector with no missing values")
    }
    
  } else {
    # Use weighted composite score (default)
    grid_filtered$score <- (
      weights$accident_weight * grid_filtered$accident_density_norm +
      weights$population_weight * grid_filtered$population_density_norm +
      weights$road_weight * grid_filtered$road_accessibility_norm
    )
  }
  
  # Apply minimum thresholds
  if (min_accidents > 0) {
    grid_filtered <- grid_filtered[grid_filtered$accident_count >= min_accidents, ]
    if (verbose) {
      message("Grid cells after minimum accidents filter: ", nrow(grid_filtered))
    }
  }
  
  if (population_threshold > 0) {
    grid_filtered <- grid_filtered[grid_filtered$population_density >= population_threshold, ]
    if (verbose) {
      message("Grid cells after population threshold filter: ", nrow(grid_filtered))
    }
  }
  
  if (nrow(grid_filtered) == 0) {
    warning("No grid cells meet the minimum threshold criteria")
    return(create_empty_optimal_locations())
  }
  
  # 9. Select top locations
  grid_sorted <- grid_filtered[order(grid_filtered$score, decreasing = TRUE), ]
  top_locations <- head(grid_sorted, max_locations)
  
  # 10. Create final locations as points
  optimal_locations <- sf::st_sf(
    location_id = seq_len(nrow(top_locations)),
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
      "2. Areas NOT within existing land use polygons (avoids developments)",
      "3. Road Proximity (500m-1000m from roads)",
      "4. Accident Density (grid cell analysis)",
      "5. Population Density (higher density prioritized)",
      "6. Optimal Positioning (centroid calculation)"
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
        avoid_landuse = avoid_landuse,
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
#' @param show_munster Logical, whether to show Münster city boundaries (default=TRUE)
#' @param show_hotspots Logical, whether to show accident hotspots (default=TRUE)
#' @param show_landuse Logical, whether to show land use overlay (default=FALSE)
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_optimal_locations <- function(x, data = NULL, show_grid = FALSE, show_munster = TRUE, show_hotspots = TRUE, show_landuse = FALSE, ...) {
  if (nrow(x$locations) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No optimal locations found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  locations_wgs84 <- sf::st_transform(x$locations, 4326)
  accidents_wgs84 <- sf::st_transform(x$accidents, 4326)
  
  # Create base plot with Münster boundaries
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

  # Add accident hotspots
  if (show_hotspots && !is.null(data)) {
    tryCatch({
      hotspots <- sero_hotspots(data$accident, buffer = 1000)
      if (nrow(hotspots$hotspots) > 0) {
        hotspots_wgs84 <- sf::st_transform(hotspots$hotspots, 4326)
        p <- p + ggplot2::geom_sf(data = hotspots_wgs84,
                                 fill = "red", 
                                 color = "darkred",
                                 alpha = 0.3,
                                 size = 0.5)
      }
    }, error = function(e) {
      warning("Could not add hotspots: ", e$message)
    })
  }
  
  # Add accident distributions
  p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                           ggplot2::aes(color = factor(.data$UKATEGORIE)),
                           size = 0.8, alpha = 0.7)
  
  # Add optimal locations
  p <- p + ggplot2::geom_sf(data = locations_wgs84,
                           ggplot2::aes(size = .data$score, fill = .data$accident_count),
                           shape = 21, color = "darkgreen", alpha = 0.9, stroke = 2)
  
  # Add location labels
  p <- p + ggplot2::geom_sf_text(data = locations_wgs84,
                                ggplot2::aes(label = .data$location_id),
                                color = "white", fontface = "bold", size = 3)
  
  # Color scales
  p <- p + ggplot2::scale_color_manual(values = c("1" = "red", "2" = "orange", "3" = "yellow"),
                                      name = "Accident Category",
                                      labels = c("1" = "Fatal", "2" = "Serious", "3" = "Slight"))
  
  p <- p + ggplot2::scale_fill_viridis_c(name = "Accident Count", option = "viridis")
  
  p <- p + ggplot2::scale_size_continuous(name = "Optimization Score", range = c(4, 10))
  
  # Add grid cells if requested
  if (show_grid && nrow(x$grid_cells) > 0) {
    grid_wgs84 <- sf::st_transform(x$grid_cells, 4326)
    p <- p + ggplot2::geom_sf(data = grid_wgs84,
                             fill = "transparent",
                             color = "lightgray",
                             size = 0.2, alpha = 0.5)
  }
  
  # Styling
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(title = "Optimal Emergency Service Locations - Münster",
                 subtitle = paste("Locations found:", x$summary$locations_found,
                                 "| Average score:", round(x$summary$avg_score, 3),
                                 "| Base map: Münster districts",
                                 "| Land use:", if(show_landuse) "shown" else "hidden"),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
  
  return(p)
}

#' Create comprehensive map with all layers
#'
#' @param optimal_locations sero_optimal_locations object
#' @param data Original data containing all spatial layers
#' @param show_grid Logical, whether to show grid cells (default=FALSE)
#' @param show_munster Logical, whether to show Münster city boundaries (default=TRUE)
#' @param show_hotspots Logical, whether to show accident hotspots (default=TRUE)
#' @param show_roads Logical, whether to show road network (default=TRUE)
#' @param show_landuse Logical, whether to show land use (default=FALSE)
#' @param show_population Logical, whether to show population density (default=FALSE)
#' @return ggplot2 object
#' @export
sero_plot_all <- function(optimal_locations, data, 
                                   show_grid = FALSE, 
                                   show_munster = TRUE, 
                                   show_hotspots = TRUE,
                                   show_roads = TRUE,
                                   show_landuse = FALSE,
                                   show_population = FALSE) {
  
  if (nrow(optimal_locations$locations) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No optimal locations found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  locations_wgs84 <- sf::st_transform(optimal_locations$locations, 4326)
  accidents_wgs84 <- sf::st_transform(optimal_locations$accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot()
  
  # 1. Add Münster city boundaries as base map (always use built-in districts)
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
                                 alpha = 0.15,
                                 size = 1.2)
      }
    }, error = function(e) {
      warning("Could not load Münster districts: ", e$message)
    })
  }
  
  # 2. Add population density (if requested)
  if (show_population && !is.null(data) && "population" %in% names(data)) {
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
    
    if (!is.null(density_col)) {
      p <- p + ggplot2::geom_sf(data = population_wgs84, 
                               ggplot2::aes(fill = !!rlang::sym(density_col)),
                               color = "white", 
                               size = 0.05,
                               alpha = 0.4) +
        ggplot2::scale_fill_viridis_c(name = "Population\nDensity", 
                                     option = "plasma", 
                                     trans = "sqrt")
    }
  }
  
  # 3. Add land use (if requested)
  if (show_landuse && !is.null(data) && "landuse" %in% names(data)) {
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
                               alpha = 0.3) +
        ggplot2::scale_fill_brewer(type = "qual", 
                                  palette = "Set3", 
                                  name = "Land Use")
    }
  }
  
  # 4. Add road network
  if (show_roads && !is.null(data) && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    p <- p + ggplot2::geom_sf(data = roads_wgs84, 
                             color = "darkgray", 
                             size = 0.3,
                             alpha = 0.6)
  }
  
  # 5. Add accident hotspots
  if (show_hotspots && !is.null(data)) {
    tryCatch({
      hotspots <- sero_hotspots(data$accident, buffer = 1000)
      if (nrow(hotspots$hotspots) > 0) {
        hotspots_wgs84 <- sf::st_transform(hotspots$hotspots, 4326)
        p <- p + ggplot2::geom_sf(data = hotspots_wgs84,
                                 fill = "red", 
                                 color = "darkred",
                                 alpha = 0.4,
                                 size = 0.8)
      }
    }, error = function(e) {
      warning("Could not add hotspots: ", e$message)
    })
  }
  
  # 6. Add grid cells if requested
  if (show_grid && nrow(optimal_locations$grid_cells) > 0) {
    grid_wgs84 <- sf::st_transform(optimal_locations$grid_cells, 4326)
    p <- p + ggplot2::geom_sf(data = grid_wgs84,
                             fill = "transparent",
                             color = "lightgray",
                             size = 0.2, alpha = 0.5)
  }
  
  # 7. Add accident distributions
  p <- p + ggplot2::geom_sf(data = accidents_wgs84, 
                           ggplot2::aes(color = factor(.data$UKATEGORIE)),
                           size = 1.2, alpha = 0.8)
  
  # 8. Add optimal locations (most important - on top)
  p <- p + ggplot2::geom_sf(data = locations_wgs84,
                           ggplot2::aes(size = .data$score),
                           shape = 21, 
                           color = "darkgreen", 
                           fill = "lightgreen",
                           alpha = 0.9, 
                           stroke = 2)
  
  # 9. Add location labels
  p <- p + ggplot2::geom_sf_text(data = locations_wgs84,
                                ggplot2::aes(label = .data$location_id),
                                color = "black", 
                                fontface = "bold", 
                                size = 3.5)
  
  # Color scales
  p <- p + ggplot2::scale_color_manual(values = c("1" = "red", "2" = "orange", "3" = "yellow"),
                                      name = "Accident Category",
                                      labels = c("1" = "Fatal", "2" = "Serious", "3" = "Slight"))
  
  p <- p + ggplot2::scale_size_continuous(name = "Optimization Score", 
                                         range = c(6, 14),
                                         breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
                                         labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))
  
  # Styling
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Comprehensive Emergency Response Optimization Map - Münster",
      subtitle = paste("Optimal locations:", optimal_locations$summary$locations_found,
                      "| Base map: Münster districts",
                      "| Land use:", if(show_landuse) "shown" else "hidden"),
      x = "Longitude", 
      y = "Latitude",
      caption = "Base map: Münster districts | Red areas: Accident hotspots | Green circles: Optimal locations"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 9, style = "italic")
    )
  
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

#' Print detailed summary of optimal location analysis
#'
#' @param x sero_optimal_locations object
#' @param detailed Logical, whether to show detailed breakdown (default=TRUE)
#' @param show_locations Logical, whether to show individual location details (default=TRUE)
#' @param show_criteria Logical, whether to show criteria breakdown (default=TRUE)
#' @return NULL (prints to console)
#' @export
summary.sero_optimal_locations <- function(x, detailed = TRUE, show_locations = TRUE, show_criteria = TRUE) {
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("                    SERO OPTIMAL LOCATION ANALYSIS SUMMARY                    \n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n\n")
  
  # 1. OVERVIEW
  cat("📊 ANALYSIS OVERVIEW\n")
  cat("──────────────────────────────────────────────────────────────────────────────\n")
  cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Target Area: Münster, Germany\n")
  cat("Analysis Type: Multi-criteria Emergency Service Location Optimization\n")
  cat("Status:", if(x$summary$locations_found > 0) "✅ SUCCESS" else "❌ NO LOCATIONS FOUND", "\n\n")
  
  # 2. DATA SUMMARY
  cat("📋 INPUT DATA SUMMARY\n")
  cat("──────────────────────────────────────────────────────────────────────────────\n")
  cat("• Total accident records processed:", x$summary$total_accidents, "\n")
  cat("• Risk categories analyzed:", paste(x$parameters$risk_categories, collapse = ", "), "\n")
  cat("• Grid resolution:", x$parameters$grid_size, "meters\n")
  cat("• Target coordinate system: EPSG:", x$parameters$target_crs, "\n")
  cat("• Land use avoidance:", if(x$parameters$avoid_landuse) "✅ ENABLED" else "❌ DISABLED", "\n")
  cat("• Road proximity criteria:", x$parameters$min_road_distance, "-", x$parameters$max_road_distance, "meters\n\n")
  
  # 3. FILTERING PIPELINE
  if (detailed) {
    cat("🔍 FILTERING PIPELINE RESULTS\n")
    cat("──────────────────────────────────────────────────────────────────────────────\n")
    cat("Step 1: Grid creation            →", sprintf("%5d", x$summary$grid_cells_created), "cells\n")
    cat("Step 2: Accident intersection    →", sprintf("%5d", x$summary$grid_cells_with_accidents), "cells with accidents\n")
    
    if (x$parameters$avoid_landuse) {
      cat("Step 3: Land use avoidance       →", sprintf("%5d", x$summary$grid_cells_after_landuse), "cells avoiding landuse\n")
    } else {
      cat("Step 3: Land use avoidance       →", sprintf("%5s", "SKIP"), "(disabled)\n")
    }
    
    cat("Step 4: Road proximity filter    →", sprintf("%5d", x$summary$grid_cells_after_roads), "cells meeting road criteria\n")
    cat("Step 5: Final optimization       →", sprintf("%5d", x$summary$locations_found), "optimal locations\n")
    
    # Calculate filtering efficiency
    if (x$summary$grid_cells_created > 0) {
      efficiency <- round((x$summary$locations_found / x$summary$grid_cells_created) * 100, 2)
      cat("                                   ", sprintf("%5s", paste0(efficiency, "%")), "overall efficiency\n")
    }
    cat("\n")
  }
  
  # 4. OPTIMIZATION RESULTS
  cat("🎯 OPTIMIZATION RESULTS\n")
  cat("──────────────────────────────────────────────────────────────────────────────\n")
  
  if (x$summary$locations_found > 0) {
    cat("✅ Analysis completed successfully!\n")
    cat("• Optimal locations found:", x$summary$locations_found, "out of", x$parameters$max_locations, "requested\n")
    cat("• Quality metrics:\n")
    cat("  - Highest optimization score:", sprintf("%.3f", x$summary$max_score), "\n")
    cat("  - Average optimization score:", sprintf("%.3f", x$summary$avg_score), "\n")
    cat("  - Score range:", sprintf("%.3f", min(x$locations$score)), "-", sprintf("%.3f", max(x$locations$score)), "\n")
    
    # Additional statistics
    cat("• Location characteristics:\n")
    cat("  - Total accidents covered:", sum(x$locations$accident_count), "\n")
    cat("  - Average accidents per location:", sprintf("%.1f", mean(x$locations$accident_count)), "\n")
    cat("  - Average road distance:", sprintf("%.0f", mean(x$locations$road_distance)), "meters\n")
    
    if (any(x$locations$population_density > 0)) {
      cat("  - Average population density:", sprintf("%.1f", mean(x$locations$population_density)), "\n")
    }
    
  } else {
    cat("❌ No optimal locations found!\n")
    cat("• Possible reasons:\n")
    cat("  - Criteria too restrictive (try adjusting road distance or grid size)\n")
    cat("  - Limited accident data in target area\n")
    cat("  - Land use constraints too strict\n")
  }
  cat("\n")
  
  # 5. CRITERIA BREAKDOWN
  if (show_criteria) {
    cat("📐 MULTI-CRITERIA ANALYSIS DETAILS\n")
    cat("──────────────────────────────────────────────────────────────────────────────\n")
    for (i in seq_along(x$summary$criteria_applied)) {
      cat(sprintf("  %s\n", x$summary$criteria_applied[i]))
    }
    cat("\n• Scoring weights:\n")
    cat("  - Accident density: 40%\n")
    cat("  - Population density: 30%\n")
    cat("  - Road accessibility: 30%\n\n")
  }
  
  # 6. INDIVIDUAL LOCATIONS
  if (show_locations && x$summary$locations_found > 0) {
    cat("📍 INDIVIDUAL LOCATION DETAILS\n")
    cat("──────────────────────────────────────────────────────────────────────────────\n")
    
    # Create a formatted table
    locations_df <- data.frame(
      ID = x$locations$location_id,
      Score = sprintf("%.3f", x$locations$score),
      Accidents = x$locations$accident_count,
      RoadDist = sprintf("%.0fm", x$locations$road_distance),
      PopDensity = sprintf("%.1f", x$locations$population_density),
      Longitude = sprintf("%.6f", x$locations$longitude),
      Latitude = sprintf("%.6f", x$locations$latitude)
    )
    
    # Print table header
    cat(sprintf("%-3s %-8s %-9s %-9s %-10s %-11s %-10s\n", 
                "ID", "Score", "Accidents", "RoadDist", "PopDensity", "Longitude", "Latitude"))
    cat("─────────────────────────────────────────────────────────────────────────────\n")
    
    # Print each location
    for (i in seq_len(nrow(locations_df))) {
      cat(sprintf("%-3s %-8s %-9s %-9s %-10s %-11s %-10s\n",
                  locations_df$ID[i],
                  locations_df$Score[i],
                  locations_df$Accidents[i],
                  locations_df$RoadDist[i],
                  locations_df$PopDensity[i],
                  locations_df$Longitude[i],
                  locations_df$Latitude[i]))
    }
    cat("\n")
  }
  
  # 7. RECOMMENDATIONS
  cat("💡 RECOMMENDATIONS\n")
  cat("──────────────────────────────────────────────────────────────────────────────\n")
  
  if (x$summary$locations_found > 0) {
    cat("✅ Next steps:\n")
    cat("  1. Visualize results: plot(result) or sero_plot_all(result, data)\n")
    cat("  2. Validate locations through field assessment\n")
    cat("  3. Consider local regulations and zoning requirements\n")
    cat("  4. Evaluate infrastructure requirements for each location\n")
    
    # Performance-based recommendations
    if (x$summary$max_score < 0.5) {
      cat("  ⚠️  Low optimization scores detected - consider adjusting criteria\n")
    }
    
    if (x$summary$locations_found < x$parameters$max_locations) {
      cat("  ⚠️  Fewer locations found than requested - consider relaxing constraints\n")
    }
    
  } else {
    cat("❌ Troubleshooting suggestions:\n")
    cat("  1. Increase road distance range (e.g., 100-2000m)\n")
    cat("  2. Increase grid size for broader coverage\n")
    cat("  3. Disable land use avoidance: avoid_landuse = FALSE\n")
    cat("  4. Include more accident risk categories\n")
    cat("  5. Check data quality and coverage\n")
  }
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("                              END OF SUMMARY                                  \n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
}

#' Export optimal location results to various formats
#'
#' @param x sero_optimal_locations object
#' @param format Export format ("csv", "geojson", "shapefile", "xlsx")
#' @param filename Output filename (without extension)
#' @param include_grid Logical, whether to include grid cells in export
#' @return Character vector of created files
#' @export
sero_export_results <- function(x, format = "csv", filename = "sero_optimal_locations", include_grid = FALSE) {
  
  if (nrow(x$locations) == 0) {
    warning("No locations to export")
    return(character(0))
  }
  
  created_files <- character(0)
  
  # Export locations
  if (format == "csv") {
    # Convert to data frame with coordinates
    locations_df <- sf::st_drop_geometry(x$locations)
    coords <- sf::st_coordinates(x$locations)
    locations_df$longitude <- coords[, 1]
    locations_df$latitude <- coords[, 2]
    
    csv_file <- paste0(filename, "_locations.csv")
    write.csv(locations_df, csv_file, row.names = FALSE)
    created_files <- c(created_files, csv_file)
    
    # Export summary
    summary_file <- paste0(filename, "_summary.csv")
    summary_df <- data.frame(
      Metric = c("Total Accidents", "Grid Cells Created", "Grid Cells with Accidents", 
                "Grid Cells After Roads", "Locations Found", "Max Score", "Avg Score"),
      Value = c(x$summary$total_accidents, x$summary$grid_cells_created, 
               x$summary$grid_cells_with_accidents, x$summary$grid_cells_after_roads,
               x$summary$locations_found, x$summary$max_score, x$summary$avg_score)
    )
    write.csv(summary_df, summary_file, row.names = FALSE)
    created_files <- c(created_files, summary_file)
    
  } else if (format == "geojson") {
    geojson_file <- paste0(filename, "_locations.geojson")
    sf::st_write(x$locations, geojson_file, delete_dsn = TRUE, quiet = TRUE)
    created_files <- c(created_files, geojson_file)
    
  } else if (format == "shapefile") {
    shp_file <- paste0(filename, "_locations.shp")
    sf::st_write(x$locations, shp_file, delete_dsn = TRUE, quiet = TRUE)
    created_files <- c(created_files, shp_file)
    
  } else if (format == "xlsx") {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      xlsx_file <- paste0(filename, "_results.xlsx")
      
      # Create workbook
      wb <- openxlsx::createWorkbook()
      
      # Add locations sheet
      openxlsx::addWorksheet(wb, "Locations")
      locations_df <- sf::st_drop_geometry(x$locations)
      coords <- sf::st_coordinates(x$locations)
      locations_df$longitude <- coords[, 1]
      locations_df$latitude <- coords[, 2]
      openxlsx::writeData(wb, "Locations", locations_df)
      
      # Add summary sheet
      openxlsx::addWorksheet(wb, "Summary")
      summary_df <- data.frame(
        Metric = c("Total Accidents", "Grid Cells Created", "Grid Cells with Accidents", 
                  "Grid Cells After Roads", "Locations Found", "Max Score", "Avg Score"),
        Value = c(x$summary$total_accidents, x$summary$grid_cells_created, 
                 x$summary$grid_cells_with_accidents, x$summary$grid_cells_after_roads,
                 x$summary$locations_found, x$summary$max_score, x$summary$avg_score)
      )
      openxlsx::writeData(wb, "Summary", summary_df)
      
      # Add parameters sheet
      openxlsx::addWorksheet(wb, "Parameters")
      params_df <- data.frame(
        Parameter = names(x$parameters),
        Value = sapply(x$parameters, function(p) paste(p, collapse = ", "))
      )
      openxlsx::writeData(wb, "Parameters", params_df)
      
      openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
      created_files <- c(created_files, xlsx_file)
      
    } else {
      warning("Package 'openxlsx' required for Excel export")
    }
  }
  
  # Export grid if requested
  if (include_grid && nrow(x$grid_cells) > 0) {
    if (format == "geojson") {
      grid_file <- paste0(filename, "_grid.geojson")
      sf::st_write(x$grid_cells, grid_file, delete_dsn = TRUE, quiet = TRUE)
      created_files <- c(created_files, grid_file)
    } else if (format == "shapefile") {
      grid_file <- paste0(filename, "_grid.shp")
      sf::st_write(x$grid_cells, grid_file, delete_dsn = TRUE, quiet = TRUE)
      created_files <- c(created_files, grid_file)
    }
  }
  
  cat("✅ Results exported to:", length(created_files), "files\n")
  for (file in created_files) {
    cat("  -", file, "\n")
  }
  
  return(created_files)
}
