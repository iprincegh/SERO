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
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' hotspots <- sero_identify_hotspots(data$accident)
#' plot(hotspots)
#' }
sero_identify_hotspots <- function(accidents, 
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
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
plot.sero_hotspots <- function(x, ...) {
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
  p <- ggplot2::ggplot() +
    # Add accidents as small points
    ggplot2::geom_sf(data = accidents_wgs84, 
                    ggplot2::aes(color = factor(.data$UKATEGORIE)),
                    size = 0.5, alpha = 0.6) +
    # Add hotspots as larger points
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
    ggplot2::labs(title = "Accident Hotspots",
                 subtitle = paste("Total accidents:", x$summary$total_accidents,
                                 "| Hotspots:", x$summary$hotspot_count),
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
