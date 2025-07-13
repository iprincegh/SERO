# Restore base min/max functions and check before any spatial or raster processing
min <- base::min
max <- base::max
message("[DIAG] min is base::min: ", identical(min, base::min))
message("[DIAG] max is base::max: ", identical(max, base::max))

# Declare global variables for CRAN compliance
utils::globalVariables(c("x", "y", "lon", "lat", "..count.."))
#' Create accident hotspots using point pattern analysis
#'
#' @param accidents sf object containing accident data
#' @param bandwidth numeric, bandwidth for kernel density estimation (default: 0.01)
#' @return ggplot2 object (hotspot map)
#' @export
sero_hotspots <- function(accidents, districts = NULL, bandwidth = 0.01, n = 100) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    install.packages("MASS", repos="https://cloud.r-project.org")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    install.packages("sf", repos="https://cloud.r-project.org")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    install.packages("raster", repos="https://cloud.r-project.org")
  }
  # Transform to WGS84 and extract coordinates
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  if (!sf::st_is_longlat(accidents_wgs84)) {
    stop("accidents_wgs84 is not in WGS84 (EPSG:4326)")
  }
  coords <- sf::st_coordinates(accidents_wgs84)
  # Kernel density estimation using kde2d
  dens <- MASS::kde2d(coords[,1], coords[,2], h = bandwidth, n = n)
  dens_raster <- raster::raster(list(x = dens$x, y = dens$y, z = dens$z))
  # Clip raster to district polygon if provided
  if (!is.null(districts)) {
    districts_wgs84 <- sf::st_transform(districts, 4326)
    if (!sf::st_is_longlat(districts_wgs84)) {
      stop("districts_wgs84 is not in WGS84 (EPSG:4326)")
    }
    districts_wgs84 <- sf::st_make_valid(districts_wgs84)
    districts_wgs84 <- sf::st_union(districts_wgs84)
    # Rasterize the district polygon to the density grid
    mask_raster <- raster::raster(dens_raster)
    raster::crs(mask_raster) <- raster::crs(dens_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
    mask_raster <- raster::rasterize(sf::as_Spatial(districts_wgs84), mask_raster, field=1, background=NA)
    dens_raster <- raster::mask(dens_raster, mask_raster)
    # If the mask results in all NA, fall back to unclipped
    if (all(is.na(raster::getValues(dens_raster)))) {
      dens_raster <- raster::raster(list(x = dens$x, y = dens$y, z = dens$z))
    }
    boundary_layer <- ggplot2::geom_sf(data = sf::st_as_sf(districts_wgs84), fill = NA, color = "#08306B", size = 1.1, alpha = 1)
    # Diagnostics
    bbox <- sf::st_bbox(districts_wgs84)
    message(sprintf("District bounding box: xmin=%.5f, xmax=%.5f, ymin=%.5f, ymax=%.5f", bbox[[1]], bbox[[3]], bbox[[2]], bbox[[4]]))
    message(sprintf("District CRS: %s", sf::st_crs(districts_wgs84)$input))
  } else {
    boundary_layer <- NULL
  }
  # Convert masked raster to data.frame for ggplot
  dens_df <- as.data.frame(raster::rasterToPoints(dens_raster))
  colnames(dens_df) <- c("x", "y", "density")
  # Diagnostics: print structure and first few rows
  message("dens_df structure:")
  print(str(dens_df))
  message("dens_df head:")
  print(utils::head(dens_df))
  # Fallback: if dens_df is empty, use unclipped density
  if (nrow(dens_df) == 0) {
    dens_raster <- raster::raster(list(x = dens$x, y = dens$y, z = dens$z))
    dens_df <- as.data.frame(raster::rasterToPoints(dens_raster))
    colnames(dens_df) <- c("x", "y", "density")
    warning("Clipped density is empty; falling back to unclipped density.")
  }
  # Enforce numeric types for plotting
  dens_df$x <- as.numeric(dens_df$x)
  dens_df$y <- as.numeric(dens_df$y)
  dens_df$density <- as.numeric(dens_df$density)
  # Remove rows with NA, Inf, or non-finite values
  dens_df <- dens_df[is.finite(dens_df$x) & is.finite(dens_df$y) & is.finite(dens_df$density), ]
  # Final check: stop if any column is not numeric or dens_df is empty
  if (!is.numeric(dens_df$x) || !is.numeric(dens_df$y) || !is.numeric(dens_df$density) || nrow(dens_df) == 0) {
    stop("Density data for plotting is invalid: check rasterization and masking steps.")
  }
  message("[DIAG] typeof(min): ", typeof(min))
  print(min)
  # Plot (without axis limits)
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = dens_df, ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7, interpolate = TRUE) +
    boundary_layer +
    ggplot2::scale_fill_viridis_c(option = "plasma", direction = -1, name = "Density") +
    ggplot2::labs(title = "Accident Hotspots (Kernel Density)", x = "Longitude", y = "Latitude") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::number_format(accuracy = 0.01), limits = c(base::min(dens_df$x, na.rm=TRUE), base::max(dens_df$x, na.rm=TRUE))) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::number_format(accuracy = 0.01), limits = c(base::min(dens_df$y, na.rm=TRUE), base::max(dens_df$y, na.rm=TRUE))) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 15),
      axis.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12)
    )
  return(p)
}

#' Create accident heatmap using 2D binning
#'
#' @param accidents sf object containing accident data
#' @param bins number of bins for 2D histogram (default: 50)
#' @return ggplot2 object (heatmap)
#' @export
sero_heatmap <- function(accidents, districts = NULL, bins = 50) {
  accidents_wgs84 <- sf::st_transform(accidents, 4326)
  coords <- sf::st_coordinates(accidents_wgs84)
  df <- data.frame(lon = coords[,1], lat = coords[,2])
  p <- ggplot2::ggplot()
  if (!is.null(districts)) {
    districts_wgs84 <- sf::st_transform(districts, 4326)
    p <- p + ggplot2::geom_sf(data = districts_wgs84, fill = NA, color = "#08306B", size = 1.1, alpha = 1)
  }
  message("[DIAG] typeof(min): ", typeof(min))
  print(min)
  p <- p + ggplot2::stat_bin_2d(data = df, ggplot2::aes(x = lon, y = lat, fill = ..count..), bins = bins, color = "white", alpha = 0.8) +
    ggplot2::scale_fill_viridis_c(option = "plasma", direction = -1, name = "Count") +
    ggplot2::labs(title = "Accident Heatmap (2D Histogram)", x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 15),
      axis.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12)
    )
  return(p)
}
