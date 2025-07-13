# Restore base min/max functions before any spatial or raster processing
min <- base::min
max <- base::max

# Declare global variables for CRAN compliance
utils::globalVariables(c("x", "y", "lon", "lat", "..count..", ".data", "UKATEGORIE", "density"))
# NOTE: The simple sero_hotspots function has been removed to avoid conflicts
# with the comprehensive version in hotspots.R. Use sero_heatmap() for 
# kernel density visualization or sero_hotspots() from hotspots.R for 
# comprehensive hotspot analysis.

#' Create enhanced accident heatmap with customizable features
#'
#' This function creates sophisticated heatmaps of accident data with support for:
#' - Multiple basemap options (districts, OSM, none)
#' - Clipping to Munster city boundaries
#' - Custom color schemes and transparency controls
#' - Kernel density estimation with various kernels
#'
#' @param accidents sf object containing accident data with UKATEGORIE column
#' @param risk_categories numeric vector, categories to include (1=fatal, 2=severe, 3=slight)
#' @param risk_category_names character vector, alternative to risk_categories ("fatal", "severe", "slight")
#' @param bandwidth numeric, bandwidth for kernel density estimation in meters (default: 1000)
#' @param grid_size numeric, grid cell size in meters (default: 100)
#' @param data list, SERO data object containing districts and other spatial layers
#' @param clip_to_munster logical, whether to clip heatmap to Munster boundaries (default: TRUE)
#' @param basemap character, basemap type: "districts", "osm", or "none" (default: "districts")
#' @param show_landuse logical, whether to show land use overlay (default: FALSE)
#' @param show_accidents logical, whether to show individual accident points (default: TRUE)
#' @param color_scheme character, color scheme: "viridis", "plasma", "inferno", "magma", "custom" (default: "viridis")
#' @param custom_colors character vector, custom color palette when color_scheme="custom"
#' @param alpha_heatmap numeric, transparency of heatmap layer (0-1, default: 0.8)
#' @param alpha_basemap numeric, transparency of basemap layer (0-1, default: 0.3)
#' @param kernel character, kernel type: "gaussian", "epanechnikov", "quartic" (default: "gaussian")
#' @param normalize_density logical, whether to normalize density values (default: FALSE)
#' @param districts DEPRECATED - use data parameter instead
#' @param bins DEPRECATED - use grid_size parameter instead
#' @return ggplot2 object (enhanced heatmap)
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' 
#' # Basic heatmap with districts basemap (fatal and severe accidents)
#' heatmap_plot <- sero_heatmap(data$accident)
#' 
#' # Only fatal accidents with OSM basemap and custom colors
#' heatmap_plot <- sero_heatmap(data$accident, 
#'                             risk_categories = "fatal",
#'                             basemap = "osm",
#'                             color_scheme = "custom",
#'                             custom_colors = c("white", "yellow", "orange", "red", "darkred"),
#'                             bandwidth = 800, 
#'                             grid_size = 50)
#' 
#' # All accident types with plasma color scheme and clipping
#' heatmap_plot <- sero_heatmap(data$accident, 
#'                             risk_categories = c("fatal", "severe", "slight"),
#'                             color_scheme = "plasma",
#'                             clip_to_munster = TRUE,
#'                             alpha_heatmap = 0.7)
#' }
sero_heatmap <- function(accidents, 
                        risk_categories = c("fatal", "severe"),
                        risk_category_names = NULL,
                        bandwidth = 1000,
                        grid_size = 100,
                        data = NULL,
                        clip_to_munster = TRUE,
                        basemap = "districts",
                        show_landuse = FALSE,
                        show_accidents = TRUE,
                        color_scheme = "viridis",
                        custom_colors = NULL,
                        alpha_heatmap = 0.8,
                        alpha_basemap = 0.3,
                        kernel = "gaussian",
                        normalize_density = FALSE,
                        # Deprecated parameters for backward compatibility
                        districts = NULL,
                        bins = NULL) {
  # Handle deprecated parameters for backward compatibility
  if (!is.null(districts) && is.null(data)) {
    data <- list(districts = districts)
    warning("Parameter 'districts' is deprecated. Please use 'data' parameter instead.")
  }
  if (!is.null(bins)) {
    grid_size <- max(50, 5000 / bins)  # Convert bins to approximate grid size
    warning("Parameter 'bins' is deprecated. Please use 'grid_size' parameter instead.")
  }
  
  # Input validation
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  if (!"UKATEGORIE" %in% names(accidents)) {
    stop("accidents must have UKATEGORIE column")
  }
  
  # Validate basemap option
  if (!basemap %in% c("districts", "osm", "none")) {
    stop("basemap must be 'districts', 'osm', or 'none'")
  }
  
  # Validate color scheme
  if (!color_scheme %in% c("viridis", "plasma", "inferno", "magma", "custom")) {
    stop("color_scheme must be 'viridis', 'plasma', 'inferno', 'magma', or 'custom'")
  }
  
  if (color_scheme == "custom" && is.null(custom_colors)) {
    stop("custom_colors must be provided when color_scheme is 'custom'")
  }
  
  # Convert risk category names to numeric codes if needed
  if (is.character(risk_categories)) {
    name_map <- c(fatal=1, severe=2, slight=3)
    numeric_categories <- unname(name_map[risk_categories])
    if (any(is.na(numeric_categories))) {
      stop("Invalid risk_categories. Use 'fatal', 'severe', or 'slight'.")
    }
    risk_categories <- numeric_categories
  }
  
  # Handle legacy risk_category_names parameter
  if (!is.null(risk_category_names)) {
    warning("Parameter 'risk_category_names' is deprecated. Use 'risk_categories' directly with category names.")
    name_map <- c(fatal=1, severe=2, serious=2, slight=3)  # Include 'serious' for backward compatibility
    risk_categories <- unname(name_map[risk_category_names])
    if (any(is.na(risk_categories))) {
      stop("Invalid risk_category_names. Use 'fatal', 'severe', or 'slight'.")
    }
  }
  
  # Filter high-risk accidents
  high_risk_accidents <- accidents[accidents$UKATEGORIE %in% risk_categories, ]
  if (nrow(high_risk_accidents) == 0) {
    stop(paste("No accidents found for risk categories:", paste(risk_categories, collapse=", ")))
  }
  
  # Ensure projected CRS for accurate distance calculations
  if (sf::st_is_longlat(high_risk_accidents)) {
    high_risk_accidents <- sf::st_transform(high_risk_accidents, 32632)
  }
  
  # Load Munster districts for clipping if needed
  districts_data <- NULL
  if (clip_to_munster || basemap == "districts") {
    if (!is.null(data) && "districts" %in% names(data)) {
      districts_data <- data$districts
    } else {
      tryCatch({
        gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
        if (file.exists(gpkg_path)) {
          districts_data <- sf::st_read(gpkg_path, layer = "munster_districtsshp", quiet = TRUE)
        }
      }, error = function(e) {
        warning("Could not load Muenster districts: ", e$message)
      })
    }
    
    if (!is.null(districts_data) && sf::st_is_longlat(districts_data)) {
      districts_data <- sf::st_transform(districts_data, 32632)
    }
  }
  
  # Get accident coordinates
  coords <- sf::st_coordinates(high_risk_accidents)
  
  # Determine bbox - use districts if available for clipping, otherwise use accidents
  if (clip_to_munster && !is.null(districts_data)) {
    bbox <- sf::st_bbox(districts_data)
  } else {
    bbox <- sf::st_bbox(high_risk_accidents)
  }
  
  # Expand bbox slightly for better visualization
  bbox_expanded <- bbox
  bbox_expanded[1] <- bbox[1] - bandwidth  # xmin
  bbox_expanded[2] <- bbox[2] - bandwidth  # ymin
  bbox_expanded[3] <- bbox[3] + bandwidth  # xmax
  bbox_expanded[4] <- bbox[4] + bandwidth  # ymax
  
  # Create grid for heatmap
  x_seq <- seq(bbox_expanded[1], bbox_expanded[3], by = grid_size)
  y_seq <- seq(bbox_expanded[2], bbox_expanded[4], by = grid_size)
  density_grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Calculate density for each grid point (kernel options)
  density_values <- numeric(nrow(density_grid))
  kernel_fun <- switch(kernel,
    gaussian = function(d) exp(-(d^2) / (2 * bandwidth^2)),
    epanechnikov = function(d) ifelse(abs(d) <= bandwidth, 0.75 * (1 - (d/bandwidth)^2) / bandwidth, 0),
    quartic = function(d) ifelse(abs(d) <= bandwidth, (15/16) * (1 - (d/bandwidth)^2)^2 / bandwidth, 0),
    stop("Unsupported kernel type. Use 'gaussian', 'epanechnikov', or 'quartic'.")
  )
  
  for (i in seq_len(nrow(density_grid))) {
    distances <- sqrt((coords[,1] - density_grid$x[i])^2 + (coords[,2] - density_grid$y[i])^2)
    kernel_values <- kernel_fun(distances)
    density_values[i] <- sum(kernel_values)
  }
  
  # Normalize density if requested
  if (normalize_density && base::max(density_values) > 0) {
    density_values <- density_values / base::max(density_values)
  }
  
  density_grid$density <- density_values
  density_grid <- density_grid[density_grid$density > 0.01, ]
  
  if (nrow(density_grid) == 0) {
    stop("No density surface generated for selected parameters.")
  }
  
  # Convert to sf and clip to Munster if requested
  density_sf <- sf::st_as_sf(density_grid, coords = c("x", "y"), crs = 32632)
  
  if (clip_to_munster && !is.null(districts_data)) {
    # Ensure both datasets have the same CRS for clipping
    if (sf::st_crs(density_sf) != sf::st_crs(districts_data)) {
      districts_data <- sf::st_transform(districts_data, sf::st_crs(density_sf))
    }
    
    # Clip density points to Munster boundaries
    density_sf <- sf::st_filter(density_sf, districts_data)
    
    if (nrow(density_sf) == 0) {
      warning("No density points remain after clipping to Munster boundaries")
      return(ggplot2::ggplot() + 
             ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No data after clipping"), 
                               size = 5) +
             ggplot2::theme_void())
    }
  }
  
  # Transform to WGS84 for plotting
  density_wgs84 <- sf::st_transform(density_sf, 4326)
  coords_wgs84 <- sf::st_coordinates(density_wgs84)
  density_df <- data.frame(
    x = coords_wgs84[,1],
    y = coords_wgs84[,2],
    density = density_sf$density
  )
  
  accidents_wgs84 <- sf::st_transform(high_risk_accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot()
  
  # Add basemap based on selection
  if (basemap == "districts" && !is.null(districts_data)) {
    districts_wgs84 <- sf::st_transform(districts_data, 4326)
    p <- p + ggplot2::geom_sf(data = districts_wgs84, 
                             fill = "lightgray", 
                             color = "darkgray", 
                             alpha = alpha_basemap,
                             size = 0.8)
  } else if (basemap == "osm") {
    # For OSM tiles, we'll add a note that ggspatial is needed
    if (requireNamespace("ggspatial", quietly = TRUE)) {
      p <- p + ggspatial::annotation_map_tile(type = "osm", alpha = alpha_basemap)
    } else {
      warning("ggspatial package required for OSM tiles. Install with: install.packages('ggspatial')")
      # Fallback to districts if available
      if (!is.null(districts_data)) {
        districts_wgs84 <- sf::st_transform(districts_data, 4326)
        p <- p + ggplot2::geom_sf(data = districts_wgs84, 
                                 fill = "lightgray", 
                                 color = "darkgray", 
                                 alpha = alpha_basemap,
                                 size = 0.8)
      }
    }
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
  
  # Add heatmap with customizable transparency
  p <- p + ggplot2::geom_point(data = density_df,
                              ggplot2::aes(x = .data$x, y = .data$y, color = .data$density),
                              size = 1.5, alpha = alpha_heatmap)
  
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
  } else if (color_scheme == "custom") {
    p <- p + ggplot2::scale_color_gradientn(colors = custom_colors, name = "Accident\nDensity")
  } else {
    p <- p + ggplot2::scale_color_viridis_c(name = "Accident\nDensity", option = "viridis")
  }
  
  # Shape scale for accidents
  if (show_accidents) {
    p <- p + ggplot2::scale_shape_manual(values = c("1" = 4, "2" = 1, "3" = 16),
                                        name = "Accident Category",
                                        labels = c("1" = "Fatal", "2" = "Severe", "3" = "Slight"))
  }
  
  # Final styling and theme
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = ifelse(clip_to_munster, 
                    "Accident Density Heatmap - Clipped to Muenster", 
                    "Accident Density Heatmap - Muenster Area"),
      subtitle = paste("Total accidents:", nrow(high_risk_accidents),
                      "| Bandwidth:", bandwidth, "m",
                      "| Grid size:", grid_size, "m",
                      "| Basemap:", basemap),
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
