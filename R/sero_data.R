#' Load SERO spatial data
#'
#' Load the built-in Munster dataset containing accident, road, landuse, 
#' population, and district data for emergency response analysis.
#' 
#' Data is optimized for vector-based spatial analysis using sf objects.
#' All layers are validated, standardized for consistent CRS, and optimized
#' for emergency response operations (distance calculations, routing, coverage analysis).
#'
#' @param layers character vector, specific layers to load (default: all)
#' @param crs numeric or character, target CRS for all layers (default: UTM 32N for precise distance calculations)
#' @param quiet logical, suppress loading messages (default: FALSE)
#' @param validate logical, validate data structure (default: TRUE)
#' @param optimize_for character, optimize for specific operations: "distance", "routing", "coverage" (default: "distance")
#' @return list containing optimized sf objects:
#' \describe{
#'   \item{accident}{sf POINT object with accident locations}
#'   \item{roads}{sf MULTILINESTRING object with road network}
#'   \item{landuse}{sf MULTIPOLYGON object with land use areas}
#'   \item{population}{sf POLYGON object with population areas}
#'   \item{districts}{sf MULTIPOLYGON object with district boundaries}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Load optimized for distance calculations (default)
#' data <- sero_load_data()
#' 
#' # Load specific layers only
#' data <- sero_load_data(layers = c("accident", "districts"))
#' 
#' # Load with specific CRS
#' data <- sero_load_data(crs = 4326)  # WGS84
#' 
#' # Optimize for routing operations
#' data <- sero_load_data(optimize_for = "routing")
#' 
#' # Check data structure
#' str(data)
#' }
sero_load_data <- function(layers = NULL, crs = 25832, quiet = FALSE, validate = TRUE, optimize_for = "distance") {
  # Load the built-in data
  data_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
  
  if (!file.exists(data_path)) {
    stop("Dataset not found. Please ensure the SERO package is properly installed.")
  }
  
  # Load all layers from the GeoPackage
  tryCatch({
    available_layers <- sf::st_layers(data_path)
    
    # Filter layers if specified
    if (!is.null(layers)) {
      # Map user-friendly names to file names
      layer_mapping <- c(
        "accident" = "accidents",
        "districts" = "munster_districtsshp",
        "population" = "population_density",
        "roads" = "munster_roadsshp",
        "landuse" = "munster_landuseshp"
      )
      
      # Convert requested layers to file names
      file_layers <- character()
      for (layer in layers) {
        if (layer %in% names(layer_mapping)) {
          file_layers <- c(file_layers, layer_mapping[layer])
        } else if (layer %in% available_layers$name) {
          file_layers <- c(file_layers, layer)
        } else {
          warning("Layer '", layer, "' not found. Available layers: ", 
                  paste(names(layer_mapping), collapse = ", "))
        }
      }
      
      if (length(file_layers) == 0) {
        stop("No valid layers specified")
      }
      
      available_layers$name <- file_layers
    }
    
    # Set target CRS for emergency response optimization
    target_crs <- sf::st_crs(crs)
    if (is.na(target_crs) || is.null(target_crs)) {
      target_crs <- sf::st_crs(25832)  # Default: UTM 32N for Germany
      if (!quiet) cat("Using default CRS: UTM 32N (EPSG:25832) for precise distance calculations\n")
    }
    
    data <- list()
    
    # Load each layer with vector optimization
    for (layer in available_layers$name) {
      if (!quiet) cat("Loading", layer, "...\n")
      
      # Load layer
      layer_data <- sf::st_read(data_path, layer = layer, quiet = TRUE)
      
      # Transform to consistent CRS for precise calculations
      layer_data <- sf::st_transform(layer_data, target_crs)
      
      # Optimize geometry for vector operations
      layer_data <- sf::st_make_valid(layer_data)
      
      # Apply optimization based on use case
      if (optimize_for == "distance") {
        # Optimize for distance calculations
        if (sf::st_geometry_type(layer_data)[1] %in% c("POINT", "MULTIPOINT")) {
          # For points, ensure coordinates are accessible
          layer_data <- sf::st_cast(layer_data, "POINT")
        }
      } else if (optimize_for == "routing") {
        # Optimize for routing operations
        if (sf::st_geometry_type(layer_data)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
          # For roads, ensure proper network topology
          layer_data <- sf::st_cast(layer_data, "LINESTRING")
        }
      } else if (optimize_for == "coverage") {
        # Optimize for coverage analysis
        if (sf::st_geometry_type(layer_data)[1] %in% c("POLYGON", "MULTIPOLYGON")) {
          # For polygons, ensure proper topology
          layer_data <- sf::st_make_valid(layer_data)
        }
      }
      
      # Map layer names to expected names
      if (layer == "accidents") {
        data$accident <- layer_data
      } else if (layer == "munster_districtsshp") {
        data$districts <- layer_data
      } else if (layer == "population_density") {
        data$population <- layer_data
      } else if (layer == "munster_roadsshp") {
        data$roads <- layer_data
      } else if (layer == "munster_landuseshp") {
        data$landuse <- layer_data
      } else {
        # Keep original name for other layers
        data[[layer]] <- layer_data
      }
    }
    
    # Validate data structure if requested
    if (validate) {
      validation_result <- sero_validate_data(data)
      if (!validation_result) {
        warning("Data validation failed. Some functions may not work properly.")
      }
    }
    
    # Add metadata optimized for vector operations
    attr(data, "loaded_at") <- Sys.time()
    attr(data, "data_path") <- data_path
    attr(data, "layers") <- names(data)
    attr(data, "crs") <- target_crs
    attr(data, "optimize_for") <- optimize_for
    attr(data, "approach") <- "vector"
    attr(data, "total_features") <- sum(sapply(data, nrow))
    
    if (!quiet) {
      cat("Data loaded successfully! (", length(data), " layers)\n")
      cat("Optimized for:", optimize_for, "operations using vector approach\n")
      cat("Total features:", sum(sapply(data, nrow)), "\n")
      cat("Target CRS:", target_crs$input, "\n")
    }
    
    return(data)
    
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

#' Validate SERO data structure
#'
#' Validate that the loaded data contains the required spatial layers
#' and has the correct structure for SERO functions.
#'
#' @param data list containing spatial data layers
#' @return logical indicating if data is valid
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' is_valid <- sero_validate_data(data)
#' }
sero_validate_data <- function(data) {
  # Check if data is a list
  if (!is.list(data)) {
    warning("Data must be a list")
    return(FALSE)
  }
  
  # Check required layers
  required_layers <- c("accident", "districts")
  missing_layers <- setdiff(required_layers, names(data))
  
  if (length(missing_layers) > 0) {
    warning("Missing required layers: ", paste(missing_layers, collapse = ", "))
    return(FALSE)
  }
  
  # Check if layers are sf objects
  for (layer in names(data)) {
    if (!inherits(data[[layer]], "sf")) {
      warning("Layer '", layer, "' is not an sf object")
      return(FALSE)
    }
  }
  
  # Check accident data structure
  if (!"UKATEGORIE" %in% names(data$accident)) {
    warning("Accident data missing UKATEGORIE column")
    return(FALSE)
  }
  
  return(TRUE)
}
