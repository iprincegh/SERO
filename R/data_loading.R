#' Load built-in SERO spatial data
#'
#' Loads the built-in spatial data from the inst/gpkg folder.
#' This data is used for all SERO analysis functions which implement
#' 6 multi-criteria analysis criteria:
#' 1. High-Risk Accidents (UKATEGORIE = 1 or 2)
#' 2. Suitable Land Use (residential, commercial, industrial)
#' 3. Proximity to Roads (500m-1000m from roads)
#' 4. Accident Density (100m grid cells)
#' 5. Population Density (higher density areas prioritized)
#' 6. Centroid Calculation (ST_Centroid equivalent)
#'
#' @return List of sf objects containing spatial data
#' @export
#' @examples
#' \dontrun{
#' # Load built-in data
#' data <- sero_load_data()
#' 
#' # View available layers
#' print(names(data))
#' }
sero_load_data <- function() {
  
  # Get path to built-in data
  gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
  
  if (!file.exists(gpkg_path)) {
    stop("Built-in dataset not found. Please check package installation.")
  }
  
  # Load all layers from GeoPackage
  layers <- sf::st_layers(gpkg_path)$name
  
  data_list <- list()
  for (layer in layers) {
    data_list[[layer]] <- sf::st_read(gpkg_path, layer = layer, quiet = TRUE)
  }
  
  # Standardize layer names and validate
  standardized_data <- sero_validate_data(data_list)
  
  return(standardized_data)
}

#' Validate and standardize spatial data for SERO analysis
#'
#' @param data_list List of sf objects
#' @return List of validated sf objects with standardized names
#' @export
sero_validate_data <- function(data_list) {
  
  if (!is.list(data_list)) {
    stop("Input must be a list of sf objects")
  }
  
  # Define expected layer mappings based on the actual data structure
  layer_mapping <- list(
    accident = c("accidents", "unfaelle", "crash"),
    landuse = c("munster_landuseshp", "landuse", "land_use"),
    population = c("population_density", "population", "pop_density"),
    roads = c("munster_roadsshp", "roads", "road", "strassen"),
    districts = c("munster_districtsshp", "districts", "bezirke")
  )
  
  # Find and map layers
  result <- list()
  
  for (target_name in names(layer_mapping)) {
    candidates <- layer_mapping[[target_name]]
    found_layer <- NULL
    
    for (candidate in candidates) {
      if (candidate %in% names(data_list)) {
        found_layer <- candidate
        break
      }
    }
    
    if (!is.null(found_layer)) {
      result[[target_name]] <- data_list[[found_layer]]
    }
  }
  
  # Validate required layers
  required_layers <- c("accident", "roads")
  missing_layers <- setdiff(required_layers, names(result))
  
  if (length(missing_layers) > 0) {
    stop("Missing required layers: ", paste(missing_layers, collapse = ", "))
  }
  
  # Validate accident data
  if (!"UKATEGORIE" %in% names(result$accident)) {
    # Try to find accident severity column
    severity_candidates <- c("severity", "category", "kategorie", "type")
    severity_col <- NULL
    
    for (candidate in severity_candidates) {
      if (candidate %in% names(result$accident)) {
        severity_col <- candidate
        break
      }
    }
    
    if (!is.null(severity_col)) {
      names(result$accident)[names(result$accident) == severity_col] <- "UKATEGORIE"
      message("Renamed '", severity_col, "' to 'UKATEGORIE' in accident data")
    } else {
      stop("Accident data must have UKATEGORIE column (or severity/category column)")
    }
  }
  
  # Ensure valid geometries for all layers
  for (layer_name in names(result)) {
    if (!all(sf::st_is_valid(result[[layer_name]]))) {
      result[[layer_name]] <- sf::st_make_valid(result[[layer_name]])
      message("Fixed invalid geometries in ", layer_name, " data")
    }
  }
  
  # Validate landuse data if present
  if ("landuse" %in% names(result)) {
    if (!"fclass" %in% names(result$landuse)) {
      # Try to find land use classification column
      fclass_candidates <- c("class", "type", "category", "landuse", "use")
      fclass_col <- NULL
      
      for (candidate in fclass_candidates) {
        if (candidate %in% names(result$landuse)) {
          fclass_col <- candidate
          break
        }
      }
      
      if (!is.null(fclass_col)) {
        names(result$landuse)[names(result$landuse) == fclass_col] <- "fclass"
        message("Renamed '", fclass_col, "' to 'fclass' in landuse data")
      }
    }
  }
  
  # Validate population data if present
  if ("population" %in% names(result)) {
    if (!"density" %in% names(result$population)) {
      # Try to find population density column
      density_candidates <- c("pop_density", "population", "density", "pop", "inhabitants")
      density_col <- NULL
      
      for (candidate in density_candidates) {
        if (candidate %in% names(result$population)) {
          density_col <- candidate
          break
        }
      }
      
      if (!is.null(density_col)) {
        names(result$population)[names(result$population) == density_col] <- "density"
        message("Renamed '", density_col, "' to 'density' in population data")
      }
    }
  }
  
  return(result)
}
