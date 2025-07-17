#' Load SERO spatial data
#'
#' Load the built-in Munster dataset containing accident, road, landuse, 
#' population, and district data for emergency response analysis.
#'
#' @return list containing spatial data layers:
#' \describe{
#'   \item{accident}{sf object with accident locations}
#'   \item{roads}{sf object with road network}
#'   \item{landuse}{sf object with land use areas}
#'   \item{population}{sf object with population areas}
#'   \item{districts}{sf object with district boundaries}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Load the dataset
#' data <- sero_load_data()
#' 
#' # Explore the data structure
#' str(data)
#' 
#' # Check accident data
#' head(data$accident)
#' }
sero_load_data <- function() {
  # Load the built-in data
  data_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
  
  if (!file.exists(data_path)) {
    stop("Dataset not found. Please ensure the SERO package is properly installed.")
  }
  
  # Load all layers from the GeoPackage
  tryCatch({
    layers <- sf::st_layers(data_path)
    
    data <- list()
    
    # Load each layer
    for (layer in layers$name) {
      cat("Loading", layer, "...\n")
      data[[layer]] <- sf::st_read(data_path, layer = layer, quiet = TRUE)
    }
    
    cat("Data loaded successfully!\n")
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
