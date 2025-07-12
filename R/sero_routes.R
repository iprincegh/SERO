#' Calculate fastest routes to accident scenes
#'
#' Computes fastest routes from emergency service locations to accident scenes.
#' Can use either straight-line distance or road-based routing.
#'
#' @param locations sero_optimal_locations object or sf object with service locations
#' @param accidents sf object with accident locations
#' @param max_routes Maximum number of routes to calculate (default=10)
#' @param use_roads Logical, whether to use road network for routing (default=FALSE)
#' @param data Optional data object containing road network
#' @return sero_routes S3 object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' routes <- sero_routes(locations, data$accident)
#' plot(routes)
#' 
#' # With road-based routing
#' routes_road <- sero_routes(locations, data$accident, use_roads = TRUE, data = data)
#' }
sero_routes <- function(locations, accidents, max_routes = 10, use_roads = FALSE, data = NULL) {
  
  # Handle input types
  if (inherits(locations, "sero_optimal_locations")) {
    service_locations <- locations$locations
  } else if (inherits(locations, "sf")) {
    service_locations <- locations
  } else {
    stop("locations must be sero_optimal_locations object or sf object")
  }
  
  if (!inherits(accidents, "sf")) {
    stop("accidents must be an sf object")
  }
  
  if (nrow(service_locations) == 0 || nrow(accidents) == 0) {
    return(create_empty_routes())
  }
  
  # Ensure projected CRS for accurate distance calculations
  if (sf::st_is_longlat(service_locations)) {
    service_locations <- sf::st_transform(service_locations, 32632)
  }
  if (sf::st_is_longlat(accidents)) {
    accidents <- sf::st_transform(accidents, 32632)
  }
  
  # Calculate routes
  if (use_roads && !is.null(data)) {
    routes_data <- calculate_road_routes(service_locations, accidents, max_routes, data)
  } else {
    routes_data <- calculate_straight_routes(service_locations, accidents, max_routes)
  }
  
  # Create summary statistics
  summary_stats <- list(
    total_routes = nrow(routes_data$routes),
    avg_distance = mean(routes_data$routes$distance),
    avg_time = mean(routes_data$routes$estimated_time),
    max_distance = max(routes_data$routes$distance),
    max_time = max(routes_data$routes$estimated_time)
  )
  
  # Create S3 object
  result <- structure(
    list(
      routes = routes_data$routes,
      service_locations = service_locations,
      accidents = accidents,
      parameters = list(
        max_routes = max_routes,
        use_roads = use_roads
      ),
      summary = summary_stats,
      crs = sf::st_crs(service_locations)
    ),
    class = "sero_routes"
  )
  
  return(result)
}

#' Create empty routes object
#' @return empty sero_routes object
create_empty_routes <- function() {
  structure(
    list(
      routes = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      service_locations = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      accidents = sf::st_sf(data.frame(), geometry = sf::st_sfc()),
      parameters = list(),
      summary = list(
        total_routes = 0,
        avg_distance = 0,
        avg_time = 0,
        max_distance = 0,
        max_time = 0
      )
    ),
    class = "sero_routes"
  )
}

#' Plot method for sero_routes using ggplot2
#'
#' @param x sero_routes object
#' @param ... additional arguments (unused)
#' @return ggplot2 object
#' @export
#' @importFrom rlang .data
plot.sero_routes <- function(x, ...) {
  if (nrow(x$routes) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No routes found"), 
                             size = 5) +
           ggplot2::theme_void())
  }
  
  # Transform to WGS84 for plotting
  routes_wgs84 <- sf::st_transform(x$routes, 4326)
  locations_wgs84 <- sf::st_transform(x$service_locations, 4326)
  accidents_wgs84 <- sf::st_transform(x$accidents, 4326)
  
  # Create base plot
  p <- ggplot2::ggplot() +
    # Add routes as lines
    ggplot2::geom_sf(data = routes_wgs84,
                    ggplot2::aes(color = .data$estimated_time),
                    size = 1.2, alpha = 0.7) +
    # Add service locations
    ggplot2::geom_sf(data = locations_wgs84,
                    color = "blue", fill = "lightblue",
                    shape = 21, size = 4, alpha = 0.8) +
    # Add accident locations
    ggplot2::geom_sf(data = accidents_wgs84[seq_len(nrow(routes_wgs84)), ],
                    color = "red", size = 2, alpha = 0.8) +
    # Color scale for travel time
    ggplot2::scale_color_viridis_c(name = "Travel Time\n(minutes)", 
                                  option = "plasma", trans = "sqrt") +
    # Styling
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Emergency Response Routes",
                 subtitle = paste("Showing", x$summary$total_routes, "fastest routes"),
                 x = "Longitude", y = "Latitude") +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Print method for sero_routes
#'
#' @param x sero_routes object
#' @param ... additional arguments (unused)
#' @export
print.sero_routes <- function(x, ...) {
  cat("SERO Emergency Response Routes\n")
  cat("==============================\n\n")
  
  cat("Summary:\n")
  cat("- Total routes calculated:", x$summary$total_routes, "\n")
  cat("- Average distance:", round(x$summary$avg_distance, 0), "meters\n")
  cat("- Average travel time:", round(x$summary$avg_time, 1), "minutes\n")
  cat("- Maximum distance:", round(x$summary$max_distance, 0), "meters\n")
  cat("- Maximum travel time:", round(x$summary$max_time, 1), "minutes\n\n")
  
  cat("Parameters:\n")
  cat("- Maximum routes:", x$parameters$max_routes, "\n\n")
  
  if (x$summary$total_routes > 0) {
    cat("Route Details:\n")
    for (i in seq_len(min(5, nrow(x$routes)))) {
      cat(sprintf("  %d. Distance: %dm, Time: %.1f min\n", 
                  i, 
                  round(x$routes$distance[i]), 
                  x$routes$estimated_time[i]))
    }
    if (nrow(x$routes) > 5) {
      cat("  ... and", nrow(x$routes) - 5, "more routes\n")
    }
  }
  
  cat("\nUse plot() to visualize with ggplot2.\n")
}

#' Calculate straight-line routes
#' @param service_locations sf object with service locations
#' @param accidents sf object with accident locations  
#' @param max_routes Maximum number of routes to calculate
#' @return List with routes sf object
calculate_straight_routes <- function(service_locations, accidents, max_routes) {
  
  # Calculate distance matrix
  distance_matrix <- sf::st_distance(service_locations, accidents)
  
  # Find optimal routes (nearest service location to each accident)
  routes_data <- data.frame()
  
  for (i in seq_len(min(max_routes, nrow(accidents)))) {
    # Find closest service location for this accident
    accident_idx <- i
    service_distances <- distance_matrix[, accident_idx]
    closest_service_idx <- which.min(service_distances)
    
    # Create route data
    route_data <- data.frame(
      route_id = i,
      service_location_id = closest_service_idx,
      accident_id = accident_idx,
      distance = as.numeric(service_distances[closest_service_idx]),
      estimated_time = as.numeric(service_distances[closest_service_idx]) / 50 * 3.6 # Assume 50 km/h avg speed
    )
    
    routes_data <- rbind(routes_data, route_data)
  }
  
  # Create route geometries (straight lines)
  route_geometries <- list()
  for (i in seq_len(nrow(routes_data))) {
    service_idx <- routes_data$service_location_id[i]
    accident_idx <- routes_data$accident_id[i]
    
    # Create line between service location and accident
    service_point <- sf::st_geometry(service_locations[service_idx, ])
    accident_point <- sf::st_geometry(accidents[accident_idx, ])
    
    route_line <- sf::st_linestring(rbind(
      sf::st_coordinates(service_point),
      sf::st_coordinates(accident_point)
    ))
    
    route_geometries[[i]] <- route_line
  }
  
  # Create routes sf object
  routes_sf <- sf::st_sf(
    routes_data,
    geometry = sf::st_sfc(route_geometries, crs = sf::st_crs(service_locations))
  )
  
  return(list(routes = routes_sf))
}

#' Calculate road-based routes
#' @param service_locations sf object with service locations
#' @param accidents sf object with accident locations  
#' @param max_routes Maximum number of routes to calculate
#' @param data Data object containing road network
#' @return List with routes sf object
calculate_road_routes <- function(service_locations, accidents, max_routes, data) {
  
  # Load road network if not provided
  if (!"roads" %in% names(data)) {
    tryCatch({
      gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
      if (file.exists(gpkg_path)) {
        roads <- sf::st_read(gpkg_path, layer = "munster_roadsshp", quiet = TRUE)
        if (sf::st_is_longlat(roads)) {
          roads <- sf::st_transform(roads, 32632)
        }
        data$roads <- roads
      } else {
        warning("Road network not found, using straight-line routing")
        return(calculate_straight_routes(service_locations, accidents, max_routes))
      }
    }, error = function(e) {
      warning("Error loading road network: ", e$message, ", using straight-line routing")
      return(calculate_straight_routes(service_locations, accidents, max_routes))
    })
  }
  
  # For now, use simplified road-based routing
  # In a full implementation, this would use a proper routing algorithm
  # like Dijkstra's algorithm or A* on the road network
  
  routes_data <- data.frame()
  
  for (i in seq_len(min(max_routes, nrow(accidents)))) {
    # Find closest service location using road network distance approximation
    accident_point <- accidents[i, ]
    
    # Calculate distances to all service locations
    # For simplicity, use straight-line distance but increase by road factor
    straight_distances <- sf::st_distance(service_locations, accident_point)
    road_factor <- 1.3  # Assume roads are 30% longer than straight-line
    estimated_road_distances <- as.numeric(straight_distances) * road_factor
    
    closest_service_idx <- which.min(estimated_road_distances)
    
    # Create route data
    route_data <- data.frame(
      route_id = i,
      service_location_id = closest_service_idx,
      accident_id = i,
      distance = estimated_road_distances[closest_service_idx],
      estimated_time = estimated_road_distances[closest_service_idx] / 40 * 3.6 # Assume 40 km/h avg speed in city
    )
    
    routes_data <- rbind(routes_data, route_data)
  }
  
  # Create route geometries 
  # In a full implementation, this would trace the actual road path
  route_geometries <- list()
  for (i in seq_len(nrow(routes_data))) {
    service_idx <- routes_data$service_location_id[i]
    accident_idx <- routes_data$accident_id[i]
    
    # For now, create straight line (placeholder for actual road routing)
    service_point <- sf::st_geometry(service_locations[service_idx, ])
    accident_point <- sf::st_geometry(accidents[accident_idx, ])
    
    route_line <- sf::st_linestring(rbind(
      sf::st_coordinates(service_point),
      sf::st_coordinates(accident_point)
    ))
    
    route_geometries[[i]] <- route_line
  }
  
  # Create routes sf object
  routes_sf <- sf::st_sf(
    routes_data,
    geometry = sf::st_sfc(route_geometries, crs = sf::st_crs(service_locations))
  )
  
  return(list(routes = routes_sf))
}

#' Save optimal locations as a dataset
#'
#' Saves the computed optimal locations to a file for reuse in routing.
#' Uses GeoPackage format which is optimal for sf objects.
#'
#' @param locations sero_optimal_locations object
#' @param filename File path to save the locations (default: "optimal_locations.gpkg")
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' 
#' # Save as GeoPackage (recommended - optimal for sf objects)
#' sero_save_locations(locations, "optimal_locations.gpkg")
#' 
#' # Use custom filename
#' sero_save_locations(locations, "my_emergency_locations.gpkg")
#' }
sero_save_locations <- function(locations, filename = "optimal_locations.gpkg") {
  
  if (!inherits(locations, "sero_optimal_locations")) {
    stop("locations must be a sero_optimal_locations object")
  }
  
  if (nrow(locations$locations) == 0) {
    warning("No locations to save")
    return(invisible(FALSE))
  }
  
  # Transform to WGS84 for storage
  locations_wgs84 <- sf::st_transform(locations$locations, 4326)
  
  # Save as GeoPackage
  tryCatch({
    sf::st_write(locations_wgs84, filename, delete_dsn = TRUE, quiet = TRUE)
    cat("Optimal locations saved to:", filename, "\n")
    cat("Number of locations:", nrow(locations_wgs84), "\n")
    cat("Format: GeoPackage (.gpkg)\n")
    cat("CRS: WGS84 (EPSG:4326)\n")
    return(invisible(TRUE))
  }, error = function(e) {
    warning("Error saving locations: ", e$message)
    return(invisible(FALSE))
  })
}

#' Load optimal locations from a dataset
#'
#' Loads pre-computed optimal locations from a GeoPackage file for use in routing.
#' GeoPackage format is optimal for sf objects and preserves all spatial information.
#'
#' @param filename File path to load the locations from (must be .gpkg file)
#' @return sf object with optimal locations
#' @export
#' @examples
#' \dontrun{
#' # Load from GeoPackage
#' locations <- sero_load_locations("optimal_locations.gpkg")
#' 
#' # Use in routing
#' data <- sero_load_data()
#' routes <- sero_routes(locations, data$accident[1:5, ])
#' }
sero_load_locations <- function(filename) {
  
  if (!file.exists(filename)) {
    stop("File not found: ", filename)
  }
  
  # Check file extension
  ext <- tools::file_ext(filename)
  if (tolower(ext) != "gpkg") {
    warning("File extension is not .gpkg. Expected GeoPackage format.")
  }
  
  tryCatch({
    # Read GeoPackage
    locations <- sf::st_read(filename, quiet = TRUE)
    
    cat("Loaded", nrow(locations), "optimal locations from:", filename, "\n")
    cat("CRS:", sf::st_crs(locations)$input, "\n")
    
    # Display basic info about the locations
    if (nrow(locations) > 0) {
      cat("Location attributes:\n")
      attr_names <- names(sf::st_drop_geometry(locations))
      for (i in seq_along(attr_names)) {
        cat("  -", attr_names[i], "\n")
      }
    }
    
    return(locations)
    
  }, error = function(e) {
    stop("Error loading locations: ", e$message)
  })
}

#' Create interactive routing map (Basic version)
#'
#' Creates an interactive map where users can click to simulate accident locations
#' and see the route to the nearest optimal emergency service location.
#' This is a basic version that shows click functionality. 
#' For full OSRM routing, use sero_launch_interactive().
#'
#' @param data Data object containing spatial layers
#' @param optimal_locations sf object with optimal locations (or path to file)
#' @param use_roads Logical, whether to use road-based routing (default=TRUE)
#' @return Interactive leaflet map
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' sero_interactive_routing(data, locations)
#' }
sero_interactive_routing <- function(data, optimal_locations, use_roads = TRUE) {
  
  # Check if leaflet is available
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package is required for interactive routing. Please install it with: install.packages('leaflet')")
  }
  
  # Handle optimal locations input
  if (inherits(optimal_locations, "sero_optimal_locations")) {
    locations_sf <- optimal_locations$locations
  } else if (inherits(optimal_locations, "sf")) {
    locations_sf <- optimal_locations
  } else if (is.character(optimal_locations)) {
    locations_sf <- sero_load_locations(optimal_locations)
  } else {
    stop("optimal_locations must be sf object, sero_optimal_locations object, or file path")
  }
  
  # Transform to WGS84 for leaflet
  locations_wgs84 <- sf::st_transform(locations_sf, 4326)
  
  # Load districts for basemap
  districts_wgs84 <- NULL
  if ("districts" %in% names(data)) {
    districts_wgs84 <- sf::st_transform(data$districts, 4326)
  } else {
    tryCatch({
      gpkg_path <- system.file("gpkg", "dataset.gpkg", package = "SERO")
      if (file.exists(gpkg_path)) {
        districts <- sf::st_read(gpkg_path, layer = "munster_districtsshp", quiet = TRUE)
        districts_wgs84 <- sf::st_transform(districts, 4326)
      }
    }, error = function(e) {
      warning("Could not load districts: ", e$message)
    })
  }
  
  # Create base map
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView(lng = 7.628, lat = 51.961, zoom = 12)
  
  # Add districts
  if (!is.null(districts_wgs84)) {
    m <- m %>%
      leaflet::addPolygons(
        data = districts_wgs84,
        fillColor = "lightblue",
        color = "blue",
        weight = 2,
        opacity = 0.7,
        fillOpacity = 0.2,
        group = "Districts"
      )
  }
  
  # Add optimal locations
  m <- m %>%
    leaflet::addCircleMarkers(
      data = locations_wgs84,
      radius = 8,
      color = "red",
      fillColor = "red",
      fillOpacity = 0.8,
      popup = ~paste("Optimal Location", seq_along(locations_wgs84[[1]])),
      group = "Optimal Locations"
    )
  
  # Add click event for routing
  m <- m %>%
    leaflet::addLayersControl(
      overlayGroups = c("Districts", "Optimal Locations"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        map.on('click', function(e) {
          var lat = e.latlng.lat;
          var lng = e.latlng.lng;
          
          // Add accident marker
          L.marker([lat, lng], {
            icon: L.icon({
              iconUrl: 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png',
              shadowUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png',
              iconSize: [25, 41],
              iconAnchor: [12, 41],
              popupAnchor: [1, -34],
              shadowSize: [41, 41]
            })
          }).addTo(map)
          .bindPopup('Accident Location<br/>Use sero_launch_interactive() for full routing')
          .openPopup();
        });
      }
    ")
  
  return(m)
}

#' Launch interactive routing application with OSRM
#'
#' Creates a Shiny app with interactive map where users can click to simulate 
#' accident locations and see the actual road route to the nearest optimal 
#' emergency service location using OSRM routing.
#'
#' @param optimal_locations sf object with optimal locations (must be in WGS84)
#' @param roads Optional sf object with road network for background display
#' @param landuse Optional sf object with landuse polygons for background display
#' @param population Optional sf object with population data for background display
#' @param osrm_server OSRM server URL (default uses public server with usage limits)
#' @return Shiny app object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' 
#' # Launch interactive app with OSRM routing
#' sero_launch_interactive(locations$locations)
#' 
#' # With background layers
#' sero_launch_interactive(locations$locations, 
#'                        roads = data$roads, 
#'                        landuse = data$landuse)
#' }
sero_launch_interactive <- function(optimal_locations, 
                                   roads = NULL, 
                                   landuse = NULL, 
                                   population = NULL,
                                   osrm_server = "http://router.project-osrm.org") {
  
  # Check required packages
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required for interactive routing. Please install it with: install.packages('shiny')")
  }
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package is required for interactive routing. Please install it with: install.packages('leaflet')")
  }
  
  if (!requireNamespace("osrm", quietly = TRUE)) {
    stop("osrm package is required for routing. Please install it with: install.packages('osrm')")
  }
  
  # Validate optimal locations
  if (!inherits(optimal_locations, "sf")) {
    stop("optimal_locations must be an sf object")
  }
  
  # Ensure locations are in WGS84
  if (sf::st_crs(optimal_locations)$epsg != 4326) {
    optimal_locations <- sf::st_transform(optimal_locations, 4326)
  }
  
  # Set OSRM server
  options(osrm.server = osrm_server)
  
  # Create Shiny UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("SERO Interactive Emergency Routing"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(12,
        shiny::p("Click on the map to simulate an accident location. The system will calculate the route to the nearest optimal emergency service location."),
        shiny::p(shiny::strong("Note:"), "This uses the public OSRM server which has usage limits. For production use, set up your own OSRM server.")
      )
    ),
    shiny::fluidRow(
      shiny::column(8,
        leaflet::leafletOutput("map", height = "600px")
      ),
      shiny::column(4,
        shiny::h4("Route Information"),
        shiny::verbatimTextOutput("route_info"),
        shiny::br(),
        shiny::h5("Instructions:"),
        shiny::tags$ul(
          shiny::tags$li("Red circles = Optimal emergency locations"),
          shiny::tags$li("Click anywhere to simulate accident"),
          shiny::tags$li("Blue line = Computed route"),
          shiny::tags$li("Green marker = Accident location")
        )
      )
    )
  )
  
  # Create Shiny Server
  server <- function(input, output, session) {
    
    # Reactive values for storing route data
    route_data <- shiny::reactiveValues(
      current_route = NULL,
      current_accident = NULL,
      current_service = NULL
    )
    
    # Render the base map
    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 7.628, lat = 51.961, zoom = 12)
      
      # Add background layers if provided
      if (!is.null(roads)) {
        roads_wgs84 <- sf::st_transform(roads, 4326)
        m <- m %>%
          leaflet::addPolylines(
            data = roads_wgs84,
            color = "gray",
            weight = 1,
            opacity = 0.5,
            group = "Roads"
          )
      }
      
      if (!is.null(landuse)) {
        landuse_wgs84 <- sf::st_transform(landuse, 4326)
        m <- m %>%
          leaflet::addPolygons(
            data = landuse_wgs84,
            fillColor = "green",
            fillOpacity = 0.1,
            color = "darkgreen",
            weight = 0.5,
            opacity = 0.3,
            group = "Landuse"
          )
      }
      
      # Add optimal locations
      m <- m %>%
        leaflet::addCircleMarkers(
          data = optimal_locations,
          radius = 8,
          color = "red",
          fillColor = "red",
          fillOpacity = 0.8,
          weight = 2,
          popup = ~paste("Optimal Location", seq_len(nrow(optimal_locations))),
          group = "Emergency Locations"
        )
      
      # Add layer control
      overlay_groups <- c("Emergency Locations")
      if (!is.null(roads)) overlay_groups <- c(overlay_groups, "Roads")
      if (!is.null(landuse)) overlay_groups <- c(overlay_groups, "Landuse")
      
      m %>%
        leaflet::addLayersControl(
          overlayGroups = overlay_groups,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })
    
    # Handle map clicks
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      
      tryCatch({
        # Create accident point
        accident_coords <- c(click$lng, click$lat)
        accident_point <- sf::st_sf(
          id = 1,
          geometry = sf::st_sfc(sf::st_point(accident_coords), crs = 4326)
        )
        
        # Find nearest optimal location
        nearest_idx <- sf::st_nearest_feature(accident_point, optimal_locations)
        service_location <- optimal_locations[nearest_idx, ]
        
        # Get coordinates for OSRM
        service_coords <- sf::st_coordinates(service_location)
        accident_coords_df <- data.frame(
          lon = click$lng,
          lat = click$lat
        )
        service_coords_df <- data.frame(
          lon = service_coords[1, 1],
          lat = service_coords[1, 2]
        )
        
        # Calculate route using OSRM
        route <- osrm::osrmRoute(
          src = service_coords_df,
          dst = accident_coords_df,
          returnclass = "sf"
        )
        
        # Store route data
        route_data$current_route <- route
        route_data$current_accident <- accident_point
        route_data$current_service <- service_location
        
        # Update map
        leaflet::leafletProxy("map") %>%
          leaflet::clearGroup("Route") %>%
          leaflet::clearGroup("Accident") %>%
          leaflet::addPolylines(
            data = route,
            color = "blue",
            weight = 4,
            opacity = 0.8,
            group = "Route"
          ) %>%
          leaflet::addMarkers(
            lng = click$lng,
            lat = click$lat,
            popup = paste("Accident Location<br/>",
                         "Distance:", round(route$distance, 2), "km<br/>",
                         "Duration:", round(route$duration, 1), "minutes"),
            group = "Accident"
          )
        
      }, error = function(e) {
        # Handle OSRM errors (e.g., server unavailable, no route found)
        shiny::showNotification(
          paste("Routing error:", e$message, 
                "\nTry clicking closer to roads or check OSRM server availability."),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Update route information
    output$route_info <- shiny::renderText({
      if (is.null(route_data$current_route)) {
        "Click on the map to simulate an accident and calculate route."
      } else {
        route <- route_data$current_route
        service_id <- if (is.null(route_data$current_service$id)) "Unknown" else route_data$current_service$id
        paste(
          "Route calculated successfully!\n\n",
          "Distance:", round(route$distance, 2), "km\n",
          "Duration:", round(route$duration, 1), "minutes\n",
          "Average Speed:", round(route$distance / (route$duration / 60), 1), "km/h\n\n",
          "Nearest Service Location:", service_id
        )
      }
    })
  }
  
  # Launch the Shiny app
  shiny::shinyApp(ui = ui, server = server)
}

#' Route to nearest optimal location using OSRM
#'
#' Calculate actual road-based route from a point to the nearest optimal emergency location
#' using OSRM routing service.
#'
#' @param lat Latitude of accident location
#' @param lng Longitude of accident location  
#' @param optimal_locations sf object with optimal locations (must be in WGS84)
#' @param osrm_server OSRM server URL (default uses public server)
#' @return sf object with route linestring and metadata
#' @export
#' @examples
#' \dontrun{
#' data <- sero_load_data()
#' locations <- sero_optimal(data)
#' 
#' # Calculate route from a point to nearest optimal location
#' route <- sero_route_osrm(51.9606, 7.6261, locations$locations)
#' plot(route)
#' }
sero_route_osrm <- function(lat, lng, optimal_locations, osrm_server = "http://router.project-osrm.org") {
  
  # Check required packages
  if (!requireNamespace("osrm", quietly = TRUE)) {
    stop("osrm package is required for routing. Please install it with: install.packages('osrm')")
  }
  
  # Validate inputs
  if (!inherits(optimal_locations, "sf")) {
    stop("optimal_locations must be an sf object")
  }
  
  # Ensure locations are in WGS84
  if (sf::st_crs(optimal_locations)$epsg != 4326) {
    optimal_locations <- sf::st_transform(optimal_locations, 4326)
  }
  
  # Set OSRM server
  options(osrm.server = osrm_server)
  
  # Create accident point
  accident_point <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
  )
  
  # Find nearest optimal location
  nearest_idx <- sf::st_nearest_feature(accident_point, optimal_locations)
  service_location <- optimal_locations[nearest_idx, ]
  
  # Get coordinates for OSRM
  service_coords <- sf::st_coordinates(service_location)
  accident_coords_df <- data.frame(lon = lng, lat = lat)
  service_coords_df <- data.frame(
    lon = service_coords[1, 1],
    lat = service_coords[1, 2]
  )
  
  # Calculate route using OSRM
  route <- osrm::osrmRoute(
    src = service_coords_df,
    dst = accident_coords_df,
    returnclass = "sf"
  )
  
  # Add metadata
  route$service_location_id <- nearest_idx
  route$accident_lat <- lat
  route$accident_lng <- lng
  
  return(route)
}
