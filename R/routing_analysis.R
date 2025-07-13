#' Complete Interactive Emergency Routing System
#'
#' Professional interactive routing with OSRM integration and real-time accident simulation

#' Interactive emergency routing system with professional features
#'
#' @param optimal_locs sf object of optimal emergency service locations
#' @param roads Road network sf object
#' @param districts District boundaries sf object
#' @param landuse Landuse data sf object (optional)
#' @param accidents Historical accidents sf object (optional)
#' @return Shiny app for interactive routing
#' @importFrom shiny fluidPage absolutePanel wellPanel h4 actionButton checkboxInput hr htmlOutput renderUI observeEvent reactive reactiveValues tags HTML
#' @importFrom leaflet leaflet addTiles addProviderTiles addPolygons addPolylines addCircleMarkers addMarkers makeIcon clearGroup fitBounds leafletProxy renderLeaflet
#' @importFrom sf st_transform st_bbox st_nearest_feature st_as_sf st_sfc st_point st_coordinates
#' @export
sero_interactive_routing <- function(optimal_locs, roads, districts, landuse = NULL, accidents = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for interactive routing")
  }
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive routing")
  }
  
  # Transform all data to WGS84
  optimal_locs <- sf::st_transform(optimal_locs, 4326)
  roads <- sf::st_transform(roads, 4326)
  districts <- sf::st_transform(districts, 4326)
  if (!is.null(landuse)) landuse <- sf::st_transform(landuse, 4326)
  if (!is.null(accidents)) accidents <- sf::st_transform(accidents, 4326)
  
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .leaflet-container { cursor: crosshair !important; }
        .control-panel { background-color: rgba(255,255,255,0.9); border-radius: 5px; }
      "))
    ),
    leaflet::leafletOutput("map", height = "90vh"),
    shiny::absolutePanel(
      top = 10, right = 10, width = 300,
      class = "control-panel",
      shiny::wellPanel(
        shiny::h4("🚑 Emergency Response Routing", style = "color: #0066CC; margin-bottom: 15px;"),
        shiny::p("Click anywhere on the map to simulate an accident location", 
                style = "font-style: italic; color: #666;"),
        shiny::hr(),
        shiny::actionButton("reset", "🔄 Reset Map", class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
        shiny::checkboxInput("show_landuse", "🌳 Show Landuse", value = FALSE),
        shiny::checkboxInput("show_accidents", "⚠️ Show Historical Accidents", value = TRUE),
        shiny::checkboxInput("show_roads", "🛣️ Show Road Network", value = FALSE),
        shiny::hr(),
        shiny::htmlOutput("route_info"),
        shiny::hr(),
        shiny::htmlOutput("system_info")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Reactive values for storing state
    values <- shiny::reactiveValues(
      accident_location = NULL,
      current_route = NULL,
      nearest_base = NULL,
      route_stats = NULL
    )
    
    # Initialize base map
    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Light", 
                                 options = leaflet::providerTileOptions(opacity = 0.9)) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
          overlayGroups = c("Districts", "Emergency Bases", "Roads", "Landuse", "Historical Accidents"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
      
      # Add district boundaries
      m <- m %>%
        leaflet::addPolygons(
          data = districts,
          fillColor = "transparent",
          color = "#2E4057",
          weight = 2,
          opacity = 0.8,
          group = "Districts",
          popup = ~ifelse("name" %in% names(districts), paste("District:", name), "District boundary")
        )
      
      # Add emergency bases with professional styling
      m <- m %>%
        leaflet::addCircleMarkers(
          data = optimal_locs,
          radius = 8,
          color = "#0066CC",
          fillColor = "#4A90E2",
          fillOpacity = 0.9,
          stroke = TRUE,
          weight = 2,
          group = "Emergency Bases",
          popup = ~paste(
            "<b>🚑 Emergency Base:</b>", location_id, "<br>",
            if("accident_count_500m" %in% names(optimal_locs)) paste("<b>Coverage (500m):</b>", accident_count_500m, "accidents<br>") else "",
            if("accessibility_score" %in% names(optimal_locs)) paste("<b>Accessibility Score:</b>", round(accessibility_score, 2)) else ""
          ),
          label = ~paste("Base", location_id),
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top", 
                                              style = list("font-weight" = "bold", "color" = "#0066CC"))
        )
      
      # Center map on districts
      bbox <- sf::st_bbox(districts)
      m %>% leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    })
    
    # Handle map clicks for accident simulation
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      
      # Create accident location
      values$accident_location <- sf::st_sfc(
        sf::st_point(c(click$lng, click$lat)),
        crs = 4326
      )
      
      # Find nearest emergency base
      nearest_base_idx <- sf::st_nearest_feature(
        values$accident_location,
        optimal_locs
      )
      values$nearest_base <- optimal_locs[nearest_base_idx, ]
      
      # Calculate route (using euclidean distance as OSRM fallback)
      tryCatch({
        if (requireNamespace("osrm", quietly = TRUE)) {
          # Try OSRM routing
          route <- osrm::osrmRoute(
            src = sf::st_as_sf(data.frame(
              id = "accident",
              lon = click$lng,
              lat = click$lat
            ), coords = c("lon", "lat"), crs = 4326),
            dst = values$nearest_base,
            returnclass = "sf",
            overview = "full"
          )
          
          values$route_stats <- list(
            distance_km = round(route$distance, 2),
            duration_min = round(route$duration, 1),
            method = "OSRM (Road Network)"
          )
        } else {
          # Fallback to euclidean distance
          route <- create_euclidean_route(values$accident_location, values$nearest_base)
          values$route_stats <- route$stats
        }
        
        values$current_route <- route
        
      }, error = function(e) {
        # Fallback route calculation
        route <- create_euclidean_route(values$accident_location, values$nearest_base)
        values$current_route <- route$geometry
        values$route_stats <- route$stats
      })
      
      # Update map with accident and route
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("simulation") %>%
        # Add accident marker
        leaflet::addMarkers(
          lng = click$lng, lat = click$lat,
          group = "simulation",
          icon = leaflet::makeIcon(
            iconUrl = "https://cdn-icons-png.flaticon.com/512/564/564619.png",
            iconWidth = 30, iconHeight = 30,
            iconAnchorX = 15, iconAnchorY = 30
          ),
          popup = "🚨 <b>Simulated Accident</b><br>Emergency response dispatched!",
          label = "Accident Location"
        ) %>%
        # Highlight dispatched base
        leaflet::addCircleMarkers(
          data = values$nearest_base,
          radius = 12,
          color = "#FF6B35",
          fillColor = "#FF8C42",
          fillOpacity = 1,
          stroke = TRUE,
          weight = 3,
          group = "simulation",
          popup = ~paste("🚑 <b>Dispatched Base:</b>", location_id),
          label = "Dispatched Emergency Base"
        ) %>%
        # Add route line
        leaflet::addPolylines(
          data = values$current_route,
          color = "#FF3333",
          weight = 4,
          opacity = 0.8,
          group = "simulation",
          popup = paste("🚨 Emergency Route<br>",
                       "Distance:", values$route_stats$distance_km, "km<br>",
                       "Est. Time:", values$route_stats$duration_min, "min")
        )
    })
    
    # Reset functionality
    shiny::observeEvent(input$reset, {
      values$accident_location <- NULL
      values$current_route <- NULL
      values$nearest_base <- NULL
      values$route_stats <- NULL
      
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("simulation")
    })
    
    # Toggle landuse layer
    shiny::observeEvent(input$show_landuse, {
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("landuse_layer")
      
      if (input$show_landuse && !is.null(landuse)) {
        leaflet::leafletProxy("map") %>%
          leaflet::addPolygons(
            data = landuse,
            group = "landuse_layer",
            fillColor = "#90EE90",
            fillOpacity = 0.3,
            color = "#228B22",
            weight = 0.5,
            popup = ~ifelse("type" %in% names(landuse), paste("Landuse:", type), "Landuse area")
          )
      }
    })
    
    # Toggle historical accidents
    shiny::observeEvent(input$show_accidents, {
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("historical_accidents")
      
      if (input$show_accidents && !is.null(accidents)) {
        # Sample accidents to avoid performance issues
        accidents_sample <- if (nrow(accidents) > 1000) {
          accidents[sample(nrow(accidents), 1000), ]
        } else {
          accidents
        }
        
        leaflet::leafletProxy("map") %>%
          leaflet::addCircleMarkers(
            data = accidents_sample,
            radius = 2,
            color = "#FF6B6B",
            fillColor = "#FF8A80",
            fillOpacity = 0.6,
            stroke = FALSE,
            group = "historical_accidents",
            popup = "Historical accident location"
          )
      }
    })
    
    # Toggle road network
    shiny::observeEvent(input$show_roads, {
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("road_network")
      
      if (input$show_roads) {
        # Sample roads to avoid performance issues
        roads_sample <- if (nrow(roads) > 1500) {
          roads[sample(nrow(roads), 1500), ]
        } else {
          roads
        }
        
        leaflet::leafletProxy("map") %>%
          leaflet::addPolylines(
            data = roads_sample,
            color = "#888888",
            opacity = 0.5,
            weight = 1,
            group = "road_network"
          )
      }
    })
    
    # Route information display
    output$route_info <- shiny::renderUI({
      if (!is.null(values$route_stats) && !is.null(values$nearest_base)) {
        shiny::HTML(paste(
          "<h5 style='color: #FF3333; margin-bottom: 10px;'>🚨 Active Emergency Response</h5>",
          "<div style='background-color: #FFE5E5; padding: 10px; border-radius: 5px; border-left: 4px solid #FF3333;'>",
          "<b>Dispatched Base:</b>", values$nearest_base$location_id, "<br>",
          "<b>Route Distance:</b>", values$route_stats$distance_km, " km<br>",
          "<b>Estimated Time:</b>", values$route_stats$duration_min, " minutes<br>",
          "<b>Routing Method:</b>", values$route_stats$method,
          "</div>"
        ))
      } else {
        shiny::HTML("<p style='color: #666; font-style: italic;'>Click on the map to simulate an emergency and dispatch the nearest response unit.</p>")
      }
    })
    
    # System information
    output$system_info <- shiny::renderUI({
      shiny::HTML(paste(
        "<h6 style='color: #0066CC;'>📊 System Status</h6>",
        "<div style='font-size: 12px; color: #666;'>",
        "<b>Emergency Bases:</b>", nrow(optimal_locs), "<br>",
        "<b>Districts Covered:</b>", nrow(districts), "<br>",
        if (!is.null(accidents)) paste("<b>Historical Accidents:</b>", nrow(accidents), "<br>") else "",
        "<b>OSRM Routing:</b>", ifelse(requireNamespace("osrm", quietly = TRUE), "✅ Available", "❌ Fallback Mode"),
        "</div>"
      ))
    })
  }
  
  # Launch the Shiny app
  shiny::shinyApp(ui, server)
}

#' Create euclidean route as fallback
#' @keywords internal
create_euclidean_route <- function(accident_location, nearest_base) {
  
  # Calculate straight line distance
  distance_m <- as.numeric(sf::st_distance(accident_location, nearest_base))
  distance_km <- round(distance_m / 1000, 2)
  
  # Estimate travel time (assuming 50 km/h average speed with road factor)
  duration_min <- round((distance_km * 1.3) / 50 * 60, 1)
  
  # Create route geometry
  coords <- rbind(
    sf::st_coordinates(accident_location),
    sf::st_coordinates(nearest_base)
  )
  
  route_geometry <- sf::st_sfc(
    sf::st_linestring(coords),
    crs = 4326
  )
  
  route_sf <- sf::st_sf(
    distance = distance_km,
    duration = duration_min,
    geometry = route_geometry
  )
  
  stats <- list(
    distance_km = distance_km,
    duration_min = duration_min,
    method = "Euclidean (Fallback)"
  )
  
  return(list(
    geometry = route_sf,
    stats = stats
  ))
}

#' Create interactive map for emergency routing
#'
#' @param data Original data list
#' @param optimal_locations Optimal locations in WGS84
#' @param districts Districts in WGS84
#' @param include_roads Whether to show roads
#' @return Interactive leaflet map
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers addMarkers
#' @export
create_interactive_routing_map <- function(data, optimal_locations, districts, include_roads = TRUE) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. Please install it.")
  }
  
  # Create base map
  m <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB"),
      overlayGroups = c("Districts", "Optimal Locations", "Roads", "Accidents"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # Add district boundaries
  m <- m %>%
    leaflet::addPolygons(
      data = districts,
      fillColor = "lightblue",
      fillOpacity = 0.3,
      color = "darkblue",
      weight = 2,
      group = "Districts",
      popup = ~paste("District:", name)
    )
  
  # Add optimal emergency service locations
  m <- m %>%
    leaflet::addMarkers(
      data = optimal_locations,
      group = "Optimal Locations",
      popup = ~paste("Emergency Service Location:", id),
      icon = leaflet::makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41
      )
    )
  
  # Add existing accidents if available
  if ("accident" %in% names(data)) {
    accidents_wgs84 <- sf::st_transform(data$accident, 4326)
    m <- m %>%
      leaflet::addCircleMarkers(
        data = accidents_wgs84,
        radius = 2,
        color = "red",
        fillOpacity = 0.6,
        group = "Accidents",
        popup = ~paste("Historical Accident")
      )
  }
  
  # Add road network if requested and available
  if (include_roads && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    # Sample roads to avoid overloading map
    if (nrow(roads_wgs84) > 1000) {
      roads_sample <- roads_wgs84[sample(nrow(roads_wgs84), 1000), ]
    } else {
      roads_sample <- roads_wgs84
    }
    
    m <- m %>%
      leaflet::addPolylines(
        data = roads_sample,
        color = "gray",
        weight = 1,
        opacity = 0.5,
        group = "Roads"
      )
  }
  
  # Add click functionality for new accidents
  m <- m %>%
    leaflet::addControl(
      html = "<div style='background:white; padding:5px; border-radius:5px;'>
              <b>Emergency Routing Tool</b><br/>
              Click on the map to simulate a new accident location<br/>
              and find the nearest emergency service location.
              </div>",
      position = "topright"
    )
  
  return(m)
}

#' Calculate Euclidean distance route (straight line)
#' @keywords internal
calculate_euclidean_route <- function(new_accident, optimal_locations) {
  
  # Calculate distances to all optimal locations
  distances <- sf::st_distance(new_accident, optimal_locations)
  
  # Find nearest location
  nearest_idx <- which.min(distances)
  nearest_location <- optimal_locations[nearest_idx, ]
  nearest_distance <- distances[nearest_idx]
  
  # Create straight line route
  route_coords <- rbind(
    sf::st_coordinates(new_accident),
    sf::st_coordinates(nearest_location)
  )
  
  return(list(
    nearest_location = nearest_location,
    distance_m = as.numeric(nearest_distance),
    route_coordinates = route_coords,
    route_type = "euclidean"
  ))
}

#' Calculate road network route (simplified)
#' @keywords internal
calculate_road_network_route <- function(new_accident, optimal_locations, data) {
  
  # For now, use Euclidean as approximation
  # In a full implementation, this would use road network routing
  euclidean_result <- calculate_euclidean_route(new_accident, optimal_locations)
  
  # Add road network factor (typically 1.3x straight line distance)
  euclidean_result$distance_m <- euclidean_result$distance_m * 1.3
  euclidean_result$route_type <- "road_network_approx"
  
  return(euclidean_result)
}

#' Calculate hybrid route (weighted combination)
#' @keywords internal
calculate_hybrid_route <- function(new_accident, optimal_locations, data) {
  
  euclidean_result <- calculate_euclidean_route(new_accident, optimal_locations)
  road_result <- calculate_road_network_route(new_accident, optimal_locations, data)
  
  # Use road network estimate but keep euclidean coordinates for visualization
  hybrid_result <- euclidean_result
  hybrid_result$distance_m <- road_result$distance_m
  hybrid_result$route_type <- "hybrid"
  
  return(hybrid_result)
}

#' Create route visualization
#' @keywords internal
create_route_visualization <- function(new_accident, optimal_locations, districts, 
                                     route_result, data, include_roads) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts, 
                    fill = "lightblue", 
                    color = "darkblue", 
                    alpha = 0.3,
                    size = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Emergency Response Route",
                 subtitle = paste("Distance to nearest service:", 
                                round(route_result$distance_m), "meters"),
                 x = "Longitude", y = "Latitude")
  
  # Add roads if requested
  if (include_roads && "roads" %in% names(data)) {
    roads_wgs84 <- sf::st_transform(data$roads, 4326)
    p <- p + ggplot2::geom_sf(data = roads_wgs84, 
                             color = "gray", 
                             size = 0.2, 
                             alpha = 0.5)
  }
  
  # Add route line
  route_line <- data.frame(
    x = route_result$route_coordinates[,1],
    y = route_result$route_coordinates[,2]
  )
  
  p <- p + ggplot2::geom_path(data = route_line,
                             ggplot2::aes(x = .data$x, y = .data$y),
                             color = "red", size = 2, alpha = 0.8)
  
  # Add optimal locations
  p <- p + ggplot2::geom_sf(data = optimal_locations, 
                           color = "blue", 
                           size = 3, 
                           shape = 17)
  
  # Highlight nearest optimal location
  p <- p + ggplot2::geom_sf(data = route_result$nearest_location, 
                           color = "green", 
                           size = 5, 
                           shape = 17)
  
  # Add new accident location
  p <- p + ggplot2::geom_sf(data = new_accident, 
                           color = "red", 
                           size = 4, 
                           shape = 19)
  
  return(p)
}

#' Calculate route statistics
#' @keywords internal
calculate_route_statistics <- function(route_result) {
  
  return(list(
    nearest_location_id = route_result$nearest_location$id,
    distance_meters = round(route_result$distance_m),
    distance_km = round(route_result$distance_m / 1000, 2),
    estimated_travel_time_min = round(route_result$distance_m / 1000 / 50 * 60, 1), # Assume 50 km/h
    route_type = route_result$route_type
  ))
}
