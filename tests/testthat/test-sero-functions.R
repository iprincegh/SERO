test_that("data loading works", {
  data <- sero_load_data()
  expect_is(data, "list")
  expect_true("accident" %in% names(data))
  expect_true("roads" %in% names(data))
  expect_true(nrow(data$accident) > 0)
})

test_that("optimal location calculation works", {
  data <- sero_load_data()
  optimal_locs <- sero_calculate_optimal_locations(data, max_locations = 3)
  expect_is(optimal_locs, "sero_optimal_locations")
  expect_true(nrow(optimal_locs$locations) <= 3)
})

test_that("hotspot identification works", {
  data <- sero_load_data()
  hotspots <- sero_identify_hotspots(data$accident, min_events = 3)
  expect_is(hotspots, "sero_hotspots")
})

test_that("visualization functions work", {
  data <- sero_load_data()
  
  # Test individual plots
  expect_is(sero_plot_munster(data), "ggplot")
  expect_is(sero_plot_accidents(data), "ggplot")
  expect_is(sero_plot_roads(data), "ggplot")
  
  # Test combined plot
  expect_is(sero_plot_combined(data), "ggplot")
})

test_that("routing functions work", {
  data <- sero_load_data()
  optimal_locs <- sero_calculate_optimal_locations(data, max_locations = 2)
  
  # Test accident selection
  coords <- sf::st_coordinates(sf::st_transform(data$accident[1,], 4326))
  selected <- sero_select_accident_by_coords(data, coords[1], coords[2])
  expect_is(selected, "sf")
  
  # Test routing (only if optimal locations are available)
  if(!is.null(optimal_locs) && inherits(optimal_locs, "sf") && nrow(optimal_locs) > 0) {
    route <- sero_route_to_selected_accident(optimal_locs, selected)
    expect_is(route, "sero_route_to_accident")
  }
})
