## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(SERO)

## -----------------------------------------------------------------------------
# Load data
data <- sero_load_data()

# Test different buffer sizes
buffer_values <- c(300, 500, 800, 1000, 1200, 1500)
buffer_results <- data.frame(
  buffer = buffer_values,
  hotspots = integer(length(buffer_values)),
  max_density = numeric(length(buffer_values))
)

for (i in seq_along(buffer_values)) {
  result <- sero_identify_hotspots(data$accident, buffer = buffer_values[i])
  buffer_results$hotspots[i] <- nrow(result$hotspots)
  if (nrow(result$hotspots) > 0) {
    buffer_results$max_density[i] <- max(result$hotspots$density, na.rm = TRUE)
  }
}

print(buffer_results)

