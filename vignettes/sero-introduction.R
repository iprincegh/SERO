## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("iprincegh/SERO")

## ----setup--------------------------------------------------------------------
library(SERO)

## -----------------------------------------------------------------------------
# Load built-in data
data <- sero_load_data()

# Explore the data structure
names(data)

## -----------------------------------------------------------------------------
# Data summary
cat("Accidents:", nrow(data$accident), "\n")
cat("Population grid cells:", nrow(data$population), "\n")
cat("Road segments:", nrow(data$roads), "\n")
cat("Land use polygons:", nrow(data$landuse), "\n")
cat("Districts:", nrow(data$districts), "\n")

## -----------------------------------------------------------------------------
# Identify hotspots using point pattern analysis
hotspots <- sero_identify_hotspots(
  data$accident,
  risk_categories = c(1, 2),  # Fatal and serious accidents
  buffer = 1000,              # 1km buffer for density estimation
  min_events = 5              # Minimum 5 accidents per hotspot
)

print(hotspots)

## ----fig.cap="Accident hotspots visualization"--------------------------------
plot(hotspots)

