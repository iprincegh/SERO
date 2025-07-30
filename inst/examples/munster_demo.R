# SERO Package - Basic Münster Demo
# This is a simple demonstration of the SERO package core functionality

# Load required libraries
library(SERO)

# Load the built-in Münster dataset
cat("Loading Münster emergency response dataset...\n")
data <- sero_load_data()

# Validate data structure
if (sero_validate_data(data)) {
  cat("✅ Data validation successful!\n")
} else {
  cat("❌ Data validation failed!\n")
}

# Display basic dataset information
cat("\n📊 Dataset Summary:\n")
cat("==================\n")
for (layer in names(data)) {
  cat(sprintf("%-12s: %,d features\n", layer, nrow(data[[layer]])))
}

# Define high-risk categories (fatal and serious accidents)
high_risk_categories <- c(1, 2)

# Basic hotspot analysis
cat("\n🔍 Finding accident hotspots...\n")
hotspots <- sero_hotspots(
  accidents = data$accident,
  risk_categories = high_risk_categories,
  buffer = 800,
  min_events = 3
)

cat("Found", nrow(hotspots$hotspots), "hotspots\n")

# Plot hotspots
plot(hotspots, data = data, show_munster = TRUE)

# Find optimal emergency service locations
cat("\n🎯 Finding optimal locations...\n")
optimal_locations <- sero_find_optimal_locations(
  data = data,
  num_locations = 5,
  method = "fast",
  risk_categories = high_risk_categories
)

cat("Found", nrow(optimal_locations), "optimal locations\n")

# Create quick visualization
sero_plot_optimal_quick(optimal_locations, data$districts, accidents = data$accident)

cat("\n✅ Basic demonstration complete!\n")
cat("For comprehensive analysis, see SERO_comprehensive_demo.Rmd\n")
