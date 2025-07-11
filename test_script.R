#!/usr/bin/env Rscript

# SERO Package User Test Script
library(SERO)

cat("=== SERO Package User Test ===\n")
cat("1. Loading data...\n")
data <- sero_load_data()
cat("   ✓ Data loaded:", length(data), "layers\n")
cat("   ✓ Accidents:", nrow(data$accident), "records\n")

cat("2. Identifying hotspots...\n")
hotspots <- sero_identify_hotspots(data$accident, buffer = 1000)
cat("   ✓ Hotspots found:", nrow(hotspots$hotspots), "\n")

cat("3. Computing optimal locations...\n")
optimal <- sero_calculate_optimal_locations(data)
cat("   ✓ Optimal locations:", nrow(optimal$locations), "\n")

cat("4. Calculating routes...\n")
routes <- sero_calculate_routes(optimal, data$accident, max_routes = 5)
cat("   ✓ Routes calculated:", nrow(routes$routes), "\n")

cat("5. Testing visualization...\n")
p1 <- plot(hotspots)
p2 <- plot(optimal)
p3 <- plot(routes)
cat("   ✓ All plots created successfully\n")

cat("\n=== PACKAGE TEST COMPLETED SUCCESSFULLY ===\n")
cat("SERO package is fully functional from user perspective!\n")
