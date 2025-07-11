% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sero_routes.R
\name{sero_load_locations}
\alias{sero_load_locations}
\title{Load optimal locations from a dataset}
\usage{
sero_load_locations(filename)
}
\arguments{
\item{filename}{File path to load the locations from (must be .gpkg file)}
}
\value{
sf object with optimal locations
}
\description{
Loads pre-computed optimal locations from a GeoPackage file for use in routing.
GeoPackage format is optimal for sf objects and preserves all spatial information.
}
\examples{
\dontrun{
# Load from GeoPackage
locations <- sero_load_locations("optimal_locations.gpkg")

# Use in routing
data <- sero_load_data()
routes <- sero_routes(locations, data$accident[1:5, ])
}
}
