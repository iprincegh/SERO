% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sero_routes.R
\name{calculate_road_routes}
\alias{calculate_road_routes}
\title{Calculate road-based routes}
\usage{
calculate_road_routes(service_locations, accidents, max_routes, data)
}
\arguments{
\item{service_locations}{sf object with service locations}

\item{accidents}{sf object with accident locations}

\item{max_routes}{Maximum number of routes to calculate}

\item{data}{Data object containing road network}
}
\value{
List with routes sf object
}
\description{
Calculate road-based routes
}
