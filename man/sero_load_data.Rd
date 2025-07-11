% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_loading.R
\name{sero_load_data}
\alias{sero_load_data}
\title{Load built-in SERO spatial data}
\usage{
sero_load_data()
}
\value{
List of sf objects containing spatial data
}
\description{
Loads the built-in spatial data from the inst/gpkg folder.
This data is used for all SERO analysis functions which implement
6 multi-criteria analysis criteria:
\enumerate{
\item High-Risk Accidents (UKATEGORIE = 1 or 2)
\item Suitable Land Use (residential, commercial, industrial)
\item Proximity to Roads (500m-1000m from roads)
\item Accident Density (100m grid cells)
\item Population Density (higher density areas prioritized)
\item Centroid Calculation (ST_Centroid equivalent)
}
}
\examples{
\dontrun{
# Load built-in data
data <- sero_load_data()

# View available layers
print(names(data))
}
}
