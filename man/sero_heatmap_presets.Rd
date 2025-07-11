% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotspots.R
\name{sero_heatmap_presets}
\alias{sero_heatmap_presets}
\title{Get recommended heatmap parameters for different analysis types}
\usage{
sero_heatmap_presets(analysis_type = "default")
}
\arguments{
\item{analysis_type}{Type of analysis: "urban", "regional", "overview", "detailed", "presentation"}
}
\value{
List of recommended parameters for sero_heatmap()
}
\description{
Provides preset parameter combinations for common analysis scenarios.
Makes it easier for users to get started with appropriate settings.
}
\examples{
\dontrun{
# Get parameters for urban analysis
urban_params <- sero_heatmap_presets("urban")

# Use with sero_heatmap
data <- sero_load_data()
heatmap <- do.call(sero_heatmap, c(list(data$accident), urban_params))
}
}
