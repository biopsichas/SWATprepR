% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preparing.R
\name{load_climate_lst}
\alias{load_climate_lst}
\title{Extract climate data into nested list of lists}
\usage{
load_climate_lst(dir_path, location)
}
\arguments{
\item{dir_path}{character, path to CORDEX-BC folder (example climate/CORDEX-BC").}

\item{location}{character or list. In case of character, path should be provided to
catchment boundary file (example GIS/basin.shp). In case of list, nested list of lists
with dataframes. Nested structure meteo_lst -> data -> Station ID -> Parameter ->
Dataframe (DATE, PARAMETER). List of list could be obtained using \code{load_template()}
with prepared excel template.}
}
\value{
Nested lists of lists. First nesting level is for RCP, second for RCM model numbers,
the rest is the same as in meteo_lst. This part could be used with other package functions
(example  plot_weather(result$rcp26$1), "PCP", "month", "sum").
}
\description{
Extract climate data into nested list of lists
}
\examples{
\dontrun{
basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR") 
data_path <- "climate/CORDEX-BC"
result <- load_climate_lst(data_path, basin_path)
}
}
