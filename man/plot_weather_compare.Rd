% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_weather_compare}
\alias{plot_weather_compare}
\title{Plotting Figure to Compare Two Datasets with Weather Data}
\usage{
plot_weather_compare(
  meteo_lst1,
  meteo_lst2,
  par,
  period = "day",
  fn_summarize = "mean",
  name_set1 = "dataset 1",
  name_set2 = "dataset 2"
)
}
\arguments{
\item{meteo_lst1}{First nested list with dataframes.
Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
  Dataframe (DATE, PARAMETER)},
\code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
  geometry, Long, Lat)}. \cr\cr
meteo_lst can be created using \code{\link{load_template}} function using
'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
function loading information from SWAT+ model setup weather files.}

\item{meteo_lst2}{Second nested list with dataframes. Same structure as \code{meteo_lst1}.}

\item{par}{Character vector, weather variable to extract (e.g., "PCP", "SLR").}

\item{period}{(optional) Character, the time interval to display. Default
\code{period = "day"}, , other examples are "week", "month", "year".
See \href{https://www.rdocumentation.org/packages/lubridate/versions/1.3.3/topics/floor_date}{lubridate::floor_date} for details.}

\item{fn_summarize}{(optional) Function to recalculate to the specified time interval.
Default \code{fn_summarize ="mean"}, other examples are "median", "sum".
See \href{https://dplyr.tidyverse.org/reference/summarise.html}{dplyr::summarise} for details.}

\item{name_set1}{(optional) Character, to name the first dataset. Default
\code{name_set1 = "dataset 1"}.}

\item{name_set2}{(optional) Character, to name the second dataset. Default
\code{name_set2 = "dataset 2"}.}
}
\value{
Plotly figure object with displayed weather data for two datasets.
}
\description{
This function generates a Plotly figure to compare weather data between two datasets based on user-defined parameters.
}
\examples{
\dontrun{
##Loading data
temp_path1 <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
met_lst1 <- load_template(temp_path1)
temp_path2 <- system.file("extdata", "weather_data_raw.xlsx", package = "SWATprepR")
met_lst2 <- load_template(temp_path2)
##Plotting
plot_weather_compare(met_lst1, met_lst2, "PCP", "month", "mean", "clean", "raw")
}
}
\keyword{plotting}
