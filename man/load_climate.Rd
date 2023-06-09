% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loading.R
\name{load_climate}
\alias{load_climate}
\title{Function loading climate data csv files}
\usage{
load_climate(
  f_path,
  f_lst = list(PCP = "prec-1", SLR = "solarRad-1", RELHUM = "relHum-1", TMP_MAX =
    "Tmax-1", TMP_MIN = "Tmin-1", WNDSPD = "windSpeed-1")
)
}
\arguments{
\item{f_path}{character providing path to climate date folder (i.e. "inst/extdata/CORDEX-BC")}

\item{f_lst}{list providing file name for each weather parameter (default list("PCP" = "prec-1",
"SLR" = "solarRad-1", "RELHUM" = "relHum-1", "TMP_MAX" = "Tmax-1", "TMP_MIN" = "Tmin-1",
"WNDSPD" = "windSpeed-1"))}
}
\value{
nested list of lists with dataframes.
Nested structure meteo_lst -> RCM_MOD-> Parameter -> Dataframe (DATE, PARAMETER).
}
\description{
Function loading climate data csv files
}
\examples{
\dontrun{
temp_path <- paste0(system.file("extdata", package = "SWATprepR"), "/CORDEX-BC")
cli_lst <- load_climate(temp_path)
str(cli_lst)
}
}
\keyword{internal}
