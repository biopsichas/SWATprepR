% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{add_missing_pcp_zero}
\alias{add_missing_pcp_zero}
\title{Replace very low or empty PCP entries with valid data or 0}
\usage{
add_missing_pcp_zero(df_to_correct, df_valid = NULL, value_to_zero = 0.2)
}
\arguments{
\item{df_to_correct}{The dataframe to correct for the PCP variable with
"DATE" and "PCP" columns.}

\item{df_valid}{(optional) The dataframe with valid data for the PCP variable,
having "DATE" and "PCP" columns. If not provided, the function will use the
df_to_correct data. It should overlap with df_to_correct data.}

\item{value_to_zero}{(optional) The numeric value of daily PCP. Only NA values
can be set to 0 when df_valid PCP values are below or equal to value_to_zero
(default is 0.2).}
}
\value{
Updated dataframe
}
\description{
This function replaces deals with suspiciously low, missing PCP entries
in the provided dataframe.
}
\examples{
\dontrun{
# Get the current date as a POSIXct object
today <- as.POSIXct(Sys.Date())
# Set the end date as 10 days from the current date
end_day <- as.POSIXct(Sys.Date() + 10)

# Create a dataframe with a sequence of dates and corresponding PCP values
df_to_correct <- data.frame(
  DATE = seq.POSIXt(today, end_day, by = "1 day"),  # Create a daily sequence of dates
  PCP = c(0.1, NA, 0.3, NA, 0.5, NA, 0.7, NA, 0.9, NA, 1.1)
)

# Create a dataframe with valid PCP values for the same date range
df_valid <- data.frame(
  DATE = seq.POSIXt(today, end_day, by = "1 day"),  # Create a daily sequence of dates
  PCP = c(0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2)
)

# Apply the function to replace missing PCP values with zero
add_missing_pcp_zero(df_to_correct, df_valid, 0.3)
}
}
\keyword{internal}
