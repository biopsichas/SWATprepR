% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preparing.R
\name{extract_rotation}
\alias{extract_rotation}
\title{Extract rotation information from raster file}
\usage{
extract_rotation(df, start_year, tif_name, r_path, lookup, lu_constant = c())
}
\arguments{
\item{df}{sf data.frame with land use. "id" and "type" columns should be present.}

\item{start_year}{numeric, representing a year from which data begins.}

\item{tif_name}{string for name of .tif raster file.}

\item{r_path}{string for path to .tif file.}

\item{lookup}{dataframe with "lc1" column for numeric codes and "type" column
for text.}

\item{lu_constant}{vector of strings with land uses to be kept constant in
land use (i.e. water, urban areas, etc.)}
}
\value{
sf data.frame with land use amended with crop rotation information
}
\description{
Extract rotation information from raster file
}
\examples{
\dontrun{
library(sf)
##Loading land use/crop layer
lu_path <- system.file("extdata", "GIS/lu_layer.shp", package = "SWATprepR")
lu <- st_read(lu_path,  quiet = TRUE) \%>\% mutate(id = row_number())
##Preparing lookup table
lookup <- data.frame(lc1 = seq(1:length(unique(c(lu$type)))), 
type = unique(c(lu$type)))
lu_constant <- c("fesc", "orch", "frst", "frse", "frsd", "urld", "urhd", 
"wetl", "past", "watr", "agrl")

##Extracting rotation information from raster
##Raster information should have been prepared with remote sensing 
lu_rot <- extract_rotation(lu, 2015, "cropmaps.tif", "./output/", lookup, lu_constant)
}
}
