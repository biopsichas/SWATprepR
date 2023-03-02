Introduction to svatools
================

# svatools

[![](https://img.shields.io/badge/devel%20version-0.0.5-gold.svg)](https://github.com/biopsichas/svatools)
[![](https://img.shields.io/github/last-commit/biopsichas/svatools.svg)](https://github.com/biopsichas/svatools/commits/green)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/languages/code-size/biopsichas/svatools.svg)](https://github.com/biopsichas/svatools)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/badge/doi-https://doi.org/10.5281/zenodo.7436013-yellow.svg)](https://doi.org/https://doi.org/10.5281/zenodo.7436013)

The goal of `svatools` is to help with the [SWAT+
model](https://swat.tamu.edu/software/plus/) input data preparation.
There are mostly functions, which were developed for the implementation
of modeling tasks in the [OPTAIN project](https://www.optain.eu/). These
tools are intended to fill the gaps in the SWAT+ workflow along side the
main tools developed by [Christoph
Schuerz](https://www.ufz.de/index.php?en=49467). Therefore, we highly
recommend trying and using these tools:

- [SWATfarmR](http://chrisschuerz.github.io/SWATfarmR/) - R tool for
  preparing management schedules for SWAT model;
- [SWATplusR](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html) -
  R tool for sensitivity analyse, model calibration and validation;
- [SWATbuildR](https://git.ufz.de/optain/wp4-integrated-assessment/swat/bildr_script)[^1] -
  R tool for building SWAT+ setups;
- [SWATdoctR](https://git.ufz.de/schuerz/swatdoctr)[^2] - A collection
  of functions in R and routines for SWAT model calibration and model
  diagnostics.

Detailed information about packages, workflow steps, input data, SWAT+
parameters, model calibration, validation, etc., could be found in the
[SWAT+ modeling protocol](https://doi.org/10.5281/zenodo.7463395).

## Installation

You can install the development version of svatools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("biopsichas/svatools")
# euptf2 package is needed and used for soil parameter functions
devtools::install_github("tkdweber/euptf2")
```

## Data

All the data required to run and test package is installed with package
in extdata folder. Exact location on computer could be found running
lines below. Please run it on your system to get it for you.

``` r
library(svatools)
temp_path <- system.file("extdata", package = "svatools")
print(temp_path)
#> [1] "C:/Users/laptop/AppData/Local/R/win-library/4.2/svatools/extdata"
```

## Templates

In order to use *svatools* package functions with your data you should
prepare your data to be inline with templates we have provided in
*extdata* folder. Such are:

- **calibration_data.xlsx** - template for loading calibration (water
  flow and water quality variables) data.
- **weather_data.xlsx** - template for loading weather variables.
- **soil_lookup.xlsx** - example of loading soil parameters dataset.
- **GIS/** - folder with GIS layers needed to run some functions.
- **CORDEX-BC/** - folder with example climate data.

Data prepared according to templates can be directly loaded into R and
all the functions applied as described.

[^1]: Currently requires access to OPTAIN <https://git.ufz.de/optain>

[^2]: Currently requires access to OPTAIN <https://git.ufz.de/optain>
