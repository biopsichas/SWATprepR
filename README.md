Introduction to SWATprepR
================

# SWATprepR

[![](https://img.shields.io/badge/devel%20version-1.0.11-gold.svg)](https://github.com/biopsichas/SWATprepR)
[![](https://img.shields.io/github/last-commit/biopsichas/SWATprepR.svg)](https://github.com/biopsichas/SWATprepR/commits/green)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/languages/code-size/biopsichas/SWATprepR.svg)](https://github.com/biopsichas/SWATprepR)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/badge/doi-https://doi.org/10.1186/s12302--024--00873--1-yellow.svg)](https://doi.org/https://doi.org/10.1186/s12302-024-00873-1)

The goal of `SWATprepR` is to help with the [SWAT+
model](https://swat.tamu.edu/software/plus/) input data preparation. A
detailed overview is presented in the article by Plunge, Szabó, et al.
(2024). Most functions were developed for the implementation of modeling
tasks in the [OPTAIN project](https://www.optain.eu/). These tools are
intended to fill the gaps in the SWAT+ workflow alongside the main tools
developed by [Christoph Schuerz](https://www.ufz.de/index.php?en=49467).
Therefore, we highly recommend trying and using these tools:

- [SWATbuildR](https://github.com/chrisschuerz/SWATbuildR) - R tool for
  building SWAT+ setups.
- [SWATfarmR](http://chrisschuerz.github.io/SWATfarmR/) - R tool for
  preparing management schedules for the SWAT model.
- [SWATdoctR](https://git.ufz.de/schuerz/swatdoctr) - A collection of
  functions in R and routines for SWAT model diagnostics. The package is
  presented in the article by Plunge, Schürz, et al. (2024).
- [SWATrunR](https://chrisschuerz.github.io/SWATrunR/) - R tool for
  running SWAT models for different parameters and scenarios.
- [SWATtunR](https://biopsichas.github.io/SWATtunR/) - R tool for soft &
  hard calibration, validation of SWAT+ models.
- [SWATmeasR](https://git.ufz.de/schuerz/swatmeasr) - R tool for
  implementing Natural/Small Water Retention Measures (NSWRMs) in the
  SWAT+ models and running scenarios.

<img src="man/figures/swativerse_update.png" title="SWAT packages for R" alt="swativerse logo" width="80%" style="display: block; margin: auto;" />

Detailed information about packages, workflow steps, input data, SWAT+
parameters, model calibration, validation, etc., can be found in the
[SWAT+ modeling protocol](https://doi.org/10.5281/zenodo.7463395) by
Christoph et al. (2022).

## Installation

You can install the development version of `SWATprepR` from
[GitHub](https://github.com/biopsichas/SWATprepR). Please be aware that
to run the `get_usersoil_table()` function, the `euptf2`
[package](https://github.com/tkdweber/euptf2) has to be installed. More
information about this package can be found in the article by Szabó,
Weynants, and Weber (2020). Other functions might require specific
packages as well. Please check the documentation of the functions for
more information.

``` r
# If the package 'remotes' is not installed run first:
install.packages("remotes")

# The installation of `SWATprepR`.
remotes::install_github("biopsichas/SWATprepR")
```

## Data

All the data required to run and test the package is installed with the
package in the extdata folder. The exact location on your computer can
be found by running the lines below. Please run these commands on your
system to locate it.

``` r
library(SWATprepR)
temp_path <- system.file("extdata", package = "SWATprepR")
print(temp_path)
#> [1] "C:/Users/laptop/AppData/Local/R/win-library/4.2/SWATprepR/extdata"
```

## Templates

To use the `SWATprepR` package functions with your data, you should
prepare your data to align with the templates provided in the *extdata*
folder. These templates include:

- **calibration_data.xlsx** - template for loading calibration (water
  flow and water quality variables) data.
- **weather_data.xlsx** - template for loading weather variables.
- **usersoils.csv** - example of a soil parameters dataset.
- **pnt_data.xlsx** - template for a point source dataset.
- **GIS/** - folder with GIS layers needed to run some functions.

Data prepared according to these templates can be directly loaded into
R, allowing you to apply all the functions as described.

<br>

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-optain2022" class="csl-entry">

Christoph, Schürz, Čerkasova Natalja, Farkas Csilla, Nemes Attila,
Plunge Svajunas, Strauch Michael, Szabó Brigitta, and Piniewski Mikołaj.
2022. “<span class="nocase">SWAT+ modeling protocol for the assessment
of water and nutrient retention measures in small agricultural
catchments</span>.” Zenodo. <https://doi.org/10.5281/zenodo.7463395>.

</div>

<div id="ref-plunge2024a" class="csl-entry">

Plunge, Svajunas, Christoph Schürz, Natalja Čerkasova, Michael Strauch,
and Mikołaj Piniewski. 2024. “<span class="nocase">SWAT+ model setup
verification tool: SWATdoctR</span>.” *Environmental Modelling &
Software* 171: 105878. <https://doi.org/10.1016/j.envsoft.2023.105878>.

</div>

<div id="ref-plunge2024b" class="csl-entry">

Plunge, Svajunas, Brigitta Szabó, Michael Strauch, Natalja Čerkasova,
Christoph Schürz, and Mikołaj Piniewski. 2024.
“<span class="nocase">SWAT + input data preparation in a scripted
workflow: SWATprepR</span>.” *Environmental Sciences Europe* 36 (1): 53.
<https://doi.org/10.1186/s12302-024-00873-1>.

</div>

<div id="ref-szabo2020" class="csl-entry">

Szabó, Brigitta, Melanie Weynants, and Tobias K. D. Weber. 2020.
“<span class="nocase">Updated European hydraulic pedotransfer functions
with communicated uncertainties in the predicted variables
(euptfv2)</span>.” *Geoscientific Model Development* 14 (1): 151–75.
<https://doi.org/10.5194/gmd-14-151-2021>.

</div>

</div>
