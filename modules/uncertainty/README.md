
# PEcAn.uncertainty

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.uncertainty status badge](https://pecanproject.r-universe.dev/badges/PEcAn.uncertainty)](https://pecanproject.r-universe.dev/PEcAn.uncertainty)

<!-- badges: end -->

PEcAn Functions Used for Propagating and Partitioning Uncertainties in Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAn.uncertainty` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.uncertainty in R
install.packages('PEcAn.uncertainty')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/uncertainty")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.uncertainty)
## basic example code
```

