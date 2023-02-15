
# PEcAn.DB

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.DB status badge](https://pecanproject.r-universe.dev/badges/PEcAn.DB)](https://pecanproject.r-universe.dev/PEcAn.DB)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAn.DB` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.DB in R
install.packages('PEcAn.DB')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "base/db")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.DB)
## basic example code
```

