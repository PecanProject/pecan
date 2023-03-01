
# PEcAn.all

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.all status badge](https://pecanproject.r-universe.dev/badges/PEcAn.all)](https://pecanproject.r-universe.dev/PEcAn.all)

<!-- badges: end -->

PEcAn functions used for ecological forecasts and reanalysis

## Installation

You can install the development version of `PEcAn.all` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.all in R
install.packages('PEcAn.all')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "base/all")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.all)
## basic example code
```

