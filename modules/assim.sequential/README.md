
# PEcAnAssimSequential

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAnAssimSequential status badge](https://pecanproject.r-universe.dev/badges/PEcAnAssimSequential)](https://pecanproject.r-universe.dev/PEcAnAssimSequential)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAnAssimSequential` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAnAssimSequential in R
install.packages('PEcAnAssimSequential')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/assim.sequential")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAnAssimSequential)
## basic example code
```

