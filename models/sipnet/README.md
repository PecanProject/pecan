# PEcAn.SIPNET

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.SIPNET status badge](https://pecanproject.r-universe.dev/badges/PEcAn.SIPNET)](https://pecanproject.r-universe.dev/PEcAn.SIPNET)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAn.SIPNET` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.SIPNET in R
install.packages('PEcAn.SIPNET')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/sipnet")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.SIPNET)
## basic example code
```
