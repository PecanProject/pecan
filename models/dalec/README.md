# PEcAn.DALEC

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.DALEC status badge](https://pecanproject.r-universe.dev/badges/PEcAn.DALEC)](https://pecanproject.r-universe.dev/PEcAn.DALEC)

<!-- badges: end -->

PEcAn Package for Integration of the DALEC Model

## Installation

You can install the development version of `PEcAn.DALEC` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.DALEC in R
install.packages('PEcAn.DALEC')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/dalec")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.DALEC)
## basic example code
```
