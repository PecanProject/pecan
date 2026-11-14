# PEcAn.RCMEM

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.RCMEM status badge](https://pecanproject.r-universe.dev/badges/PEcAn.RCMEM)](https://pecanproject.r-universe.dev/PEcAn.RCMEM)

<!-- badges: end -->

PEcAn Package for Integration of the RCMEM Model

## Installation

You can install the development version of `PEcAn.RCMEM` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.RCMEM in R
install.packages('PEcAn.RCMEM')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/rcmem")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.RCMEM)
## basic example code
```
