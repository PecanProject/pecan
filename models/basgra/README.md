# PEcAn.BASGRA

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.BASGRA status badge](https://pecanproject.r-universe.dev/badges/PEcAn.BASGRA)](https://pecanproject.r-universe.dev/PEcAn.BASGRA)

<!-- badges: end -->

PEcAn Package for Integration of the BASGRA Model

## Installation

You can install the development version of `PEcAn.BASGRA` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.BASGRA in R
install.packages('PEcAn.BASGRA')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/basgra")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.BASGRA)
## basic example code
```
