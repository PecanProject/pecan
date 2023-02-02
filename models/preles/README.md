# PEcAn.PRELES

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![PEcAn.PRELES status badge](https://pecanproject.r-universe.dev/badges/PEcAn.PRELES)](https://pecanproject.r-universe.dev/PEcAn.PRELES)

<!-- badges: end -->

PEcAn Package for Integration of the PRELES Model

## Installation

You can install the development version of `PEcAn.PRELES` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.PRELES in R
install.packages('PEcAn.PRELES')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/preles")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.PRELES)
## basic example code
```
