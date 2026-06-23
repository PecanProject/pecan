# PEcAn.PEPRMT

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.PEPRMT status badge](https://pecanproject.r-universe.dev/badges/PEcAn.PEPRMT)](https://pecanproject.r-universe.dev/PEcAn.PEPRMT)

<!-- badges: end -->

PEcAn Package for Integration of the PEPRMT Model

## Installation

You can install the development version of `PEcAn.PEPRMT` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.PEPRMT in R
install.packages('PEcAn.PEPRMT')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/peprmt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.PEPRMT)
## basic example code
```
