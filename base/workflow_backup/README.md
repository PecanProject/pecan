
# PEcAn.workflow

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.workflow status badge](https://pecanproject.r-universe.dev/badges/PEcAn.workflow)](https://pecanproject.r-universe.dev/PEcAn.workflow)

<!-- badges: end -->

PEcAn functions used for ecological forecasts and reanalysis

## Installation

You can install the development version of `PEcAn.workflow` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.workflow in R
install.packages('PEcAn.workflow')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "base/workflow")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.workflow)
## basic example code
```

