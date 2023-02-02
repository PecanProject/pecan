
# PEcAn.qaqc

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.qaqc status badge](https://pecanproject.r-universe.dev/badges/PEcAn.qaqc)](https://pecanproject.r-universe.dev/PEcAn.qaqc)

<!-- badges: end -->

PEcAn integration and model skill testing

## Installation

You can install the development version of `PEcAn.qaqc` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.qaqc in R
install.packages('PEcAn.qaqc')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "base/qaqc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.qaqc)
## basic example code
```

