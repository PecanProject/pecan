
# PEcAn.GDAY

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.GDAY status badge](https://pecanproject.r-universe.dev/badges/PEcAn.GDAY)](https://pecanproject.r-universe.dev/PEcAn.GDAY)

<!-- badges: end -->

PEcAn Package for Integration of the GDAY Model

## Installation

You can install the development version of `PEcAn.GDAY` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.GDAY in R
install.packages('PEcAn.GDAY')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/gday")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.GDAY)
## basic example code
```

