
# PEcAn.LPJGUESS

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.LPJGUESS status badge](https://pecanproject.r-universe.dev/badges/PEcAn.LPJGUESS)](https://pecanproject.r-universe.dev/PEcAn.LPJGUESS)

<!-- badges: end -->

PEcAn Package for Integration of the LPJ-GUESS Model

## Installation

You can install the development version of `PEcAn.LPJGUESS` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.LPJGUESS in R
install.packages('PEcAn.LPJGUESS')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/lpjguess")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.LPJGUESS)
## basic example code
```

