
# PEcAn.CLM45

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.CLM45 status badge](https://pecanproject.r-universe.dev/badges/PEcAn.CLM45)](https://pecanproject.r-universe.dev/PEcAn.CLM45)

<!-- badges: end -->

PEcAn Package for Integration of CLM4.5 Model

## Installation

You can install the development version of `PEcAn.CLM45` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.CLM45 in R
install.packages('PEcAn.CLM45')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/clm45")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.CLM45)
## basic example code
```

