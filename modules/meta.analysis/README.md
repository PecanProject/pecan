
# PEcAn.MA

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.MA status badge](https://pecanproject.r-universe.dev/badges/PEcAn.MA)](https://pecanproject.r-universe.dev/PEcAn.MA)

<!-- badges: end -->

PEcAn Functions Used for Meta-Analysis

## Installation

You can install the development version of `PEcAn.MA` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.MA in R
install.packages('PEcAn.MA')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/meta.analysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.MA)
## basic example code
```

