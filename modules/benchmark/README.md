
# PEcAn.benchmark

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.benchmark status badge](https://pecanproject.r-universe.dev/badges/PEcAn.benchmark)](https://pecanproject.r-universe.dev/PEcAn.benchmark)

<!-- badges: end -->

PEcAn Functions Used for Benchmarking

## Installation

You can install the development version of `PEcAn.benchmark` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.benchmark in R
install.packages('PEcAn.benchmark')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/benchmark")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.benchmark)
## basic example code
```

