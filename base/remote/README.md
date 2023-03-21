
# PEcAn.remote

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.remote status badge](https://pecanproject.r-universe.dev/badges/PEcAn.remote)](https://pecanproject.r-universe.dev/PEcAn.remote)

<!-- badges: end -->

PEcAn Model Execution Utilities

## Installation

You can install the development version of `PEcAn.remote` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.remote in R
install.packages('PEcAn.remote')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "base/remote")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.remote)
## basic example code
```

