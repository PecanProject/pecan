
# PEcAn.data.remote

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.data.remote status badge](https://pecanproject.r-universe.dev/badges/PEcAn.data.remote)](https://pecanproject.r-universe.dev/PEcAn.data.remote)

<!-- badges: end -->

PEcAn Functions Used for Extracting Remote Sensing Data

## Installation

You can install the development version of `PEcAn.data.remote` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.data.remote in R
install.packages('PEcAn.data.remote')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/data.remote")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.data.remote)
## basic example code
```

