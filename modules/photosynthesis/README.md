
# PEcAn.photosynthesis

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.photosynthesis status badge](https://pecanproject.r-universe.dev/badges/PEcAn.photosynthesis)](https://pecanproject.r-universe.dev/PEcAn.photosynthesis)

<!-- badges: end -->

PEcAn functions used for leaf-level photosynthesis calculations

## Installation

You can install the development version of `PEcAn.photosynthesis` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.photosynthesis in R
install.packages('PEcAn.photosynthesis')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/photosynthesis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.photosynthesis)
## basic example code
```

