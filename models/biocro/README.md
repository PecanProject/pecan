# PEcAn.BIOCRO

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.BIOCRO status badge](https://pecanproject.r-universe.dev/badges/PEcAn.BIOCRO)](https://pecanproject.r-universe.dev/PEcAn.BIOCRO)

<!-- badges: end -->

BioCro is a model that estimates photosynthesis at the leaf, canopy, and ecosystem levels and determines plant biomass allocation and crop yields, using underlying physiological and ecological processes to do so.

## Installation

You can install the development version of `PEcAn.BIOCRO` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.BIOCRO in R
install.packages('PEcAn.BIOCRO')
```

Or you can install directly from GitHub with the `remotes` package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/biocro")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.BIOCRO)
## basic example code
```
