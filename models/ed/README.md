# PEcAn.ED2

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.ED2 status badge](https://pecanproject.r-universe.dev/badges/PEcAn.ED2)](https://pecanproject.r-universe.dev/PEcAn.ED2)

<!-- badges: end -->

PEcAn Package for Integration of ED2 Model

## Installation

You can install the development version of `PEcAn.ED2` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.ED2 in R
install.packages('PEcAn.ED2')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/ed")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.ED2)
## basic example code
```
