# PEcAn.PRELES

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.PRELES status badge](https://pecanproject.r-universe.dev/badges/PEcAn.PRELES)](https://pecanproject.r-universe.dev/PEcAn.PRELES)

PEcAn Coupler for the PRELES Model

## Introduction

PRELES (PREdicting Light-use efficiency, Evapotranspiration and Soil water) is a model for simulating forest carbon and water cycles.

## Installation

### Install PEcAn.PRELES Package

You can install the development version of `PEcAn.PRELES` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.PRELES in R
install.packages('PEcAn.PRELES')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/preles")
```

### Install PRELES Model

PRELES is available as an R package. You can install it from GitHub:

``` r
remotes::install_github("mikapreles/Rpreles")
```

## Example

``` r
library(PEcAn.PRELES)
## basic example code
```
