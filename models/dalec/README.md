# PEcAn.DALEC

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.DALEC status badge](https://pecanproject.r-universe.dev/badges/PEcAn.DALEC)](https://pecanproject.r-universe.dev/PEcAn.DALEC)

PEcAn Coupler for the DALEC Model

## Introduction

DALEC (Data Assimilation Linked Ecosystem Carbon) is a simplified ecosystem model designed for carbon cycle data assimilation.

## Installation

### Install PEcAn.DALEC Package

You can install the development version of `PEcAn.DALEC` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.DALEC in R
install.packages('PEcAn.DALEC')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/dalec")
```

### Install DALEC Model

DALEC is typically provided as a simple executable or script.

## Example

``` r
library(PEcAn.DALEC)
## basic example code
```
