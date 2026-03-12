# PEcAn.LDNDC

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.LDNDC status badge](https://pecanproject.r-universe.dev/badges/PEcAn.LDNDC)](https://pecanproject.r-universe.dev/PEcAn.LDNDC)

PEcAn Coupler for the LandscapeDNDC Model

## Introduction

LandscapeDNDC is a ecosystem model for simulating C and N cycling, as well as greenhouse gas emissions from terrestrial ecosystems.

## Installation

### Install PEcAn.LDNDC Package

You can install the development version of `PEcAn.LDNDC` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.LDNDC in R
install.packages('PEcAn.LDNDC')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan', subdir = "models/ldndc")
```

### Install LDNDC Model

LandscapeDNDC is typically provided as a binary executable.

## Example

``` r
library(PEcAn.LDNDC)
## basic example code
```
