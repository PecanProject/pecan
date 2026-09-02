# PEcAn.STICS

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.STICS status badge](https://pecanproject.r-universe.dev/badges/PEcAn.STICS)](https://pecanproject.r-universe.dev/PEcAn.STICS)

PEcAn Coupler for the STICS Model

## Introduction

STICS (Simulateur mulTIdisciplinaire pour les Cultures Standard) is a crop model developed by INRA (French National Institute for Agronomic Research) and partners.

## Documentation

- **Home Page**: https://www6.paca.inrae.fr/stics/

## Installation

### Install PEcAn.STICS Package

You can install the development version of `PEcAn.STICS` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.STICS in R
install.packages('PEcAn.STICS')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/stics")
```

### Install STICS Model

The software (JavaStics interface and STICS model) is available for download after registration at the official website.

## Example

``` r
library(PEcAn.STICS)
## basic example code
```