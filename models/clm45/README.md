# PEcAn.CLM45

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.CLM45 status badge](https://pecanproject.r-universe.dev/badges/PEcAn.CLM45)](https://pecanproject.r-universe.dev/PEcAn.CLM45)

PEcAn Coupler for the Community Land Model (CLM)

## Introduction

The Community Land Model (CLM) is the land component of the Community Earth System Model (CESM). It simulates a wide range of ecological and hydrological processes.

### Model specific input files

CLM requires several input files, including atmospheric forcing, surface datasets, and parameter files.

## Installation

### Install PEcAn.CLM45 Package

You can install the development version of `PEcAn.CLM45` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.CLM45 in R
install.packages('PEcAn.CLM45')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/clm45")
```

### Install CLM Model

Installation varies by version and hosting system. Usually requires building within the CESM framework.

## Example

``` r
library(PEcAn.CLM45)
## basic example code
```
