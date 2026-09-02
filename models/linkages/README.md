# PEcAn.LINKAGES

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.LINKAGES status badge](https://pecanproject.r-universe.dev/badges/PEcAn.LINKAGES)](https://pecanproject.r-universe.dev/PEcAn.LINKAGES)

PEcAn Coupler for the LINKAGES Model

## Introduction

LINKAGES is a forest gap model designed to simulate the growth and death of individual trees on a small plot.

## Installation

### Install PEcAn.LINKAGES Package

You can install the development version of `PEcAn.LINKAGES` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.LINKAGES in R
install.packages('PEcAn.LINKAGES')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/linkages")
```

### Install LINKAGES Model

The LINKAGES model code is included within the PEcAn package and is compiled during installation.

## Example

``` r
library(PEcAn.LINKAGES)
## basic example code
```

### Model configuration files

- **file1**: template at `models/linkages/inst/file1`
- **file2**: template at `models/linkages/inst/file2`
- **file3**: template at `models/linkages/inst/file3`
