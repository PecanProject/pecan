# PEcAn.LPJGUESS

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.LPJGUESS status badge](https://pecanproject.r-universe.dev/badges/PEcAn.LPJGUESS)](https://pecanproject.r-universe.dev/PEcAn.LPJGUESS)

PEcAn Coupler for the LPJ-GUESS Model

## Introduction

LPJ-GUESS is a dynamic global vegetation model (DGVM) that simulates vegetation dynamics, carbon, and water cycles.

## Installation

### Install PEcAn.LPJGUESS Package

You can install the development version of `PEcAn.LPJGUESS` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.LPJGUESS in R
install.packages('PEcAn.LPJGUESS')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/lpjguess")
```

### Install LPJ-GUESS Model

LPJ-GUESS is typically provided as a standalone executable.

## Example

``` r
library(PEcAn.LPJGUESS)
## basic example code
```

### Model configuration files

- **file1**: template at `models/lpjguess/inst/file1`
- **file2**: template at `models/lpjguess/inst/file2`
- **file3**: template at `models/lpjguess/inst/file3`
