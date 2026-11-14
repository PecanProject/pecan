# PEcAn.GDAY

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.GDAY status badge](https://pecanproject.r-universe.dev/badges/PEcAn.GDAY)](https://pecanproject.r-universe.dev/PEcAn.GDAY)

PEcAn Coupler for the GDAY Model

## Introduction

GDAY (Generic Decomposition and Yield) is an ecosystem model that simulates carbon, nitrogen, and water cycles in forest and grassland ecosystems.

## Installation

### Install PEcAn.GDAY Package

You can install the development version of `PEcAn.GDAY` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.GDAY in R
install.packages('PEcAn.GDAY')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/gday")
```

### Install GDAY Model

```bash
git clone https://github.com/mdekauwe/GDAY.git
cd GDAY/src
make
```

## Example

``` r
library(PEcAn.GDAY)
## basic example code
```
