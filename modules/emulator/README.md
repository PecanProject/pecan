
# PEcAn.emulator

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.emulator status badge](https://pecanproject.r-universe.dev/badges/PEcAn.emulator)](https://pecanproject.r-universe.dev/PEcAn.emulator)

<!-- badges: end -->

Gausian Process Emulator

## Installation

You can install the development version of `PEcAn.data.emulator` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.data.emulator in R
install.packages('PEcAn.data.emulator')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/data.emulator")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.emulator)
## basic example code
```

