
# PEcAn.dvmdostem

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.dvmdostem status badge](https://pecanproject.r-universe.dev/badges/PEcAn.dvmdostem)](https://pecanproject.r-universe.dev/PEcAn.dvmdostem)

<!-- badges: end -->

PEcAn Package for Integration of the Dvmdostem Model

## Installation

You can install the development version of `PEcAn.dvmdostem` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.dvmdostem in R
install.packages('PEcAn.dvmdostem')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/dvmdostem")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.dvmdostem)
## basic example code
```

