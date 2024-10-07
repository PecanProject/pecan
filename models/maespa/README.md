
# PEcAn.MAESPA

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.MAESPA status badge](https://pecanproject.r-universe.dev/badges/PEcAn.MAESPA)](https://pecanproject.r-universe.dev/PEcAn.MAESPA)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis using MAESPA

## Installation

You can install the development version of `PEcAn.MAESPA` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.MAESPA in R
install.packages('PEcAn.MAESPA')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/maespa")
```

When writing configurations files PEcAn.MAESPA uses the `Maeswrap` package.
We recommend Maeswrap 1.8.0 or later, which you can install from GitHub with `remotes::install_github("RemkoDuursma/Maeswrap")`.
An older version of Maeswrap (1.7 at this writing in September 2024) is also available from CRAN and does provide the functionality PEcAn.MAESPA needs, but this version has a strong dependency on the `rgl` package, which can be hard to install and are not needed by PEcAn. If you have trouble installing the extra dependencies or if you are annoyed by an unwanted plot window opening every time you use PEcAn.MAESPA, try upgrading Maeswrap to 1.8 or later.


## Example

This is a basic example which shows you how to solve a common problem:

```r
library(PEcAn.MAESPA)
## basic example code
```

