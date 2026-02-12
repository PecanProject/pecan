# PEcAn.LINKAGES

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![PEcAn.LINKAGES status badge](https://pecanproject.r-universe.dev/badges/PEcAn.LINKAGES)](https://pecanproject.r-universe.dev/PEcAn.LINKAGES)
<!-- badges: end -->

## Description

PEcAn Package for Integration of the LINKAGES Model

LINKAGES is a forest succession and yield model that simulates forest stand dynamics. The PEcAn package `PEcAn.LINKAGES` provides integration with the PEcAn workflow system for parameter estimation, sensitivity analysis, and uncertainty quantification.

## Installation

You can install the development version of `PEcAn.LINKAGES` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.LINKAGES in R
install.packages('PEcAn.LINKAGES')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan', subdir = "models/linkages")
```

## Features

- Full integration with PEcAn's parameter estimation workflow
- Support for Bayesian calibration and sensitivity analysis
- Restart functionality for data assimilation
- Prescribed inputs for meteorological forcing

## Documentation

- [R Package Documentation](https://pecanproject.r-universe.dev/PEcAn.LINKAGES)
- [PEcAn Book - Models Reference](https://pecanproject.github.io/pecan-documentation/pecan-models.html#pecan-models)
- [GitHub Repository](https://github.com/PecanProject/pecan/tree/develop/models/linkages)

## References

Linked simulations of vegetation-ecosystem and watershed processes. PEcAn.LINKAGES provides a modern interface to the LINKAGES forest succession model within the PEcAn framework.
