# PEcAn.SIBCASA

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.SIBCASA status badge](https://pecanproject.r-universe.dev/badges/PEcAn.SIBCASA)](https://pecanproject.r-universe.dev/PEcAn.SIBCASA)
<!-- badges: end -->

## Description

PEcAn Package for Integration of the SiBCASA Model

SiBCASA (Simple Biosphere/Carnegie-Ames-Stanford Approach) is a coupled biogeochemistry model that integrates vegetation photosynthesis and soil carbon dynamics. The PEcAn package `PEcAn.SIBCASA` provides integration with the PEcAn workflow system.

**Note**: This module is a work in progress and is not yet fully functional.

## Installation

You can install the development version of `PEcAn.SIBCASA` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.SIBCASA in R
install.packages('PEcAn.SIBCASA')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan', subdir = "models/sibcasa")
```

## Status

**This module is a work in progress and is not yet fully functional.**

Current capabilities:
- Basic model structure in place
- Configuration file generation (partial)
- Meteorological preprocessing support

Contributing developers should refer to the main SIBCASA documentation and collaborate with the maintainers.

## Documentation

- [R Package Documentation](https://pecanproject.r-universe.dev/PEcAn.SIBCASA)
- [PEcAn Book - SiBCASA Chapter](https://pecanproject.github.io/pecan-documentation/pecan-models.html#models-sibcasa)
- [GitHub Repository](https://github.com/PecanProject/pecan/tree/develop/models/sibcasa)
- [External Model Documentation](https://daac.ornl.gov/MODELS/guides/SiBCASA.html)

## Contributing

Issues and pull requests are welcome. Please contact the maintainers for development guidance.

## References

Potter, C. S., et al. (1993). Estimates of carbon sequestration by the forests of the United States based on forest inventory and geographic data. Journal of Geophysical Research. 
relevant publications.

* `/tests/testthat/`

Each package should have tests that cover the key functions of the package, 
at a minimum, the three functions above.

* documentation

Update the `NAMESPACE`, `DESCRIPTION` and `man/*.Rd` files by running 

```r
devtools("models/<modelname>/")
```
