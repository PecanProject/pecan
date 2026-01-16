# PEcAn.RothC

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.RothC status badge](https://pecanproject.r-universe.dev/badges/PEcAn.RothC)](https://pecanproject.r-universe.dev/PEcAn.RothC)


PEcAn Coupler for the RothC Model

Work in progress. This package is following PEcAn's standard iterative process toward full model coupling:

- [x] Download, compile, and test run
- [x] jobs.sh existing test run
- [x] write.configs existing test run
- [ ] standard output (model2netcdf)
	* Partial: Only soil C written
- [x] test run through PEcAn interface
- [ ] met2model
	* Partial: Evaporation still hard-coded as 0
- [ ] parameters in write.configs
- [ ] extend list of PFTs, priors
- [ ] initial conditions
- [ ] read/write restart
- [ ] non-met inputs


## Example

See `inst/example_workflow` for a set of scripts to run multisite ensemble simulations of soil organic carbon content. TODO: consider converting to a vignette.


## Installation

You can install the development version of `PEcAn.RothC` from r-universe:

``` r
options(
  repos = c(
    getOption("repos"),
    pecanproject = 'https://pecanproject.r-universe.dev'
  )
)
install.packages('PEcAn.RothC')
```

Or you can install directly from GitHub with the remotes package:

``` r
remotes::install_github('pecanproject/pecan',  subdir = "models/RothC")

If you have a local clone of the PEcAn development repository, follow the directions provided with PEcAn (in short: use `make install`).
