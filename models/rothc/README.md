# PEcAn.RothC

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.RothC status badge](https://pecanproject.r-universe.dev/badges/PEcAn.RothC)](https://pecanproject.r-universe.dev/PEcAn.RothC)

PEcAn Coupler for the RothC Model

## What is RothC?

RothC is a model for the turnover of organic carbon in non-waterlogged top-soils that allows for the effects of soil type, temperature, moisture content and plant cover on the turnover process. It uses a monthly time step to calculate total organic carbon (t ha⁻¹), microbial biomass carbon (t ha⁻¹) and δ¹⁴C (from which the equivalent radiocarbon age of the soil can be calculated) on a years to centuries timescale.

This package implements version 2.1 of the official Fortran version of RothC.

## PEcAn Integration

This package is following PEcAn's standard iterative process toward full model coupling. Current status:

- [x] Download, compile, and test run
- [x] jobs.sh existing test run
- [x] write.configs existing test run
- [ ] standard output (model2netcdf)
    - Partial: Only soil C written
- [x] test run through PEcAn interface
- [ ] met2model
    - Partial: Evaporation still hard-coded as 0
- [ ] parameters in write.configs
- [ ] extend list of PFTs, priors
- [ ] initial conditions
- [ ] read/write restart
- [ ] non-met inputs

## PEcAn configuration file additions

None yet.

TODO: Add support for setting options `RMmoist` and `SMDbare` via `settings$model$opt_RMmoist` and `settings$model$opt_SMDbare`.

## Model specific input files

RothC takes a single input file named `RothC_input.dat` with sections for global options, soil constants, and monthly weather and carbon input driver data. 

## Model configuration files

PEcAn.RothC builds its `RothC_input.dat` from 5 components:
* Options from `settings$model`.
* Soil parameters from `settings$run$inputs$soil_physics`.
* Meteorological data from `met2model.RothC()`.
* Plant and organic amendment inputs from a PEcAn events file.
* Decomposability of plant matter and organic amendments from PFT parameters.

## Installation

### Install PEcAn.RothC Package

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

Or install directly from GitHub:

``` r
remotes::install_github('pecanproject/pecan',  subdir = "models/RothC")
```

### Install RothC Model

```bash
git clone https://github.com/Rothamsted-Models/RothC_Code
cd RothC_Code
gfortran -std=gnu RothC.for Shell.for -o rothc_bin
cp rothc_bin /usr/local/bin/rothc_bin
```

## Example

See `inst/example_workflow` for a set of scripts to run multisite ensemble simulations of soil organic carbon content.
