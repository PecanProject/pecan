# PEcAn.SIPNET

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.SIPNET status badge](https://pecanproject.r-universe.dev/badges/PEcAn.SIPNET)](https://pecanproject.r-universe.dev/PEcAn.SIPNET)

<!-- badges: end -->

## What is SIPNET?

The SIPNET v1 (**S**implified PnET (**P**hotosynthesis** and **E**vapo**t**ranspiration)) model is a lightweight ecosystem model designed to simulate ecosystem carbon and water dynamics. 

SIPNET v2 added a nitrogen cycle, trace greenhouse-gas (N₂O, CH₄) fluxes, and support for cropland and ecosystem management. As of PEcAn v1.10, support for SIPNET v2 is partially implemented and under active development.

**Key Features:**
- Simulates photosynthesis, respiration, allocation, soil water, soil temperature, and nitrogen cycling
- Operates on sub-daily time steps
- Tracks vegetation, litter, soil, and mineral-N pools
- Event-based management for agricultural processes (planting, harvest, tillage, irrigation)
- Clean, modular, BSD-licensed codebase suitable for research and production

For comprehensive SIPNET documentation, see the [SIPNET project website](https://pecanproject.github.io/sipnet).

## Using SIPNET via PEcAn

PEcAn.SIPNET is the PEcAn interface package that enables SIPNET integration with the PEcAn workflow system. This package:
- Converts PEcAn-standard inputs to SIPNET format
- Manages SIPNET model configuration and execution
- Converts SIPNET outputs to netCDF in PEcAn standard format
- Enables data assimilation, sensitivity analysis, and ensemble runs within the PEcAn modeling framework

**Getting Started:**
- [Demo 1: Basic PEcAn Run](https://pecanproject.github.io/pecan-documentation/develop/rendered-demo-notebooks/run_pecan.html)
- [SIPNET in the PEcAn Book](https://pecanproject.github.io/pecan-documentation/develop/models-sipnet.html)

## Installation

### Install PEcAn.SIPNET Package

You can install the development version of `PEcAn.SIPNET` from r-universe:

```r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.SIPNET in R
install.packages('PEcAn.SIPNET')
```

Or install directly from GitHub:

```r
library(remotes)
install_github('pecanproject/pecan', subdir = "models/sipnet")
```

### Install SIPNET Model

To use PEcAn.SIPNET, you also need the SIPNET model executable installed on your system. You can download precompiled versions from the SIPNET [releases page](https://github.com/PecanProject/sipnet/releases), or [download the code](https://github.com/PecanProject/sipnet) from Github and compile it yourself following the official [instructions](https://github.com/PecanProject/sipnet/blob/master/docs/user-guide/getting-started.md).

## Documentation

- **PEcAn.SIPNET Package Docs:** https://pecanproject.github.io/package-documentation/develop/PEcAn.SIPNET/
- **SIPNET Model Docs:** https://pecanproject.github.io/sipnet
- **PEcAn Book - SIPNET Chapter:** https://pecanproject.github.io/pecan-documentation/develop/models-sipnet.html
- **Source Code:** https://github.com/PecanProject/pecan/tree/develop/models/sipnet

## Learn More

- **PEcAn Project:** https://pecanproject.github.io
- **Tutorials:** https://pecanproject.github.io/tutorials/
- **SIPNET Repository:** https://github.com/PecanProject/sipnet
