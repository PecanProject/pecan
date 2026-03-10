# PEcAn.BIOCRO

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.BIOCRO status badge](https://pecanproject.r-universe.dev/badges/PEcAn.BIOCRO)](https://pecanproject.r-universe.dev/PEcAn.BIOCRO)

<!-- badges: end -->

## What is BioCro?

BioCro is a model that estimates photosynthesis at the leaf, canopy, and ecosystem levels and determines plant biomass allocation and crop yields, using underlying physiological and ecological processes to do so.

### PEcAn Integration

The BioCro model is integrated into the PEcAn workflow, allowing for large-scale simulations and data assimilation.

### PEcAn configuration file additions

The following sections of the PEcAn XML are relevant to the BioCro model: 

- `model`
  - `revision` -- Model version number
- `run`
  - `site/id` -- ID associated with desired site from BETYdb site entry
  - `inputs`
    - `met/output` -- Set as BIOCRO
    - `met/path` -- Path to file containing meteorological data

### Model configuration files

Genus-specific parameter files are required and stored within the PEcAn.BIOCRO package. `write.configs.BIOCRO` handles these automatically based on genus name.

When adding a new genus, provide a new default parameter file in PEcAn.BIOCRO [`inst/extdata/defaults`](https://github.com/PecanProject/pecan/tree/develop/models/biocro/inst/extdata/defaults).

## Installation

### Install PEcAn.BIOCRO Package

You can install the development version of `PEcAn.BIOCRO` from r-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.BIOCRO in R
install.packages('PEcAn.BIOCRO')
```

Or install directly from GitHub:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/biocro")
```

### Install BioCro Model

BioCro can be run standalone using the model's R package. To install version 0.951 (most robustly supported by PEcAn):

```r
remotes::install_github('ebimodeling/biocro@0.951')
```

## Example

``` r
library(PEcAn.BIOCRO)
## basic example code
```
