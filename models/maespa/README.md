# PEcAn.MAESPA

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.MAESPA status badge](https://pecanproject.r-universe.dev/badges/PEcAn.MAESPA)](https://pecanproject.r-universe.dev/PEcAn.MAESPA)

PEcAn Coupler for the MAESPA Model

## Introduction

MAESPA is a model that simulates the radiation and water balance of forest canopies.

## Installation

### Install PEcAn.MAESPA Package

``` r
remotes::install_github('pecanproject/pecan',  subdir = "models/maespa")
```

### Install MAESPA Model

```bash
git clone https://bitbucket.org/remkoduursma/maespa.git
cd maespa
make
```

### Maeswrap R Package

MAESPA requires the `Maeswrap` R package. To install dependencies and the package:

```bash
sudo apt-get install r-cran-rgl libglu1-mesa-dev
```

Then in R:
```r
install.packages("Maeswrap")
```

## Documentation

- **Home Page**: http://maespa.github.io/
- **Source Code**: http://maespa.github.io/download.html
