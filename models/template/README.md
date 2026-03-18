# PEcAn.MODEL

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.MODEL status badge](https://pecanproject.r-universe.dev/badges/PEcAn.MODEL)](https://pecanproject.r-universe.dev/PEcAn.MODEL)

<!-- badges: end -->

PEcAn Functions for the MODEL model.

## What is MODEL?

[Add a brief (1-2 sentence) description of the ecological model here.]

For full details about the model logic, compiling the source, inputs, and outputs, please see the **[Standalone MODEL Documentation](URL_TO_STANDALONE_DOCS)**.

* **Homepage**: [URL_TO_HOMEPAGE](URL_TO_HOMEPAGE)
* **Source Code**: [URL_TO_SOURCE](URL_TO_SOURCE)
* **Authors**: Maintainer Name, Original Author Name (PEcAn Integration)
* **License**: [e.g. BSD 3-Clause]

## Using MODEL via PEcAn

The `PEcAn.MODEL` package provides the necessary functions to generate configuration files and process outputs for MODEL within the PEcAn workflow. 

**Getting Started:**
- [Demo 1: Basic PEcAn Run](https://pecanproject.github.io/pecan-documentation/develop/rendered-demo-notebooks/run_pecan.html)
- [MODEL in the PEcAn Book](https://pecanproject.github.io/pecan-documentation/develop/models-MODEL.html)

### Model Configuration Files

MODEL is configured using [X] files which are placed in the run folder, along with any necessary input links:

* **`config.in`**: [Description of function and where template lives in `inst/`]
* **`params.file`**: [Description]

## Installation

### Install PEcAn.MODEL Package

You can install the development version of `PEcAn.MODEL` from R-universe:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.MODEL in R
install.packages('PEcAn.MODEL')
```

Or you can install directly from GitHub using the `remotes` package:

``` r
library(remotes)
install_github('pecanproject/pecan', subdir = "models/template")
```

### Install MODEL Model

To use PEcAn.MODEL, you also need the MODEL model executable installed on your system. Refer to the [MODEL installation guide](URL_TO_INSTALLATION_GUIDE) for instructions.

## Documentation

- **PEcAn.MODEL pkgdown site:** [https://pecanproject.github.io/package-documentation/develop/PEcAn.MODEL/index.html](https://pecanproject.github.io/package-documentation/develop/PEcAn.MODEL/index.html)
- **Standalone MODEL Docs:** [URL_TO_STANDALONE_DOCS](URL_TO_STANDALONE_DOCS)
- **PEcAn Book - MODEL Chapter:** [https://pecanproject.github.io/pecan-documentation/develop/models-MODEL.html](https://pecanproject.github.io/pecan-documentation/develop/models-MODEL.html)
- **Source Code:** [https://github.com/PecanProject/pecan/tree/develop/models/template](https://github.com/PecanProject/pecan/tree/develop/models/template)
