A generic template for adding a new model to PEcAn
==========================================================================

Adding a new model to PEcAn in a few easy steps:

1. add modeltype to BETY
2. add a model and PFT to BETY for use with modeltype
3. implement 3 functions as described below
4. Add tests to `tests/testthat`
5. Update README, documentation
6. Update Dockerfile and model_info.json
7. execute pecan with new model


### Three Functions

There are 3 functions that will need to be implemented, each of these
functions will need to have MODEL be replaced with the actual modeltype as
it is defined in the BETY database.

* `write.config.MODEL.R`

 This will write the configuratin file as well as the job launcher used by
 PEcAn. There is an example of the job execution script in the template
 folder. The configuration file can also be a template that is found based
 on the revision number of the model. This should use the computed results
 specified in defaults and trait.values to write a configuration file
 based on the PFT and traits found.

* `met2model.MODEL.R`

 This will convert the standard Met CF file to the model specific file
 format. This will allow PEcAn to create metereological files for the
 specific site and model. This will only be called if no meterological
 data is found for that specific site and model combination.

* `model2netcdf.MODEL.R`

 This will convert the model specific output to NACP Intercomparison
 format. After this function is finished PEcAn will use the generated
 output and not use the model specific outputs. The outputs should be
 named YYYY.nc
 
### Dockerization

The PEcAn system is leveraging Docker to encapsulate most of the code.
This will make it easier to share new model with others, without them
having to compile the models. The goal is for people to be able to
launch the model in docker, and it will register with PEcAn and is
almost immediatly available to be used. To accomplish this you will need to modify two files.

* `Dockerfile`

 The [Dockerfile](https://docs.docker.com/engine/reference/builder/) is
 like the Makefile for docker. This file is split in two pieces the
 part at the top is to actually build the binary. This is where you
 specify all the libraries that are needed, as well as all the build
 tools to compile your model. The second part, starting at the second
 `FROM` line, is where you will install only the libraries needed to
 run the binary and copy the binary from the build stage, using the
 `COPY --from` line.
 
* `model_info.json`

 The model_info.json describes the model and is used to register the
 model with PEcAn. In the model_info.json the only fields that are
 really required are those at the top: `name`, `type`, `version` and
 `binary`. All other fields are optional but are good to be filled
 out. You can leave `version` and `binary` with the special values
 which will be updated by the Dockerfile.
 
Once the image can be build it can be pushed so others can leverage
of the model. For PEcAn we have been using the following naming scheme
for the docker images: `pecan/model-<model>-<model_version>:<pecan_version>`
where the `model` and `model_version` are the same as those used to
build the model, and `pecan_version` is the version of PEcAn this
model is compiled for.

### Additional Changes
 
* `README.md` 
 
This file should contain basic background information about the model. 
At a minimum, this should include the scientific motivation and scope, 
name(s) of maintainer(s), links to project homepage, and a list of a few
key publications. 
relevant publications.

* `/tests/testthat/`

Each package should have tests that cover the key functions of the package, 
at a minimum, the three functions above.

* documentation

Update the `NAMESPACE`, `DESCRIPTION` and `man/*.Rd` files by running 

```r
devtools("models/<modelname>/")
```

---
**NOTE: Read the instructions above and delete them when done.**
**Below is the boilerplate README template to fill out for your model:**
---

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
- [Link to a demo or vignette that lets a user who has already run Demo 1 get started with this model. If no such demo exists, consider including a brief tutorial directly in this README.]
- [MODEL in the PEcAn Book](https://pecanproject.github.io/pecan-documentation/develop/models-MODEL.html)


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
