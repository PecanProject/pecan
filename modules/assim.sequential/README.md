
# PEcAnAssimSequential

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAnAssimSequential status badge](https://pecanproject.r-universe.dev/badges/PEcAnAssimSequential)](https://pecanproject.r-universe.dev/PEcAnAssimSequential)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAnAssimSequential` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAnAssimSequential in R
install.packages('PEcAnAssimSequential')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/assim.sequential")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAnAssimSequential)
## basic example code
```


## Predict NEE and LE period maps

`predict_flux_maps()` applies a trained three-fold model to a window on the original
north-up EPSG:4326 grid. It returns one GeoTIFF path per variable and period.
Supply the model bundle and environmental inputs separately.

Use a Linux Python 3.12 environment. Install the CPU or CUDA build of PyTorch
2.12.1 in that environment, then install the matching requirements file from
`system.file("python", "pecan_downscaling", package = "PEcAnAssimSequential")`
with `python -m pip install -r requirements.txt` (CPU) or
`python -m pip install -r requirements-cuda.txt` (CUDA 13, one visible GPU).
The predictor uses Linux shared memory on both devices. Set `resources.device`
and size `workers` and `cpu_threads` for the allocation.

```r
library(PEcAnAssimSequential)
example <- system.file("flux-maps", "predict.yaml", package = "PEcAnAssimSequential")
file.copy(example, "map-request.yaml")
# Edit the YAML to point to the model bundle, map inputs, and a fresh output directory.
maps <- predict_flux_maps("map-request.yaml", python = "/path/to/env/bin/python")
nee <- terra::rast(maps$path[maps$variable == "NEE"][1])
```

The YAML resolves relative paths beside the configuration. The window uses
zero-based row and column offsets. The calendar must cover each requested
period and its four preceding periods.

| Input | Expected data |
| --- | --- |
| Model and calendar | Bundle manifest with three proxy networks, six residual forests and three monthly spatial corrections. Calendar CSV with `period_id`, `period_key`, `start` and exclusive `stop` in UTC. |
| Meteorology and soil | Yearly three-hour ERA5 ensemble NetCDF files (including the final accumulation boundary). GLDAS four-layer soil caches per period, with their source metadata and referenced files. |
| Vegetation | Dated Aqua MYD13Q1 tile ledger with NDVI/EVI layers, original files and receipts. Supply the surrounding 64 days for gap filling. |
| Grid and land | Reference grid, native land cover, climate classification raster and legend, and both annual covariate stacks. Annual proxy bands are LC=1, disturbance age=2, biomass=3, pH=8, N=9, SOC=10, sand=11, elevation=15. Native bands are age=2, biomass=3, wetness=4, pH=9, N=10, SOC=11, sand=12. The annual 2013–2024 rasters define the land domain. |
| Timezones and optional caches | Supply pixel timezones or verified point blocks. With neither, install R package `lutz` for the `Rscript` lookup. A verified land domain, matching monthly spatial cache, and a writable rolling panel cache are optional. |

Band 1 is the mean over three folds. Band 2 is their population standard deviation,
bands 3–5 are the fold predictions, band 6 marks eligible pixels, and band 7 records
missing inputs (1 for proxy/context, 2 for native category, 4 for timezone).
Water is NoData. LE uses W/m² and NEE follows the Yang training convention.
Use `resume = TRUE` after an interruption with the same configuration, model,
sources and backend version. Start a fresh run when migrating from another backend.

From the package source directory, run the R tests with `devtools::test()`.
In the prediction environment with pytest installed, run
`python -m pytest -q tests/python` for the fixture-based backend checks.
To compare a migrated model against reference products, use the same model,
inputs, grid window and device for both runs, then execute
`python tests/integration/compare_flux_maps.py reference/products/period candidate/products/period`.
