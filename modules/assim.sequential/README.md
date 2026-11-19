
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

## Prepare Yang station targets

Two R functions connect a PEcAn ensemble to the Yang station targets. Each
has its own entry point. If annual site means already exist, start with
`prepare_flux_targets()`. Both run locally on the CPU.

```r
library(PEcAnAssimSequential)
site_map <- read.csv("pecan-to-station.csv", colClasses = c(site_id = "character"))
write_site_flux_means(settings, years = 2012:2013, site_map = site_map, outdir = "annual-means")

template <- system.file("flux-targets", "prepare.yaml", package = "PEcAnAssimSequential")
file.copy(template, "stations.yaml")
# Set sources.site_means to annual-means and fill in the other source paths.
targets <- prepare_flux_targets("stations.yaml", "yang-output", models = "residual-models",
                                python = "/path/to/env/bin/python")
```

The first function reads Ensemble records from `settings$outdir/runs_manifest.csv`
and fluxes from `settings$modeloutdir/<run_id>/<year>.nc`. Pair each PEcAn
`site_id` with its Yang station `index`. All sites must have the same set of
ensemble members and a common three-hour UTC clock for the requested year.
CF time bounds identify interval starts. Without bounds, the labels must be
three-hour UTC starts. For SIPNET output with drifting labels, pass a named
vector of `sipnet.clim` paths in `forcing`, keyed by run ID. Each must describe
the same UTC year, day of year, fractional hour and three-hour duration as the
model output. The writer checks the run-specific forcing before assigning a slot.

Each `year=YYYY.rds` contains station indices, POSIXct interval starts, and
site-by-time means for NEE (kg C m⁻² s⁻¹, positive to atmosphere) and LE
(W m⁻², positive to atmosphere). Each flux averages its finite model members.
Each year is published separately. If a later year fails, retry the missing years.
The number of model runs used here is independent of the 25 Yang members.

For the second function install the packages in
`inst/python/pecan_downscaling/requirements-targets.txt` in a Python 3.12
environment. Its R process uses arrow, data.table, dplyr, jsonlite and
**R xgboost 1.7.8.1**. Fitting also uses caret. Supply the following inputs
through the YAML. Relative paths resolve beside the YAML.

| Source | Station input |
| --- | --- |
| Station inventory and timezones | CSVs with unique `index,site_col,lat,lon` and `index,tz` (IANA timezone). Ascending `site_col` determines the HDF5 site axis. |
| Period calendar | Yang site-period CSV with `period_id,period_key,start_date,end_date,expected_3hour_steps,full_window_expected_steps,full_window_fraction`. Start and end dates are inclusive UTC dates. Short initial and terminal periods retain their actual length. |
| Towers | Match CSV with `index,Site_ID,min_dist_m`. Each gap-filled `target=NEE/site=<Site_ID>.parquet` or `target=LE/site=<Site_ID>.parquet` contains `Site_ID`, timezone-aware UTC `utc` and numeric `ens01` through `ens25` in the corresponding target units. |
| Weather and soil | ERA5 ensemble files `ERA5_YYYY.nc` with temperature and dew point in K, surface pressure in Pa, precipitation in m, accumulated radiation in J/m² and wind in m/s. The final interval needs the next three-hour accumulation boundary. Raw GLDAS three-hour files provide four instantaneous soil layers in kg/m². An optional repair registry maps UTC steps to SHA-256-verified replacements. |
| Vegetation and land | AppEEARS `*MYD13Q1*results.csv` point exports with Latitude, Longitude, Date and decoded 250 m EVI/NDVI. Native `covariates_YYYY.tiff` bands are disturbance age=2, biomass=3, wetness=4, pH=9, N=10, SOC=11 and sand=12. Supply the native land-cover and climate rasters and `code,KGC` legend. |

Omit `models` to fit and save one residual bundle per target and tower member
in `models/`. Training uses matched stations within 1,000 m and the periods in
the YAML. Supply existing bundles (`NEE_ens01.rds` through `LE_ens25.rds`, each
with groups 1–8 and EC) to apply fixed models. Residuals use **model mean minus
tower member**. When reading annual means, NEE is scaled once by
`1e8 / 1.157407`, and LE retains W/m². The raw-value magnitude cutoff is
1e6 in target units.

The output directory appears when both fluxes finish. `features/` contains
three-hour station predictors and `members/` contains each flux's raw mean,
25 target values and 25 source codes, keyed by `index,utc`. Files are split by
calendar year, station group and period. Each finite matched tower value takes
precedence (route 1). Otherwise a fitted residual corrects the raw mean
(route 2), using EC at matched sites and land cover elsewhere. A finite raw
mean supplies route 3, and a missing target has route 0.

`targets/nee.h5` and `targets/le.h5` store `values`, `support_counts` and
`route_codes` on the labelled period × member × station axes. Values average
finite steps for that member and flux. A tie for the most frequent source
favors the lower nonzero code. A period with zero support has a missing mean
and route 0. `expected_available_steps` and `full_window_fraction` describe
the calendar window independently of observed support.
