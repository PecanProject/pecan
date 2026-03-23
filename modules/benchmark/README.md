## Quickstart: run_benchmark()

`run_benchmark()` is a simple entry point that loads model output and
observations, aligns them by time, computes metrics, and returns a plot.

### Input format

Both input files must be CSV with two columns:
- `time` — timestamp (e.g. `2020-01-01 00:00:00`)
- `value` — numeric variable value

### Usage
```r
library(PEcAn.benchmark)

res <- run_benchmark(
  model_path = "inst/testdata/sample_model.csv",
  obs_path   = "inst/testdata/sample_obs.csv"
)

# View metrics
print(res$metrics)
#   metric     value
# 1   RMSE 0.1322876
# 2    MAE 0.1250000

# View plot
res$plot
```

### Parameters

- `model_path` — path to model output CSV
- `obs_path` — path to observations CSV  
- `metrics` — vector of metrics to compute: `"RMSE"`, `"MAE"` (default: both)
- `tolerance_secs` — max time difference for matching (default: 3600 seconds)
# PEcAn.benchmark

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![PEcAn.benchmark status badge](https://pecanproject.r-universe.dev/badges/PEcAn.benchmark)](https://pecanproject.r-universe.dev/PEcAn.benchmark)

<!-- badges: end -->

PEcAn Functions Used for Benchmarking

## Installation

You can install the development version of `PEcAn.benchmark` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.benchmark in R
install.packages('PEcAn.benchmark')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "modules/benchmark")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.benchmark)
## basic example code
```

