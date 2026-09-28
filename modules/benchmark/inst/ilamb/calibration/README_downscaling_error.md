# Downscaling error versus ensemble spread

`downscaling_error.R` compares the downscaling model's own predictive error
against the spread of the SDA ensemble, at the assimilation sites, in each
variable's own units.

## What it computes

The State Data Assimilation (SDA) produces an ensemble of member fields. Each
member is downscaled to a 1 km grid by its own random forest, fit to that
member's values at the pre-selected sites. For a chosen variable and year the
script reports:

- **between-member spread** — the standard deviation across ensemble members,
  averaged over sites. This is the uncertainty the ensemble represents.
- **downscaling OOB RMSE** — the random forest out-of-bag error, averaged over
  members. This is the downscaling model's own predictive error.
- **ratio** — OOB RMSE divided by spread.

When the OOB RMSE is larger than the spread, the members agree with each other
more tightly than the downscaling is actually accurate, so the downscaled maps
carry error that the ensemble spread does not represent. This is a distinct
uncertainty source from the ensemble spread itself.

## Inputs

Per variable and year, an `.Rdata` file containing an object named `models`,
a list of per-member `randomForest` regression objects (as written by the
downscaling step), found under `<model_dir>/<variable>_<year>/ml_models.Rdata`.
The out-of-bag RMSE is read from each fit's `mse` vector and the between-member
spread from each fit's training response `y`.

The default paths reflect one particular SDA setup and are illustrative. Point
`--model-dir` at your own downscaling output.

## Running

    Rscript downscaling_error.R \
      --model-dir /path/to/downscale_maps_analysis_lc_ts_noGEDI_rf \
      --variables AbvGrndWood,TotSoilCarb \
      --year 2015

Output is one row per variable with the member count, site count, spread,
OOB RMSE, and ratio. With no arguments the script uses the defaults in the
`config` block.

## Note on language

The other diagnostics in this directory are Python. This script is R because
the downscaling models are R `randomForest` objects saved in `.Rdata`, which R
reads natively; a Python port would only shell out to R to read them.

## Testing

    Rscript test_downscaling_error.R

The test builds small synthetic `randomForest` objects, saves them as `models`,
and checks the out-of-bag and spread extraction, the ratio, and the error paths
on data with known structure. It requires the `randomForest` package.
