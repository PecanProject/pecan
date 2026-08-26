#!/usr/bin/env Rscript
#
# downscaling_error.R
#
# Compare the downscaling model's own predictive error against the spread of
# the SDA ensemble, at the assimilation sites, in each variable's own units.
#
# The State Data Assimilation (SDA) produces an ensemble of member fields.
# Each member is downscaled to a 1 km grid by its own random forest, fit to
# that member's values at the pre-selected sites. For a chosen variable and
# year this script reports three magnitudes:
#
#   - between-member spread: the standard deviation across ensemble members,
#     averaged over sites. This is the uncertainty the ensemble represents.
#   - downscaling OOB RMSE: the random forest out-of-bag error, averaged over
#     members. This is the downscaling model's own predictive error.
#   - their ratio (OOB RMSE / spread).
#
# When the OOB RMSE is larger than the spread, the members agree with each
# other more tightly than the downscaling is actually accurate, so the
# downscaled maps carry error that the ensemble spread does not represent.
#
# If the saved model objects were written under a namespace that is not
# installed (e.g. PEcAnAssimSequential, which wrote the reanalysis forests),
# loading prints a harmless namespace warning; the script reads only the
# standard randomForest fields, so it works either way.

suppressWarnings(suppressMessages(
  try(library(PEcAnAssimSequential), silent = TRUE)
))

## ---- helpers -------------------------------------------------------------

# OOB RMSE for a single randomForest regression object: the square root of the
# final out-of-bag MSE (last element of the per-tree mse vector).
oob_rmse <- function(rf) {
  if (is.null(rf$mse)) {
    stop("object has no $mse; is it a randomForest regression fit?")
  }
  sqrt(utils::tail(rf$mse, 1))
}

# Given a list of per-member randomForest objects for one variable and year,
# return the mean OOB RMSE over members and the between-member spread, both in
# the variable's own units, plus their ratio.
#
# The between-member spread is computed from each member's training target y:
# stack y across members (sites x members), take the standard deviation across
# members at each site, then average over sites.
downscaling_error <- function(models) {
  if (!length(models)) stop("no models supplied")

  oob <- vapply(models, oob_rmse, numeric(1))

  ylens <- vapply(models, function(m) length(m$y), integer(1))
  if (length(unique(ylens)) != 1L) {
    stop("members have differing numbers of training sites; cannot align spread")
  }

  Y <- vapply(models, function(m) as.numeric(m$y), numeric(ylens[1]))
  if (is.null(dim(Y))) Y <- matrix(Y, ncol = length(models))  # single-site guard

  persite_sd   <- apply(Y, 1, stats::sd)
  persite_mean <- apply(Y, 1, mean)

  list(
    n_members   = length(models),
    n_sites     = nrow(Y),
    spread      = mean(persite_sd),
    mean_level  = mean(persite_mean),
    oob_rmse    = mean(oob),
    oob_rmse_sd = stats::sd(oob),
    ratio       = mean(oob) / mean(persite_sd)
  )
}

# Load the per-member models for one variable/year from an .Rdata file that
# contains an object named `models` (a list of randomForest objects).
load_models <- function(path) {
  if (!file.exists(path)) stop(sprintf("model file not found: %s", path))
  e <- new.env()
  load(path, envir = e)
  if (!exists("models", envir = e, inherits = FALSE)) {
    stop(sprintf("object 'models' not found in %s (found: %s)",
                 path, paste(ls(e), collapse = ", ")))
  }
  get("models", envir = e, inherits = FALSE)
}

## ---- config --------------------------------------------------------------

# Paths reflect one particular SDA setup and are illustrative; adapt them to
# your own data. Each variable maps to the .Rdata file holding its per-member
# random forests for the chosen year, under <model_dir>/<variable>_<year>/.
DEFAULT_MODEL_DIR <- file.path(
  "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site",
  "downscale_maps_analysis_lc_ts_noGEDI_rf"
)
DEFAULT_VARIABLES <- c("AbvGrndWood", "TotSoilCarb")
DEFAULT_YEAR      <- 2015
MODEL_FILE_NAME   <- "ml_models.Rdata"

model_file <- function(model_dir, variable, year) {
  file.path(model_dir, sprintf("%s_%d", variable, year), MODEL_FILE_NAME)
}

## ---- driver --------------------------------------------------------------

run <- function(model_dir = DEFAULT_MODEL_DIR,
                variables  = DEFAULT_VARIABLES,
                year       = DEFAULT_YEAR) {
  cat(sprintf("Downscaling error vs ensemble spread  (year %d)\n", year))
  cat(sprintf("%-14s %8s %8s %10s %10s %8s\n",
              "variable", "members", "sites", "spread", "OOB_RMSE", "ratio"))
  for (v in variables) {
    res <- downscaling_error(load_models(model_file(model_dir, v, year)))
    cat(sprintf("%-14s %8d %8d %10.3f %10.3f %8.3f\n",
                v, res$n_members, res$n_sites,
                res$spread, res$oob_rmse, res$ratio))
  }
  invisible(NULL)
}

## ---- command line --------------------------------------------------------
# Runs only when the file is executed directly (Rscript downscaling_error.R),
# not when sourced (e.g. by the test), so the functions can be reused.

if (identical(environment(), globalenv()) && !length(sys.calls())) {
  args <- commandArgs(trailingOnly = TRUE)
  get_arg <- function(flag, default) {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) return(default)
    args[i + 1]
  }
  run(
    model_dir = get_arg("--model-dir", DEFAULT_MODEL_DIR),
    variables = strsplit(get_arg("--variables",
                                 paste(DEFAULT_VARIABLES, collapse = ",")),
                         ",")[[1]],
    year      = as.integer(get_arg("--year", as.character(DEFAULT_YEAR)))
  )
}
