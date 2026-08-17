#!/usr/bin/env Rscript
#
# Tests for downscaling_error.R.
#
# Builds a small ensemble of real randomForest objects on synthetic site data
# with known structure, saves them as `models`, and checks that the extraction
# reads the out-of-bag error and the between-member spread correctly, that the
# ratio is consistent, and that the error paths fire. Mirrors the synthetic-
# data approach used by test_regional_diagnostics.py.
#
# Requires the randomForest package. Run:  Rscript test_downscaling_error.R

suppressWarnings(suppressMessages(library(randomForest)))
source("downscaling_error.R")

set.seed(42)

n_sites   <- 60
n_members <- 6

# Each member is a slightly shifted realization of the same site pattern, so
# the members disagree (non-zero between-member spread) while each is a valid
# regression the forest can partially fit (non-zero, finite OOB).
make_member <- function(shift) {
  x <- data.frame(a = rnorm(n_sites), b = rnorm(n_sites))
  y <- 3 * x$a - 2 * x$b + shift + rnorm(n_sites, sd = 0.5)
  randomForest(x = x, y = y, ntree = 100)
}
models <- lapply(seq_len(n_members), function(i) make_member(shift = i * 2))

# --- oob_rmse: positive finite scalar, and equals sqrt of final mse ---------
o1 <- oob_rmse(models[[1]])
stopifnot(is.numeric(o1), length(o1) == 1L, is.finite(o1), o1 > 0)
stopifnot(abs(o1 - sqrt(tail(models[[1]]$mse, 1))) < 1e-12)

# --- downscaling_error: shapes and consistency ------------------------------
res <- downscaling_error(models)
stopifnot(res$n_members == n_members)
stopifnot(res$n_sites   == n_sites)
stopifnot(is.finite(res$spread),   res$spread   > 0)
stopifnot(is.finite(res$oob_rmse), res$oob_rmse > 0)
# ratio is exactly oob_rmse / spread
stopifnot(abs(res$ratio - res$oob_rmse / res$spread) < 1e-12)
# mean OOB equals the mean of per-member OOB computed independently
oob_each <- vapply(models, oob_rmse, numeric(1))
stopifnot(abs(res$oob_rmse - mean(oob_each)) < 1e-12)
# spread equals mean over sites of the per-site SD across members
Y <- vapply(models, function(m) as.numeric(m$y), numeric(n_sites))
stopifnot(abs(res$spread - mean(apply(Y, 1, sd))) < 1e-12)

# --- load_models: round-trip from an .Rdata file ----------------------------
tmp <- tempfile(fileext = ".Rdata")
save(models, file = tmp)
loaded <- load_models(tmp)
stopifnot(length(loaded) == n_members)
stopifnot(abs(downscaling_error(loaded)$ratio - res$ratio) < 1e-12)

# --- error path: file present but no 'models' object ------------------------
# (inherits = FALSE in load_models means a 'models' in the caller's scope must
#  not mask a genuinely missing object in the file)
tmp2 <- tempfile(fileext = ".Rdata")
other <- 1
save(other, file = tmp2)
err <- tryCatch({ load_models(tmp2); "NO ERROR" },
                error = function(e) "errored")
stopifnot(identical(err, "errored"))

# --- error path: object without $mse ----------------------------------------
bad <- structure(list(y = rnorm(10)), class = "randomForest")
err2 <- tryCatch({ oob_rmse(bad); "NO ERROR" },
                 error = function(e) "errored")
stopifnot(identical(err2, "errored"))

# --- error path: members with differing site counts -------------------------
mismatched <- list(models[[1]],
                   randomForest(x = data.frame(a = rnorm(n_sites + 5),
                                               b = rnorm(n_sites + 5)),
                                y = rnorm(n_sites + 5), ntree = 50))
err3 <- tryCatch({ downscaling_error(mismatched); "NO ERROR" },
                 error = function(e) "errored")
stopifnot(identical(err3, "errored"))

cat("all tests passed\n")
