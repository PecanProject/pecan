#!/usr/bin/env Rscript
# Combine per-tile NDTI tilepieces into one monthly Parquet for a year and month.
# Aggregates duplicate (parcel_id, date) rows with weighted mean and variance rules.
#
# Main inputs: prep from ndti_prep_static_tilewise(year); tilepieces for that month.
# Main outputs: ndti_year=Y_month=MM.parquet under the year directory.
# How to run: Rscript combine_ndti_tilepieces.R <year> <month> [overwrite], or via driver.
# Workflow: monitoring workflow merge step for NDTI.

script_dir <- if (length(file_arg <- commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))])) {
  dirname(sub("^--file=", "", file_arg[1]))
} else "."
source(file.path(script_dir, "tilewise_ndti_implementation.R"))

#### Combine function
# Read all tilepieces for one month, aggregate cross-tile duplicates, write parquet.
# Weighted aggregation across tiles:
#   mean:    sum(w * x) / sum(w)
#   sd:      via parallel variance identity: Var = E[X^2] - E[X]^2
#   na_frac: recover w_total = w_valid / (1 - na_frac) per tile row, then
#            re-derive na_frac = 1 - sum(w_valid) / sum(w_total) across tiles
ndti_combine <- function(prep, month, overwrite = FALSE, verbose = TRUE) {
  year  <- prep$year
  month <- as.integer(month)

  out_path <- path_monthly_output(prep$out_dir, year, month)
  if (file.exists(out_path) && !overwrite) {
    if (verbose) message("[combine] skip (exists): ", out_path)
    return(invisible(out_path))
  }

  tilepieces_dir <- path_tilepieces(prep$out_dir, year, month)
  tile_files     <- list.files(tilepieces_dir, "^tile=.*\\.csv\\.gz$", full.names = TRUE)
  if (length(tile_files) == 0) stop("No tilepieces found in: ", tilepieces_dir)
  if (verbose) message("[combine] ", length(tile_files), " tilepieces -> ", out_path)

  col_types <- list(
    character = "parcel_id",
    integer   = "n_valid",
    double    = c("ndti_mean", "ndti_sd", "w_valid", "sum_w2", "na_frac")
    # date and parcel_id read as character; date is cast after rbindlist
  )

  # Read all tiles; fread handles .gz natively
  all_tiles <- rbindlist(
    lapply(tile_files, function(f) {
      dt <- tryCatch(fread(f, colClasses = col_types), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      dt
    }),
    fill = TRUE
  )

  all_tiles <- all_tiles[!is.na(ndti_mean)]
  all_tiles[, date := as.Date(date)]

  if (nrow(all_tiles) == 0) {
    if (verbose) message("[combine] no non-null rows - writing empty parquet")
    result <- data.table(
      parcel_id = character(), year = integer(),
      date      = as.Date(integer(0), origin = "1970-01-01"),
      ndti_mean = double(), ndti_sd = double(),
      n_valid   = integer(), w_valid = double(),
      sum_w2    = double(),  na_frac = double()
    )
    arrow::write_parquet(result, out_path)
    return(invisible(out_path))
  }

  # Recover per-row w_total for na_frac re-derivation across tiles
  all_tiles[, w_total := fifelse(!is.na(na_frac) & na_frac < 1,
                                 w_valid / (1 - na_frac), 0)]

  result <- all_tiles[, {
    sw  <- sum(w_valid)
    mu  <- sum(w_valid * ndti_mean) / sw
    wt  <- sum(w_total)
    .(
      ndti_mean = mu,
      # parallel variance: E[X^2] - E[X]^2
      ndti_sd   = sqrt(pmax(0,
        sum(w_valid * (ndti_sd^2 + ndti_mean^2)) / sw - mu^2
      )),
      n_valid   = sum(as.integer(n_valid)),
      w_valid   = sw,
      sum_w2    = sum(sum_w2),
      na_frac   = fifelse(wt > 0, 1 - sw / wt, NA_real_)
    )
  }, by = .(parcel_id, date)]

  setorder(result, parcel_id, date)
  result[, year := year]
  setcolorder(result, c("parcel_id", "year", "date",
                        "ndti_mean", "ndti_sd", "n_valid", "w_valid", "sum_w2", "na_frac"))

  arrow::write_parquet(result, out_path)
  if (verbose) message("[combine] wrote ", nrow(result), " rows")
  invisible(out_path)
}

#### CLI

if (sys.nframe() == 0) {
  args           <- commandArgs(trailingOnly = TRUE)
  is_ow          <- function(x) tolower(x) %in% c("true", "t", "yes", "y", "overwrite")
  overwrite_flag <- any(sapply(args, is_ow))

  if (length(args) < 2) stop(
    "Usage: Rscript combine_ndti_tilepieces.R <year> <month 1-12> [overwrite]"
  )

  year_arg  <- as.integer(args[1])
  month_arg <- suppressWarnings(as.integer(args[2]))
  if (is.na(month_arg) || month_arg < 1L || month_arg > 12L) stop("Month must be 1-12")

  prep <- ndti_prep_static_tilewise(year_arg)
  ndti_combine(prep, month_arg, overwrite = overwrite_flag)
}
