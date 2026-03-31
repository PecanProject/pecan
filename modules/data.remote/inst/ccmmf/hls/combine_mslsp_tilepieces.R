#!/usr/bin/env Rscript
# Combine per-tile MSLSP tilepieces into one annual Parquet for a year.
# Aggregates duplicate parcels at tile boundaries with weighted means and QA modes.
#
# Main inputs: prep from mslsp_prep_static_tilewise(year); tilepieces under tilepieces_year=.
# Main outputs: mslsp_year=Y.parquet in the year output directory.
# How to run: Rscript combine_mslsp_tilepieces.R <year> [overwrite], or via driver combine.
# Workflow: monitoring workflow merge step for MSLSP.

script_dir <- if (length(file_arg <- commandArgs(trailingOnly = FALSE)[grepl("^--file=",
                   commandArgs(trailingOnly = FALSE))])) {
  dirname(sub("^--file=", "", file_arg[1]))
} else "."
source(file.path(script_dir, "extract_summary_core.R"))
source(file.path(script_dir, "tilewise_mslsp_implementation.R"))

#### Combine function
# Read all tilepieces for a year, aggregate cross-tile duplicates, write Parquet.
# Weighted aggregation across tiles (when a parcel spans multiple tiles):
#   metric mean: SUM(w_valid * metric_mean) / SUM(w_valid)
#   metric sd:   derived from parallel variance identity
#   QA mode:     weighted mode (tile with greatest coverage area wins)
#   na_frac:     weighted mean of per-tile na_frac values
mslsp_combine <- function(prep, time_key, overwrite = FALSE, verbose = TRUE) {
  year <- prep$year

  tilepieces_dir <- file.path(prep$out_dir, sprintf("tilepieces_year=%d", year))
  out_path       <- file.path(prep$out_dir, sprintf("mslsp_year=%d.parquet", year))

  if (file.exists(out_path) && !overwrite) {
    if (verbose) message("[combine] skip (exists): ", out_path)
    return(invisible(out_path))
  }

  tile_files <- list.files(tilepieces_dir, "^tile=.*\\.csv\\.gz$", full.names = TRUE)
  if (length(tile_files) == 0) stop("No tilepieces found in: ", tilepieces_dir)
  if (verbose) message("[combine] ", length(tile_files), " tilepieces -> ", out_path)

  dt <- rbindlist(lapply(tile_files, function(f) {
    tryCatch(fread(f, showProgress = FALSE), error = function(e) NULL)
  }), fill = TRUE, use.names = TRUE)

  if (nrow(dt) == 0) {
    if (verbose) message("[combine] no rows - writing empty parquet")
    arrow::write_parquet(dt, out_path)
    return(invisible(out_path))
  }

  agg <- mslsp_aggregate_tilepieces(dt, year)
  arrow::write_parquet(agg, out_path)
  if (verbose) message("[combine] wrote ", nrow(agg), " rows")
  invisible(out_path)
}

#### CLI

if (sys.nframe() == 0) {
  args           <- commandArgs(trailingOnly = TRUE)
  is_ow          <- function(x) tolower(x) %in% c("true", "t", "yes", "y", "overwrite")
  overwrite_flag <- any(sapply(args, is_ow))

  if (length(args) < 1) stop(
    "Usage: Rscript combine_mslsp_tilepieces.R <year> [overwrite]"
  )

  year_arg <- as.integer(args[1])
  prep     <- mslsp_prep_static_tilewise(year_arg)
  mslsp_combine(prep, 1L, overwrite = overwrite_flag)
}
