#!/usr/bin/env Rscript
# EVImax -> LAI -> C/N pools for planting.
# Writes $MATCHED_DIR/assigned_year=Y_planting.parquet (LAI stays in memory).
# Does not write event files.
#
# Math: lai_from_mslsp.R, pool_calculations_from_lookup.R (via trait pool).
# Table builders: planting_apply.R
# Usage: Rscript apply_planting.R <year>

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: Rscript apply_planting.R <year>")
}
year_arg <- as.integer(args[1L])
if (is.na(year_arg)) {
  stop("Year must be an integer, got: ", args[1L])
}

events_root <- trimws(Sys.getenv("EVENTS_ROOT", ""))
code <- trimws(Sys.getenv("CCMMF_CODE", ""))
if (!nzchar(events_root)) {
  if (!nzchar(code)) {
    stop("Set EVENTS_ROOT or CCMMF_CODE (source documentation/setup_env.sh).")
  }
  events_root <- file.path(code, "events")
}
traits_root <- trimws(Sys.getenv("TRAITS_ROOT", ""))
if (!nzchar(traits_root)) {
  if (!nzchar(code)) {
    stop("Set TRAITS_ROOT or CCMMF_CODE.")
  }
  traits_root <- file.path(code, "traits")
}

source(file.path(events_root, "R", "paths.R"))
source(file.path(events_root, "R", "matched_input.R"))
source(file.path(events_root, "R", "trait_pool.R"))
source(file.path(traits_root, "planting_apply.R"))

paths <- events_paths()
pool <- load_events_trait_pool(paths$pool_script)
matched <- load_matched_for_events(
  year_arg, paths$matched_dir, run_planting = TRUE
)

lai_dt <- build_planting_lai_table(matched, pool$pool_env, year_arg, paths)
message("[lai] ", nrow(lai_dt), " rows in memory (not written)")
if (nrow(lai_dt)) {
  print(lai_dt[, .N, by = .(PFT, lai_source)])
}

pools_dt <- build_planting_pool_table(lai_dt, pool$pool_env, pool$lk)
pools_out <- planting_table_path(paths$matched_dir, year_arg)
dir.create(dirname(pools_out), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(pools_dt, pools_out)
message("[planting] wrote ", nrow(pools_dt), " rows: ", pools_out)
if (nrow(pools_dt)) {
  print(pools_dt[, .(
    n = .N,
    mean_LAI = mean(LAI, na.rm = TRUE),
    mean_C_LEAF = mean(C_LEAF, na.rm = TRUE)
  ), by = PFT])
}
