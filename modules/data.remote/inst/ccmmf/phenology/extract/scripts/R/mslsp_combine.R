# Combine per-tile tilepieces into the final annual MSLSP Parquet.

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

  dt <- data.table::rbindlist(lapply(tile_files, function(f) {
    tryCatch(data.table::fread(f, showProgress = FALSE), error = function(e) NULL)
  }), fill = TRUE, use.names = TRUE)

  if (nrow(dt) == 0) {
    if (verbose) message("[combine] no rows -- writing empty parquet")
    arrow::write_parquet(dt, out_path)
    return(invisible(out_path))
  }

  agg <- mslsp_aggregate_tilepieces(dt, year)
  arrow::write_parquet(agg, out_path)
  if (verbose) message("[combine] wrote ", nrow(agg), " rows")
  invisible(out_path)
}
