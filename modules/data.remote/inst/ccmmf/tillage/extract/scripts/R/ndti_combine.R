# Combine per-tile tilepieces into a final monthly parquet file.

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
  )

  all_tiles <- data.table::rbindlist(
    lapply(tile_files, function(f) {
      dt <- tryCatch(data.table::fread(f, colClasses = col_types), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      dt
    }),
    fill = TRUE
  )

  all_tiles <- all_tiles[!is.na(ndti_mean)]
  all_tiles[, date := as.Date(date)]

  if (nrow(all_tiles) == 0) {
    if (verbose) message("[combine] no non-null rows — writing empty parquet")
    result <- data.table::data.table(
      parcel_id = character(), year = integer(),
      date      = as.Date(integer(0), origin = "1970-01-01"),
      ndti_mean = double(), ndti_sd = double(),
      n_valid   = integer(), w_valid = double(),
      sum_w2    = double(),  na_frac = double()
    )
    arrow::write_parquet(result, out_path)
    return(invisible(out_path))
  }

  all_tiles[, w_total := data.table::fifelse(!is.na(na_frac) & na_frac < 1,
                                             w_valid / (1 - na_frac), 0)]

  result <- all_tiles[, {
    sw  <- sum(w_valid)
    mu  <- sum(w_valid * ndti_mean) / sw
    wt  <- sum(w_total)
    .(
      ndti_mean = mu,
      ndti_sd   = sqrt(pmax(0,
        sum(w_valid * (ndti_sd^2 + ndti_mean^2)) / sw - mu^2
      )),
      n_valid   = sum(as.integer(n_valid)),
      w_valid   = sw,
      sum_w2    = sum(sum_w2),
      na_frac   = data.table::fifelse(wt > 0, 1 - sw / wt, NA_real_)
    )
  }, by = .(parcel_id, date)]

  data.table::setorder(result, parcel_id, date)
  result[, year := year]
  data.table::setcolorder(result, c("parcel_id", "year", "date",
                        "ndti_mean", "ndti_sd", "n_valid", "w_valid", "sum_w2", "na_frac"))

  arrow::write_parquet(result, out_path)
  if (verbose) message("[combine] wrote ", nrow(result), " rows")
  invisible(out_path)
}
