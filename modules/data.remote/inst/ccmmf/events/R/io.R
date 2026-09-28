# Keep only SIPNET / PEcAn event columns (drop source / diagnostic fields).
keep_event_columns <- function(dt, cols) {
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0L) {
    stop("Missing SIPNET event columns: ", paste(missing, collapse = ", "))
  }
  dt <- dt[, cols, with = FALSE]
  data.table::setcolorder(dt, cols)
  dt
}

# Write canonical parquet + PEcAn site-nested JSON for one event type.

write_event_outputs <- function(dt, out_dir, kind, year, site_col = "site_id", json_builder) {
  paths <- event_output_paths(out_dir, kind, year)
  arrow::write_parquet(dt, paths$parquet)
  message("  Wrote ", paths$parquet, " (", nrow(dt), " rows)")

  by_site <- split(dt, dt[[site_col]])
  json_list <- lapply(by_site, function(rows) {
    lapply(seq_len(nrow(rows)), function(i) json_builder(rows, i))
  })
  write(jsonlite::toJSON(json_list, auto_unbox = TRUE, pretty = TRUE), paths$json)
  message("  Wrote ", paths$json)
  invisible(paths)
}

# Copy an apply-script parquet into event_files (no recomputation).
copy_parquet_to_event_files <- function(parquet_path, out_dir, kind, year,
                                        json_builder, missing_hint = NULL,
                                        prep = NULL) {
  if (!file.exists(parquet_path)) {
    stop(
      kind, " table not found: ", parquet_path,
      if (!is.null(missing_hint)) paste0("\n", missing_hint) else ""
    )
  }
  dt <- data.table::as.data.table(arrow::read_parquet(parquet_path))
  message("[", kind, "] Loaded ", nrow(dt), " rows from ", parquet_path)
  if (is.function(prep)) {
    dt <- prep(dt)
  }
  if (nrow(dt) && !"event_type" %in% names(dt)) {
    dt[, event_type := kind]
  }
  if (nrow(dt) && !"site_id" %in% names(dt) && "parcel_id" %in% names(dt)) {
    dt[, site_id := parcel_id]
  }
  write_event_outputs(dt, out_dir, kind, year, json_builder = json_builder)
  invisible(dt)
}
