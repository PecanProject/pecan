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
