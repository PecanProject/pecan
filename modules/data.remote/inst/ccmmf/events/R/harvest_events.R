# Harvest events: SIPNET columns from assigned_year=Y_harvest.parquet.
# Apply: traits/apply_harvest.R (does not recompute here).

.harvest_event_cols <- c(
  "event_type",
  "site_id",
  "date",
  "frac_above_removed_0to1",
  "frac_below_removed_0to1",
  "frac_above_to_litter_0to1",
  "frac_below_to_litter_0to1"
)

harvest_json_builder <- function(rows, i) {
  list(
    event_type = rows$event_type[i],
    date = rows$date[i],
    frac_above_removed_0to1 = rows$frac_above_removed_0to1[i],
    frac_below_removed_0to1 = rows$frac_below_removed_0to1[i],
    frac_above_to_litter_0to1 = rows$frac_above_to_litter_0to1[i],
    frac_below_to_litter_0to1 = rows$frac_below_to_litter_0to1[i]
  )
}

build_harvest_events <- function(year, out_dir, matched_dir) {
  harvest_file <- harvest_table_path(matched_dir, year)
  copy_parquet_to_event_files(
    harvest_file, out_dir, "harvest", year,
    json_builder = harvest_json_builder,
    missing_hint = paste0(
      "Run Sec. 2.7 first:\n",
      "  Rscript $CCMMF_CODE/traits/apply_harvest.R ", year
    ),
    prep = function(dt) {
      if (!"site_id" %in% names(dt) && "parcel_id" %in% names(dt)) {
        dt[, site_id := parcel_id]
      }
      dt[, event_type := "harvest"]
      dt <- keep_event_columns(dt, .harvest_event_cols)
      if (nrow(dt)) {
        data.table::setorder(dt, site_id, date)
      }
      dt
    }
  )
}
