# Tillage events: SIPNET columns from assigned_year=Y_tillage.parquet.
# Apply: tillage/apply_tillage.R (does not recompute here).

.tillage_event_cols <- c(
  "event_type",
  "site_id",
  "date",
  "tillage_eff_0to1"
)

tillage_json_builder <- function(rows, i) {
  list(
    event_type = rows$event_type[i],
    date = rows$date[i],
    tillage_eff_0to1 = rows$tillage_eff_0to1[i]
  )
}

build_tillage_events <- function(year, out_dir, metrics_dir) {
  tillage_file <- tillage_table_path(metrics_dir, year)
  copy_parquet_to_event_files(
    tillage_file, out_dir, "tillage", year,
    json_builder = tillage_json_builder,
    missing_hint = paste0(
      "Run Sec. 2.9 first:\n",
      "  Rscript $TILLAGE_ROOT/apply_tillage.R ", year
    ),
    prep = function(dt) {
      if (!"site_id" %in% names(dt) && "parcel_id" %in% names(dt)) {
        dt[, site_id := parcel_id]
      }
      if (!"tillage_eff_0to1" %in% names(dt)) {
        stop(
          "tillage_eff_0to1 missing from ", tillage_file,
          "; re-run tillage/apply_tillage.R"
        )
      }
      # Event date is NDTI-minimum timing; fall back to OGMn if min_date is empty.
      if ("min_date" %in% names(dt)) {
        dt[, date := as.character(min_date)]
      } else if (!"date" %in% names(dt)) {
        dt[, date := NA_character_]
      } else {
        dt[, date := as.character(date)]
      }
      if ("OGMn_date" %in% names(dt)) {
        dt[
          is.na(date) | !nzchar(date) | date %in% c("NA", "NaT"),
          date := as.character(OGMn_date)
        ]
      }
      dt[, event_type := "tillage"]
      n_pre <- nrow(dt)
      dt <- dt[
        is.finite(as.numeric(tillage_eff_0to1)) &
          !is.na(date) & nzchar(as.character(date)) &
          !(date %in% c("NA", "NaT"))
      ]
      if (nrow(dt) < n_pre) {
        message(
          "[tillage] dropped ", n_pre - nrow(dt),
          " row(s) missing tillage_eff_0to1 or date"
        )
      }
      dt <- keep_event_columns(dt, .tillage_event_cols)
      if (nrow(dt)) {
        data.table::setorder(dt, site_id, date)
      }
      dt
    }
  )
}
