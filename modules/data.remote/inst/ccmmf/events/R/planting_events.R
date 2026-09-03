# Planting events: SIPNET columns from assigned_year=Y_planting.parquet.
# Apply: traits/apply_planting.R (does not recompute here).

.planting_event_cols <- c(
  "event_type",
  "site_id",
  "date",
  "crop_code",
  "leaf_c_kg_m2",
  "wood_c_kg_m2",
  "fine_root_c_kg_m2",
  "coarse_root_c_kg_m2",
  "leaf_n_kg_m2",
  "wood_n_kg_m2",
  "fine_root_n_kg_m2",
  "coarse_root_n_kg_m2"
)

planting_json_builder <- function(rows, i) {
  list(
    event_type = rows$event_type[i],
    date = rows$date[i],
    crop_code = rows$crop_code[i],
    leaf_c_kg_m2 = rows$leaf_c_kg_m2[i],
    wood_c_kg_m2 = rows$wood_c_kg_m2[i],
    fine_root_c_kg_m2 = rows$fine_root_c_kg_m2[i],
    coarse_root_c_kg_m2 = rows$coarse_root_c_kg_m2[i],
    leaf_n_kg_m2 = rows$leaf_n_kg_m2[i],
    wood_n_kg_m2 = rows$wood_n_kg_m2[i],
    fine_root_n_kg_m2 = rows$fine_root_n_kg_m2[i],
    coarse_root_n_kg_m2 = rows$coarse_root_n_kg_m2[i]
  )
}

build_planting_events <- function(year, out_dir, matched_dir) {
  planting_file <- planting_table_path(matched_dir, year)
  copy_parquet_to_event_files(
    planting_file, out_dir, "planting", year,
    json_builder = planting_json_builder,
    missing_hint = paste0(
      "Run Sec. 2.7 first:\n",
      "  Rscript $CCMMF_CODE/traits/apply_planting.R ", year
    ),
    prep = function(dt) {
      if (!"site_id" %in% names(dt) && "parcel_id" %in% names(dt)) {
        dt[, site_id := parcel_id]
      }
      dt[, event_type := "planting"]
      dt[, crop_code := as.character(code)]
      dt[, leaf_c_kg_m2 := as.numeric(C_LEAF)]
      dt[, wood_c_kg_m2 := as.numeric(C_STEM)]
      dt[, fine_root_c_kg_m2 := as.numeric(C_FINEROOT)]
      dt[, coarse_root_c_kg_m2 := as.numeric(C_COARSEROOT)]
      dt[, leaf_n_kg_m2 := as.numeric(N_LEAF)]
      dt[, wood_n_kg_m2 := as.numeric(N_STEM)]
      dt[, fine_root_n_kg_m2 := as.numeric(N_FINEROOT)]
      dt[, coarse_root_n_kg_m2 := as.numeric(N_COARSEROOT)]
      if ("PFT" %in% names(dt)) {
        pft_l <- tolower(trimws(as.character(dt$PFT)))
        dt <- dt[!pft_l %in% c("hay", "other")]
      }
      dt <- keep_event_columns(dt, .planting_event_cols)
      if (nrow(dt)) {
        data.table::setorder(dt, site_id, date)
      }
      dt
    }
  )
}
