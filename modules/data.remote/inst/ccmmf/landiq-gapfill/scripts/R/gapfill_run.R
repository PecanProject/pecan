# Crop gap-fill driver: dispatches full-year (CLASS + SUBCLASS) or within-year (SUBCLASS only).

run_gapfill <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  if (is.na(gapfill_year)) {
    stop("gapfill_year must be a valid integer")
  }

  mode <- resolve_gapfill_mode(gapfill_year)
  message("=== gapfill year=", gapfill_year, " mode=", mode, " ===")

  ensure_emission_tables()
  emission <- load_emission_bundle()
  out_root <- path_outputs()

  if (identical(mode, "full")) {
    class_df <- run_class_gapfill(gapfill_year, emission)
    path_sub_out <- file.path(
      out_root,
      sprintf("landiq_s2_gapfill_subclass_assignment_year=%d.parquet", gapfill_year)
    )
    sub_df <- assign_subclass(gapfill_year, class_df, emission, use_plurality = TRUE)
    arrow::write_parquet(sub_df, path_sub_out)
    message("Wrote subclass assignment: ", path_sub_out)
  } else {
    run_within_year_gapfill(gapfill_year, emission)
  }

  message("=== gapfill year=", gapfill_year, " done ===")
  invisible(NULL)
}

resolve_within_year_subclass_neighbors <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  available <- landiq_gapfill_available_years()
  before <- available[available < gapfill_year]
  after <- available[available > gapfill_year]
  list(
    y_lo = if (length(before) > 0L) max(before) else NA_integer_,
    y_hi = if (length(after) > 0L) min(after) else NA_integer_
  )
}

run_within_year_gapfill <- function(gapfill_year, emission = NULL) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  if (is.null(emission)) {
    ensure_emission_tables()
    emission <- load_emission_bundle()
  }
  if (gapfill_year == 2017L) {
    stop("2017 has no LandIQ year - use full gapfill mode")
  }

  ag_classes <- load_ag_class_vector(path_crop_lookup_csv())
  path_landiq_parquet <- path_landiq_parquet()
  out_dir <- path_outputs()
  path_out <- file.path(out_dir, sprintf("landiq_s2_within_year_gapfill_year=%d.parquet", gapfill_year))

  landiq_s2 <- arrow::open_dataset(path_landiq_parquet) %>%
    dplyr::filter(year == gapfill_year, season == 2L) %>%
    dplyr::select(parcel_id, CLASS, SUBCLASS) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    ) %>%
    apply_landiq_subclass_merge(emission$crop_lk$merge, calendar_year = gapfill_year)

  targets <- landiq_s2 %>%
    dplyr::filter(needs_subclass_gapfill(CLASS, SUBCLASS, ag_classes))

  message(
    "within-year gapfill: year=", gapfill_year,
    " | ag rows missing subclass: ", nrow(targets),
    " (exempt CLASS: ", paste(subclass_gapfill_exempt_classes(), collapse = ","), ")"
  )
  if (nrow(targets) == 0L) {
    message("  Nothing to fill.")
    return(invisible(NULL))
  }

  nbr <- resolve_within_year_subclass_neighbors(gapfill_year)
  class_df <- targets %>%
    dplyr::distinct(parcel_id, CLASS) %>%
    dplyr::transmute(
      parcel_id,
      gapfill_year = gapfill_year,
      map_class_avg_mean3 = CLASS,
      neighbor_year_lo = nbr$y_lo,
      neighbor_year_hi = nbr$y_hi
    )

  sub_df <- assign_subclass(gapfill_year, class_df, emission, use_plurality = TRUE)

  filled <- sub_df %>%
    dplyr::inner_join(
      targets %>% dplyr::distinct(parcel_id, landiq_CLASS = CLASS),
      by = "parcel_id"
    ) %>%
    dplyr::transmute(
      parcel_id,
      year = gapfill_year,
      season = 2L,
      CLASS = landiq_CLASS,
      SUBCLASS = pred_subclass_assignment,
      cdl_code = suppressWarnings(as.integer(cdl_obs_native_code_gap)),
      cdl_frac = cdl_obs_subclass_frac_gap,
      subclass_source = subclass_source
    )

  if (nrow(filled) == 0L) {
    message("  No subclass assignments beyond **.")
    filled <- tibble::tibble(
      parcel_id = character(0),
      year = integer(0),
      season = integer(0),
      CLASS = character(0),
      SUBCLASS = character(0),
      cdl_code = integer(0),
      cdl_frac = numeric(0),
      subclass_source = character(0)
    )
  }

  arrow::write_parquet(filled, path_out)
  message(
    "Wrote within-year gapfill: ", path_out,
    " (", nrow(filled), " rows with specific subclass / ", nrow(targets), " candidates)"
  )
  invisible(filled)
}
