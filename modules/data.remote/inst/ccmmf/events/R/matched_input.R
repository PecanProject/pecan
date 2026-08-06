# Load assigned LandIQ-MSLSP rows for statewide events.
# Prefer gap-filled overlay when present so no_mslsp rows can get filled dates.
# Date / EVI filtering happens in each build_* module (so planting+harvest can
# keep different row sets).

load_matched_for_events <- function(year, matched_dir,
                                    run_phenology = FALSE,
                                    run_planting = FALSE,
                                    run_harvest = FALSE) {
  yr <- as.integer(year)
  assigned_file <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr))
  gapfill_dir <- Sys.getenv(
    "GAPFILL_DATES_DIR",
    file.path(matched_dir, "gapfill_dates")
  )
  gapfill_file <- file.path(gapfill_dir, sprintf("assigned_year=%d_gapfilled.parquet", yr))

  if (file.exists(gapfill_file)) {
    assigned <- data.table::as.data.table(arrow::read_parquet(gapfill_file))
    message("[assigned] loaded gap-filled overlay: ", gapfill_file)
  } else if (file.exists(assigned_file)) {
    assigned <- data.table::as.data.table(arrow::read_parquet(assigned_file))
    message(
      "[assigned] no gap-filled overlay at ", gapfill_file,
      "; using canonical assigned"
    )
  } else {
    stop("Missing assigned file: ", assigned_file)
  }

  assigned[, parcel_id := as.character(parcel_id)]
  has_gapfill_src <- "gapfill_date_source" %in% names(assigned) ||
    all(c("gapfill_planting_source", "gapfill_harvest_source") %in% names(assigned)) ||
    all(c("planting_date_filled", "harvest_date_filled") %in% names(assigned))

  if (has_gapfill_src) {
    matched <- assigned[assigned_by %in% c("matched", "no_mslsp", "no_match")]
  } else {
    matched <- assigned[assigned_by == "matched"]
  }
  message("[assigned] ", nrow(assigned), " rows; event candidates: ", nrow(matched))

  n_before <- nrow(matched)
  matched <- matched[
    !is.na(landiq_CLASS) & !is.na(landiq_SUBCLASS) & !is.na(landiq_PFT)
  ]
  message(
    "  Dropped ", n_before - nrow(matched),
    " rows missing crop/PFT; ", nrow(matched), " remain"
  )

  # Planting / harvest dates from standard MSLSP metric columns (gap-fill
  # writes into these in place when overlay is used).
  if ("planting_date_filled" %in% names(matched)) {
    # Legacy overlay with parallel filled columns
    matched[, planting_date_str := as.character(planting_date_filled)]
    matched[, harvest_date_str := as.character(harvest_date_filled)]
  } else {
    matched[, planting_date_str := as.character(mslsp_OGI)]
    for (col in c("mslsp_OGMn", "mslsp_OGD")) {
      if (!col %in% names(matched)) {
        matched[, (col) := as.Date(NA)]
      }
    }
    matched[, pft_l := tolower(trimws(as.character(landiq_PFT)))]
    matched[, harvest_date_str := NA_character_]
    matched[pft_l %in% c("row", "rice"), harvest_date_str := as.character(mslsp_OGMn)]
    matched[pft_l %in% c("hay", "woody"), harvest_date_str := as.character(mslsp_OGD)]
    matched[, pft_l := NULL]
  }

  # Phenology-only path: need leaf-on/off dates (observed or gap-filled)
  if (isTRUE(run_phenology) && !isTRUE(run_planting) && !isTRUE(run_harvest)) {
    n_ev <- nrow(matched)
    matched <- matched[
      !is.na(mslsp_50PCGI) & !is.na(mslsp_50PCGD)
    ]
    message("  Phenology filter: dropped to ", nrow(matched), " (from ", n_ev, ")")
  }

  matched
}

# Season-2 crop identity for one year (CLASS-level woody transition checks).
load_landiq_season2_identity <- function(year, landiq_crops, cropcode_csv) {
  yr <- as.integer(year)
  if (!file.exists(landiq_crops) && !dir.exists(landiq_crops)) {
    message("[landiq] crops parquet not found: ", landiq_crops)
    return(NULL)
  }
  if (!file.exists(cropcode_csv)) {
    message("[landiq] crop code lookup not found: ", cropcode_csv)
    return(NULL)
  }

  # Harmonized LandIQ uses the 2021 DWR RS legend only (includes C**, V**).
  lookup <- data.table::fread(cropcode_csv, showProgress = FALSE)
  if ("legend_year" %in% names(lookup)) {
    lookup <- lookup[as.integer(legend_year) == 2021L]
  }
  ag_pairs <- unique(
    lookup[is_agricultural == TRUE, .(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = as.character(SUBCLASS),
      PFT = as.character(PFT)
    )]
  )
  ag_classes <- unique(ag_pairs$CLASS)

  landiq <- data.table::as.data.table(
    arrow::open_dataset(landiq_crops) |>
      dplyr::filter(year == !!yr, season == 2L, CLASS %in% !!ag_classes) |>
      dplyr::collect()
  )
  if (nrow(landiq) == 0L) {
    message("[landiq] no season-2 ag rows for year=", yr)
    return(NULL)
  }

  landiq[, CLASS := trimws(as.character(CLASS))]
  landiq[, SUBCLASS := as.character(SUBCLASS)]
  landiq[
    is.na(SUBCLASS) | trimws(SUBCLASS) == "" | trimws(SUBCLASS) == "**",
    SUBCLASS := "**"
  ]
  landiq[, parcel_id := trimws(as.character(parcel_id))]
  if ("SPECOND" %in% names(landiq)) {
    landiq[, SPECOND := trimws(as.character(SPECOND))]
  } else {
    landiq[, SPECOND := NA_character_]
  }

  landiq <- merge(landiq, ag_pairs, by = c("CLASS", "SUBCLASS"))
  data.table::setorder(landiq, parcel_id)
  landiq <- landiq[, .SD[1L], by = parcel_id]
  landiq[, .(
    parcel_id,
    CLASS,
    SUBCLASS,
    PFT,
    SPECOND
  )]
}
