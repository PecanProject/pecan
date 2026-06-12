# Full-year CLASS gap-fill: temporal neighbors + CDL emission + transition matrices.
# Writes landiq_s2_gapfill_class_prob_cdl=<mode>_year=<Y>.parquet per gap year.

run_class_gapfill <- function(
    gapfill_year,
    emission,
    path_out = NULL,
    cdl_class_obs = tolower(trimws(Sys.getenv("CDL_CLASS_OBS", "fraction")))) {
  if (!cdl_class_obs %in% c("onehot", "fraction")) {
    stop("CDL_CLASS_OBS must be onehot or fraction; got: ", cdl_class_obs)
  }

  path_landiq_parquet <- path_landiq_parquet()
  path_cdl_dir <- path_cdl_fractions()
  out_root <- path_outputs()
  ag_class_vector <- emission$ag_class_vector
  E <- emission$E
  n_class <- length(ag_class_vector)

  gap_cfg <- resolve_gapfill_neighbors(gapfill_year)
  message(gapfill_run_summary(gap_cfg))
  neighboring_years <- gap_cfg$neighbor_years
  y_lo <- gap_cfg$y_lo
  y_hi <- gap_cfg$y_hi
  temporal_mode <- gap_cfg$temporal_mode
  n_temporal_signals <- gap_cfg$n_temporal_signals

  if (is.null(path_out)) {
    path_out <- file.path(
      out_root,
      sprintf("landiq_s2_gapfill_class_prob_cdl=%s_year=%d.parquet", cdl_class_obs, gapfill_year)
    )
  }

  transition_level <- gapfill_transition_level()
  transition_prob <- load_transition_matrix_csv(path_transition_matrix(), ag_class_vector)
  county_transition_mats <- list()
  if (identical(transition_level, "county")) {
    county_matrices_dir <- path_county_transition_dir()
    message("Loading county transition matrices from: ", county_matrices_dir)
    county_transition_mats <- load_county_transition_matrices(county_matrices_dir, ag_class_vector)
  }

  landiq_s2_ag <- arrow::open_dataset(path_landiq_parquet) %>%
    dplyr::filter(
      year %in% neighboring_years,
      season == 2L,
      CLASS %in% ag_class_vector
    ) %>%
    dplyr::select(parcel_id, year, season, CLASS, COUNTY) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      CLASS = trimws(as.character(CLASS)),
      COUNTY = trimws(as.character(COUNTY))
    )

  parcel_county <- landiq_s2_ag %>%
    dplyr::filter(!is.na(COUNTY), nzchar(COUNTY)) %>%
    dplyr::distinct(parcel_id, COUNTY) %>%
    dplyr::group_by(parcel_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(county = COUNTY)

  wide_liq <- landiq_s2_ag %>%
    dplyr::filter(year %in% neighboring_years) %>%
    dplyr::distinct(parcel_id, year, CLASS) %>%
    tidyr::pivot_wider(names_from = year, values_from = CLASS, names_prefix = "CLASS_")

  col_lo <- if (!is.na(y_lo)) paste0("CLASS_", y_lo) else NA_character_
  col_hi <- if (!is.na(y_hi)) paste0("CLASS_", y_hi) else NA_character_
  need_cols <- c(col_lo, col_hi)
  need_cols <- need_cols[!is.na(need_cols)]
  if (!all(need_cols %in% names(wide_liq))) {
    stop("Expected neighbor columns: ", paste(need_cols, collapse = ", "))
  }

  wide_liq <- wide_liq %>%
    dplyr::filter(
      if (!is.na(col_lo)) !is.na(.data[[col_lo]]) & .data[[col_lo]] %in% ag_class_vector else TRUE,
      if (!is.na(col_hi)) !is.na(.data[[col_hi]]) & .data[[col_hi]] %in% ag_class_vector else TRUE
    )

  path_cdl_gap <- file.path(path_cdl_dir, sprintf("cdl_fractions_year=%d.parquet", gapfill_year))
  if (!file.exists(path_cdl_gap)) {
    stop("Missing CDL fractions for gap year ", gapfill_year, ": ", path_cdl_gap)
  }
  cdl_gap_full <- arrow::read_parquet(path_cdl_gap, as_data_frame = TRUE)

  cdl_panel <- build_cdl_fraction_panel(cdl_gap_full, colnames(E))
  if (is.null(cdl_panel)) {
    stop("No parcels with positive native-code mass overlapping emission code set for gap year ", gapfill_year)
  }

  F_wide <- tibble::tibble(
    parcel_id = cdl_panel$parcel_id,
    cdl_native_mass_in_emission = cdl_panel$native_mass,
    cdl_code_gap_dominant_native = cdl_panel$dominant_code
  )
  F_wide <- bind_cols(F_wide, as_tibble(cdl_panel$F_mat, .name_repair = "minimal"))
  names(F_wide)[4:ncol(F_wide)] <- colnames(E)

  panel <- wide_liq %>%
    dplyr::inner_join(F_wide, by = "parcel_id") %>%
    dplyr::left_join(parcel_county, by = "parcel_id")

  message(
    "Gapfill panel (CDL_CLASS_OBS=", cdl_class_obs, ", transition=", transition_level, "): ",
    nrow(panel), " parcels; ", sum(!is.na(panel$county)), " with county"
  )

  idx_lo <- if (!is.na(col_lo)) match(panel[[col_lo]], ag_class_vector) else rep(NA_integer_, nrow(panel))
  idx_hi <- if (!is.na(col_hi)) match(panel[[col_hi]], ag_class_vector) else rep(NA_integer_, nrow(panel))
  if (!is.na(col_lo) && anyNA(idx_lo)) stop("Unmatched neighbor classes (earlier year)")
  if (!is.na(col_hi) && anyNA(idx_hi)) stop("Unmatched neighbor classes (later year)")

  F_panel <- as.matrix(panel[, colnames(E), drop = FALSE])
  p_cdl <- cdl_class_likelihood(F_panel, E, ag_class_vector, cdl_class_obs)

  if (identical(temporal_mode, "both")) {
    county_stems <- vapply(panel$county, county_matrix_stem, character(1))
    fwd_bwd <- compute_fwd_bwd_by_county(
      county_stems, idx_lo, idx_hi, county_transition_mats, transition_prob, n_class
    )
    p_fwd <- fwd_bwd$p_fwd
    p_bwd <- fwd_bwd$p_bwd
    panel$county_matrix_stem <- fwd_bwd$county_matrix_stem
  } else if (identical(temporal_mode, "before_only")) {
    p_fwd <- matrix(0, nrow = nrow(panel), ncol = n_class)
    p_bwd <- matrix(0, nrow = nrow(panel), ncol = n_class)
    if (identical(transition_level, "county")) {
      county_stems <- vapply(panel$county, county_matrix_stem, character(1))
      part <- compute_fwd_bwd_by_county(
        county_stems, idx_lo, rep(NA_integer_, nrow(panel)),
        county_transition_mats, transition_prob, n_class
      )
      p_fwd <- part$p_fwd
      panel$county_matrix_stem <- part$county_matrix_stem
    } else {
      part <- compute_fwd_bwd_from_transition(
        transition_prob, idx_lo, rep(NA_integer_, nrow(panel)), n_class
      )
      p_fwd <- part$p_fwd
      panel$county_matrix_stem <- NA_character_
    }
  } else if (identical(temporal_mode, "after_only")) {
    p_fwd <- matrix(0, nrow = nrow(panel), ncol = n_class)
    p_bwd <- matrix(0, nrow = nrow(panel), ncol = n_class)
    if (identical(transition_level, "county")) {
      county_stems <- vapply(panel$county, county_matrix_stem, character(1))
      part <- compute_fwd_bwd_by_county(
        county_stems, rep(NA_integer_, nrow(panel)), idx_hi,
        county_transition_mats, transition_prob, n_class
      )
      p_bwd <- part$p_bwd
      panel$county_matrix_stem <- part$county_matrix_stem
    } else {
      part <- compute_fwd_bwd_from_transition(
        transition_prob, rep(NA_integer_, nrow(panel)), idx_hi, n_class
      )
      p_bwd <- part$p_bwd
      panel$county_matrix_stem <- NA_character_
    }
  } else {
    stop("Unknown temporal_mode: ", temporal_mode)
  }

  p_mat_avg <- (p_fwd + p_bwd + p_cdl) / (n_temporal_signals + 1L)
  nm_suffix <- make.names(ag_class_vector, unique = TRUE)
  colnames(p_mat_avg) <- paste0("p_avg_", nm_suffix)
  map_avg <- ag_class_vector[max.col(p_mat_avg, ties.method = "first")]

  out <- bind_cols(
    tibble::tibble(
      parcel_id = panel$parcel_id,
      gapfill_year = gapfill_year,
      neighbor_year_lo = y_lo,
      neighbor_year_hi = y_hi,
      temporal_mode = temporal_mode,
      n_temporal_signals = n_temporal_signals,
      county = panel$county,
      county_matrix_stem = panel$county_matrix_stem,
      transition_level = transition_level,
      class_neighbor_lo = if (!is.na(col_lo)) panel[[col_lo]] else NA_character_,
      class_neighbor_hi = if (!is.na(col_hi)) panel[[col_hi]] else NA_character_,
      cdl_class_obs = cdl_class_obs,
      cdl_code_gap_dominant_native = panel$cdl_code_gap_dominant_native,
      cdl_native_mass_in_emission = panel$cdl_native_mass_in_emission,
      map_class_avg_mean3 = map_avg
    ),
    as_tibble(p_mat_avg, .name_repair = "minimal")
  )

  arrow::write_parquet(out, path_out)
  message("Wrote class gapfill: ", path_out, " (", nrow(out), " rows)")
  invisible(out)
}

run_class_gapfill_cdl_only <- function(
    gapfill_year,
    emission,
    parcel_ids,
    cdl_class_obs = "fraction") {
  path_cdl_dir <- path_cdl_fractions()
  path_cdl <- file.path(path_cdl_dir, sprintf("cdl_fractions_year=%d.parquet", gapfill_year))
  if (!file.exists(path_cdl)) {
    stop("Missing CDL fractions: ", path_cdl)
  }
  cdl_gap_full <- arrow::read_parquet(path_cdl, as_data_frame = TRUE)
  cdl_panel <- build_cdl_fraction_panel(cdl_gap_full, colnames(emission$E), parcel_ids)
  if (is.null(cdl_panel)) {
    return(tibble::tibble())
  }
  map_class <- map_class_from_cdl(cdl_panel, emission$E, emission$ag_class_vector, cdl_class_obs)
  tibble::tibble(
    parcel_id = cdl_panel$parcel_id,
    pred_class = map_class,
    cdl_code_gap_dominant_native = cdl_panel$dominant_code,
    cdl_native_mass_in_emission = cdl_panel$native_mass
  )
}
