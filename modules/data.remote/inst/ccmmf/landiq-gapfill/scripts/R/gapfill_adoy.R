# ADOY (peak-greenness day) gap-fill -- step 2 after crop identity (gapfill.R adoy).
#
# Same mode split as crop (resolve_gapfill_mode):
#   within_year -- fill invalid ADOY on ag parcels for any season (LandIQ base +
#                  within-year subclass overlay from crop step)
#   full        -- season LANDIQ_ADOY_DEFAULT_SEASON only (default 2); CLASS/SUBCLASS
#                  from full-gap subclass assignment; ADOY starts NA
#
# Invalid ADOY = NA or 0 (is_valid_adoy). Exempt CLASS X, I -> not_applicable
# (YP still receives ADOY). Valid originals stay observed.
#
# Reference tables (adoy-ref; not a predictive model) under outputs/:
#   county/statewide means (or median) by CLASS x optional SUBCLASS x season
#   parcel panel of observed ADOY for temporal_neighbor reuse
#
# Fill cascade in fill_adoy_panel (coalesce order -> adoy_source):
#   1. county_class_subclass
#   2. temporal_neighbor   (same parcel/season/CLASS/SUBCLASS, year gap <= 3)
#   3. county_class
#   4. statewide_class_subclass
#   5. statewide_class
#   6. unfilled
#   7. multiuse_season2    (post-pass: MULTIUSE=M copy season-2 ADOY)
#
# Env: ADOY_REFERENCE_STAT, LANDIQ_ADOY_TRAINING_YEARS,
#      LANDIQ_ADOY_TRAINING_EXCLUDE_YEARS, ADOY_TEMPORAL_MAX_YEAR_GAP,
#      LANDIQ_ADOY_DEFAULT_SEASON, GAPFILL_REBUILD_ADOY_REF

# --- Validity / exemptions -----------------------------------------------------

#' TRUE when ADOY is usable LandIQ peak day (numeric, not NA, not 0).
is_valid_adoy <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  !is.na(x) & x != 0
}

#' Force exempt CLASS (X, I) to ADOY NA and adoy_source = not_applicable.
apply_adoy_class_exempt <- function(df) {
  exempt <- adoy_gapfill_exempt_classes()
  cls <- trimws(as.character(df$CLASS))
  is_exempt <- cls %in% exempt
  df$ADOY <- dplyr::if_else(is_exempt, NA_real_, suppressWarnings(as.numeric(df$ADOY)))
  if ("adoy_source" %in% names(df)) {
    df$adoy_source <- dplyr::if_else(is_exempt, "not_applicable", df$adoy_source)
  }
  df
}

# --- Reference table paths / training years ------------------------------------

adoy_reference_suffix <- function() {
  yrs <- adoy_training_years()
  suf <- sprintf("%d-%d", min(yrs), max(yrs))
  excluded <- setdiff(landiq_gapfill_available_years(), yrs)
  excluded <- sort(unique(excluded))
  if (length(excluded) > 0L) {
    suf <- paste0(suf, "_excl", paste(excluded, collapse = "-"))
  }
  suf
}

#' Years of observed LandIQ used to build ADOY reference tables.
adoy_training_years <- function() {
  env <- Sys.getenv("LANDIQ_ADOY_TRAINING_YEARS", "")
  if (nzchar(trimws(env))) {
    return(.gapfill_parse_year_csv(env))
  }
  # Default: every year present in the (non-gap-filled) LandIQ product.
  yrs <- landiq_gapfill_available_years()
  exclude_env <- trimws(Sys.getenv("LANDIQ_ADOY_TRAINING_EXCLUDE_YEARS", ""))
  if (nzchar(exclude_env)) {
    yrs <- setdiff(yrs, .gapfill_parse_year_csv(exclude_env))
  }
  sort(yrs)
}

adoy_reference_stat <- function() {
  stat <- tolower(trimws(Sys.getenv("ADOY_REFERENCE_STAT", "mean")))
  if (!stat %in% c("mean", "median")) {
    stop("ADOY_REFERENCE_STAT must be 'mean' or 'median'; got: ", stat)
  }
  stat
}

.summarise_adoy_ref <- function(x, stat = adoy_reference_stat()) {
  if (identical(stat, "median")) {
    stats::median(x, na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
}

adoy_output_paths <- function(suffix = adoy_reference_suffix()) {
  stat <- adoy_reference_stat()
  out <- path_outputs()
  tag <- sprintf("adoy_%s", stat)
  list(
    county_css = file.path(out, sprintf("%s_county_class_subclass_%s.parquet", tag, suffix)),
    county_class = file.path(out, sprintf("%s_county_class_%s.parquet", tag, suffix)),
    statewide_css = file.path(out, sprintf("%s_statewide_class_subclass_%s.parquet", tag, suffix)),
    statewide_class = file.path(out, sprintf("%s_statewide_class_%s.parquet", tag, suffix)),
    observed = file.path(out, sprintf("adoy_observed_history_%s.parquet", suffix))
  )
}

adoy_reference_cached <- function(suffix = adoy_reference_suffix()) {
  paths <- adoy_output_paths(suffix)
  all(file.exists(unlist(paths)))
}

#' Build lookup tables from observed LandIQ ADOY (gapfill.R adoy-ref).
#'
#' Writes four geographic summaries + a parcel-level observed history panel.
#' Not a fitted model -- fill later joins these tables.
build_adoy_reference <- function() {
  suffix <- adoy_reference_suffix()
  train_years <- adoy_training_years()
  ag_classes <- setdiff(
    load_ag_class_vector(path_crop_lookup_csv()),
    adoy_gapfill_exempt_classes()
  )
  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  path_landiq <- path_landiq_parquet()

  stat <- adoy_reference_stat()
  message(
    "Building ADOY reference (years ", paste(train_years, collapse = ","),
    ", stat=", stat, ", per-year subclass harmonization)..."
  )
  obs <- arrow::open_dataset(path_landiq) %>%
    dplyr::filter(
      year %in% train_years,
      CLASS %in% ag_classes
    ) %>%
    dplyr::select(parcel_id, year, season, CLASS, SUBCLASS, COUNTY, ADOY) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS)),
      COUNTY = trimws(as.character(COUNTY)),
      ADOY = suppressWarnings(as.numeric(ADOY))
    ) %>%
    dplyr::filter(is_valid_adoy(ADOY)) %>%
    harmonize_landiq_subclass_by_year(crop_lk$merge)

  if (nrow(obs) == 0L) {
    stop("No observed ADOY rows in training years")
  }

  obs <- obs %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        is.na(SUBCLASS) | trimws(SUBCLASS) == "" | trimws(SUBCLASS) == "**",
        "**",
        SUBCLASS
      )
    )

  ref_county_css <- obs %>%
    dplyr::group_by(COUNTY, CLASS, SUBCLASS, season) %>%
    dplyr::summarise(
      ref_adoy = .summarise_adoy_ref(ADOY, stat),
      n = dplyr::n(),
      .groups = "drop"
    )

  ref_county_class <- obs %>%
    dplyr::group_by(COUNTY, CLASS, season) %>%
    dplyr::summarise(
      ref_adoy = .summarise_adoy_ref(ADOY, stat),
      n = dplyr::n(),
      .groups = "drop"
    )

  ref_state_css <- obs %>%
    dplyr::group_by(CLASS, SUBCLASS, season) %>%
    dplyr::summarise(
      ref_adoy = .summarise_adoy_ref(ADOY, stat),
      n = dplyr::n(),
      .groups = "drop"
    )

  ref_state_class <- obs %>%
    dplyr::group_by(CLASS, season) %>%
    dplyr::summarise(
      ref_adoy = .summarise_adoy_ref(ADOY, stat),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Parcel panel for temporal_neighbor (same parcel reuse).
  hist <- obs %>%
    dplyr::select(parcel_id, year, season, CLASS, SUBCLASS, ADOY)

  paths <- adoy_output_paths(suffix)
  arrow::write_parquet(ref_county_css, paths$county_css)
  arrow::write_parquet(ref_county_class, paths$county_class)
  arrow::write_parquet(ref_state_css, paths$statewide_css)
  arrow::write_parquet(ref_state_class, paths$statewide_class)
  arrow::write_parquet(hist, paths$observed)

  message(
    "ADOY reference: ", nrow(obs), " observed rows; county CSS groups=", nrow(ref_county_css),
    " county CLASS groups=", nrow(ref_county_class),
    " statewide CSS groups=", nrow(ref_state_css),
    " statewide CLASS groups=", nrow(ref_state_class)
  )
  invisible(paths)
}

#' Load cached ADOY reference tables, or rebuild when force=TRUE.
#'
#' Routine adoy fill expects tables under outputs/. If they are missing, stop
#' with a clear rebuild hint (do not silently rebuild). Explicit rebuild:
#'   Rscript gapfill.R adoy-ref
#'   ./run_gapfill.sh --adoy-ref YEARS
ensure_adoy_reference <- function(force = NULL) {
  suffix <- adoy_reference_suffix()
  if (is.null(force)) {
    force <- tolower(Sys.getenv("GAPFILL_REBUILD_ADOY_REF", "false")) %in% c("1", "true", "yes")
  }
  paths <- adoy_output_paths(suffix)
  if (!force && adoy_reference_cached(suffix)) {
    message(
      "ADOY reference tables present (suffix=", suffix,
      "); using cache under ", path_outputs()
    )
    return(invisible(paths))
  }
  if (!force) {
    stop(
      "Missing ADOY reference tables (suffix=", suffix, ") under ",
      path_outputs(), ".\n",
      "  Confirm files exist, or rebuild with:\n",
      "    Rscript scripts/gapfill.R adoy-ref\n",
      "    # or: ./run_gapfill.sh --adoy-ref YEARS"
    )
  }
  message("Building ADOY reference tables (suffix=", suffix, ")...")
  build_adoy_reference()
}

load_adoy_reference <- function() {
  paths <- adoy_output_paths()
  if (!adoy_reference_cached(adoy_reference_suffix())) {
    stop(
      "Missing ADOY reference tables under ", path_outputs(), ".\n",
      "  Rebuild with: Rscript scripts/gapfill.R adoy-ref\n",
      "  # or: ./run_gapfill.sh --adoy-ref YEARS"
    )
  }
  list(
    suffix = adoy_reference_suffix(),
    paths = paths,
    county_css = arrow::read_parquet(paths$county_css, as_data_frame = TRUE),
    county_class = arrow::read_parquet(paths$county_class, as_data_frame = TRUE),
    statewide_css = arrow::read_parquet(paths$statewide_css, as_data_frame = TRUE),
    statewide_class = arrow::read_parquet(paths$statewide_class, as_data_frame = TRUE),
    observed = arrow::read_parquet(paths$observed, as_data_frame = TRUE)
  )
}

# --- Temporal neighbor donors --------------------------------------------------

#' Which years may donate ADOY for temporal_neighbor.
#'
#' Full-gap: neighbor LandIQ years from resolve_gapfill_neighbors.
#' Within-year: training-year panel (adoy_training_years); temporal_mode label
#' "panel" means donors are that set, not only nearest before/after.
resolve_adoy_temporal_cfg <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  mode <- resolve_gapfill_mode(gapfill_year)

  if (identical(mode, "full")) {
    nbr <- resolve_gapfill_neighbors(gapfill_year)
    return(list(
      gapfill_year = gapfill_year,
      mode = mode,
      neighbor_years = nbr$neighbor_years,
      temporal_mode = nbr$temporal_mode
    ))
  }

  list(
    gapfill_year = gapfill_year,
    mode = mode,
    neighbor_years = adoy_training_years(),
    temporal_mode = "panel"
  )
}

.normalize_subclass <- function(x) {
  x <- trimws(as.character(x))
  dplyr::if_else(is.na(x) | x == "" | x == "**", "**", x)
}

#' Same parcel / season / CLASS / SUBCLASS ADOY from nearby years.
#'
#' Donor years from cfg; keep year_gap <= ADOY_TEMPORAL_MAX_YEAR_GAP (default 3).
#' Aggregate multiple donors with ADOY_REFERENCE_STAT (mean/median).
compute_temporal_adoy <- function(target, observed, cfg) {
  max_gap <- as.integer(Sys.getenv("ADOY_TEMPORAL_MAX_YEAR_GAP", "3"))
  key <- c("parcel_id", "season", "CLASS", "SUBCLASS")

  donor_years <- cfg$neighbor_years
  if (identical(cfg$mode, "within_year")) {
    donor_years <- adoy_training_years()
  }
  if (length(donor_years) == 0L) {
    return(target %>% dplyr::mutate(adoy_temporal = NA_real_, n_temporal = 0L))
  }

  obs <- observed %>%
    dplyr::mutate(
      SUBCLASS = .normalize_subclass(SUBCLASS),
      year = as.integer(year),
      season = as.integer(season)
    ) %>%
    dplyr::filter(
      year %in% donor_years,
      is_valid_adoy(ADOY)
    )

  tgt <- target %>%
    dplyr::mutate(
      SUBCLASS = .normalize_subclass(SUBCLASS),
      target_year = as.integer(year),
      season = as.integer(season)
    )

  joined <- tgt %>%
    dplyr::inner_join(
      obs %>% dplyr::rename(donor_year = year, adoy_donor = ADOY),
      by = key
    ) %>%
    dplyr::mutate(year_gap = abs(donor_year - target_year)) %>%
    dplyr::filter(year_gap <= max_gap)

  if (nrow(joined) == 0L) {
    return(target %>% dplyr::mutate(adoy_temporal = NA_real_, n_temporal = 0L))
  }

  temporal <- joined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "parcel_id", "target_year", "season", "CLASS", "SUBCLASS"
    )))) %>%
    dplyr::summarise(
      adoy_temporal = .summarise_adoy_ref(adoy_donor),
      n_temporal = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(year = target_year)

  tgt %>%
    dplyr::select(-target_year) %>%
    dplyr::left_join(temporal, by = c("parcel_id", "year", "season", "CLASS", "SUBCLASS"))
}

join_county_adoy <- function(df, ref_tbl, by_cols, out_col) {
  df %>%
    dplyr::left_join(
      ref_tbl %>% dplyr::rename(!!out_col := ref_adoy),
      by = by_cols
    )
}

#' For MULTIUSE = M, copy season-2 ADOY to other seasons with the same CLASS/SUBCLASS.
apply_multiuse_s2_adoy_fallback <- function(df) {
  if (!"MULTIUSE" %in% names(df)) {
    return(df)
  }

  s2_adoy <- df %>%
    dplyr::filter(season == 2L, is_valid_adoy(ADOY)) %>%
    dplyr::distinct(parcel_id, year, CLASS, SUBCLASS, .keep_all = TRUE) %>%
    dplyr::transmute(
      parcel_id,
      year,
      CLASS,
      SUBCLASS,
      adoy_s2 = ADOY
    )

  df %>%
    dplyr::mutate(MULTIUSE = trimws(as.character(MULTIUSE))) %>%
    dplyr::left_join(s2_adoy, by = c("parcel_id", "year", "CLASS", "SUBCLASS")) %>%
    dplyr::mutate(
      use_s2 = MULTIUSE == "M" &
        season != 2L &
        !is_valid_adoy(ADOY) &
        is_valid_adoy(adoy_s2),
      ADOY = dplyr::if_else(use_s2, adoy_s2, ADOY),
      adoy_source = dplyr::if_else(use_s2, "multiuse_season2", adoy_source)
    ) %>%
    dplyr::select(-adoy_s2, -use_s2)
}

# --- Fill cascade --------------------------------------------------------------

#' Apply reference + temporal fills to a target panel; never overwrite valid ADOY.
fill_adoy_panel <- function(panel, ref, cfg) {
  panel <- panel %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      SUBCLASS = .normalize_subclass(SUBCLASS),
      season = as.integer(season),
      COUNTY = trimws(as.character(COUNTY)),
      ADOY = suppressWarnings(as.numeric(ADOY)),
      needs_fill = !is_valid_adoy(ADOY)
    )

  n_obs <- sum(!panel$needs_fill)
  n_fill <- sum(panel$needs_fill)
  message(
    "ADOY panel: ", nrow(panel), " rows; observed=", n_obs,
    " need_fill=", n_fill
  )

  if (n_fill == 0L) {
    return(panel %>%
      dplyr::mutate(adoy_source = "observed") %>%
      dplyr::select(-.row_id, -needs_fill))
  }

  to_fill <- panel %>% dplyr::filter(needs_fill)

  # Join all candidates, then coalesce in cascade order (see file header).
  filled <- compute_temporal_adoy(to_fill, ref$observed, cfg) %>%
    join_county_adoy(ref$county_css, c("COUNTY", "CLASS", "SUBCLASS", "season"), "adoy_county_css") %>%
    join_county_adoy(ref$county_class, c("COUNTY", "CLASS", "season"), "adoy_county_class") %>%
    join_county_adoy(ref$statewide_css, c("CLASS", "SUBCLASS", "season"), "adoy_statewide_css") %>%
    join_county_adoy(ref$statewide_class, c("CLASS", "season"), "adoy_statewide_class") %>%
    dplyr::transmute(
      .row_id,
      adoy_fill = dplyr::coalesce(
        adoy_county_css,
        adoy_temporal,
        adoy_county_class,
        adoy_statewide_css,
        adoy_statewide_class
      ),
      adoy_source = dplyr::case_when(
        !is.na(adoy_county_css) ~ "county_class_subclass",
        !is.na(adoy_temporal) ~ "temporal_neighbor",
        !is.na(adoy_county_class) ~ "county_class",
        !is.na(adoy_statewide_css) ~ "statewide_class_subclass",
        !is.na(adoy_statewide_class) ~ "statewide_class",
        TRUE ~ "unfilled"
      )
    )

  out <- panel %>%
    dplyr::left_join(filled, by = ".row_id") %>%
    dplyr::mutate(
      adoy_orig = ADOY,
      ADOY = dplyr::if_else(needs_fill, adoy_fill, ADOY),
      adoy_source = dplyr::if_else(needs_fill, adoy_source, "observed")
    ) %>%
    dplyr::select(-.row_id, -adoy_fill, -needs_fill)

  # Safety: never mutate rows that already had a valid ADOY.
  bad <- out %>%
    dplyr::filter(is_valid_adoy(adoy_orig)) %>%
    dplyr::filter(abs(ADOY - adoy_orig) > 1e-9 | adoy_source != "observed")
  if (nrow(bad) > 0L) {
    stop("ADOY gap-fill changed ", nrow(bad), " row(s) that already had ADOY.")
  }

  out %>%
    dplyr::select(-adoy_orig) %>%
    apply_multiuse_s2_adoy_fallback()
}

# --- Build target panel for one year -------------------------------------------

path_subclass_assignment <- function(gapfill_year) {
  file.path(
    path_outputs(),
    sprintf("landiq_s2_gapfill_subclass_assignment_year=%d.parquet", as.integer(gapfill_year))
  )
}

path_within_year_gapfill <- function(gapfill_year) {
  file.path(
    path_outputs(),
    sprintf("landiq_s2_within_year_gapfill_year=%d.parquet", as.integer(gapfill_year))
  )
}

#' Harmonized LandIQ rows for gapfill_year (all seasons) before ADOY fill.
load_landiq_adoy_base <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  arrow::open_dataset(path_landiq_parquet()) %>%
    dplyr::filter(year == gapfill_year) %>%
    dplyr::select(parcel_id, year, season, CLASS, SUBCLASS, COUNTY, ADOY, MULTIUSE) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS)),
      COUNTY = trimws(as.character(COUNTY)),
      ADOY = suppressWarnings(as.numeric(ADOY))
    ) %>%
    apply_landiq_subclass_merge(crop_lk$merge, calendar_year = gapfill_year) %>%
    dplyr::mutate(SUBCLASS = .normalize_subclass(SUBCLASS))
}

count_needs_subclass_gapfill_s2 <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  ag_classes <- load_ag_class_vector(path_crop_lookup_csv())
  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  arrow::open_dataset(path_landiq_parquet()) %>%
    dplyr::filter(year == gapfill_year, season == 2L) %>%
    dplyr::select(parcel_id, CLASS, SUBCLASS) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    ) %>%
    apply_landiq_subclass_merge(crop_lk$merge, calendar_year = gapfill_year) %>%
    dplyr::filter(needs_subclass_gapfill(CLASS, SUBCLASS, ag_classes)) %>%
    nrow()
}

#' Within-year ADOY needs crop patch if season-2 still has missing subclasses.
ensure_within_year_crop_prerequisite <- function(gapfill_year) {
  n_missing <- count_needs_subclass_gapfill_s2(gapfill_year)
  path_wy <- path_within_year_gapfill(gapfill_year)
  if (n_missing > 0L && !file.exists(path_wy)) {
    stop(
      "Within-year ADOY requires subclass gap-fill output when ag rows lack subclass (",
      n_missing, " season-2 row(s)): ", path_wy,
      "\nRun gapfill.R crop ", gapfill_year, " first."
    )
  }
  invisible(n_missing)
}

#' Overlay filled SUBCLASS from within-year crop patch onto the ADOY panel.
#'
#' So temporal/county joins use the post-crop CLASS::SUBCLASS identity.
overlay_within_year_crop_fill <- function(panel, gapfill_year) {
  path_wy <- path_within_year_gapfill(gapfill_year)
  if (!file.exists(path_wy)) {
    return(panel)
  }

  wy <- arrow::read_parquet(path_wy, as_data_frame = TRUE) %>%
    dplyr::transmute(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      wy_SUBCLASS = .normalize_subclass(SUBCLASS)
    )

  n_overlay <- nrow(wy)
  message(
    "Within-year ADOY: overlay subclass fill from ", path_wy,
    " (", n_overlay, " parcel-season row(s))"
  )

  panel %>%
    dplyr::left_join(wy, by = c("parcel_id", "year", "season")) %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(!is.na(wy_SUBCLASS), wy_SUBCLASS, SUBCLASS)
    ) %>%
    dplyr::select(-wy_SUBCLASS)
}

#' Rows that need ADOY consideration for gapfill_year (ag, non-exempt CLASS).
load_adoy_target_panel <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  mode <- resolve_gapfill_mode(gapfill_year)
  ag_classes <- load_ag_class_vector(path_crop_lookup_csv())

  panel <- if (identical(mode, "full")) {
    # Full-gap: crop step wrote subclass assignment; ADOY unknown -> NA.
    path_sub <- path_subclass_assignment(gapfill_year)
    if (!file.exists(path_sub)) {
      stop(
        "Full-year ADOY requires subclass assignment output: ", path_sub,
        "\nRun gapfill.R crop ", gapfill_year, " first (crop + subclass)."
      )
    }
    sub_df <- arrow::read_parquet(path_sub, as_data_frame = TRUE)
    season_use <- as.integer(Sys.getenv("LANDIQ_ADOY_DEFAULT_SEASON", "2"))
    sub_df %>%
      dplyr::transmute(
        parcel_id = as.character(parcel_id),
        year = gapfill_year,
        season = season_use,
        CLASS = trimws(as.character(pred_class)),
        SUBCLASS = .normalize_subclass(pred_subclass_assignment),
        COUNTY = trimws(as.character(county)),
        ADOY = NA_real_
      ) %>%
      dplyr::filter(!is.na(CLASS), CLASS %in% ag_classes)
  } else {
    # Within-year: all seasons from LandIQ; overlay crop-filled subclasses.
    ensure_within_year_crop_prerequisite(gapfill_year)
    load_landiq_adoy_base(gapfill_year) %>%
      overlay_within_year_crop_fill(gapfill_year) %>%
      dplyr::filter(!is.na(CLASS), CLASS %in% ag_classes)
  }

  panel %>%
    dplyr::filter(!CLASS %in% adoy_gapfill_exempt_classes())
}

# --- Driver (gapfill.R adoy) ---------------------------------------------------

#' Run ADOY gap-fill for one year; write landiq_adoy_gapfill_year=Y.parquet.
run_adoy_gapfill <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  mode <- resolve_gapfill_mode(gapfill_year)
  cfg <- resolve_adoy_temporal_cfg(gapfill_year)
  message(
    "=== ADOY gapfill year=", gapfill_year, " mode=", mode,
    " (crop identity from step 1",
    if (identical(mode, "full")) {
      ": subclass assignment"
    } else {
      ": LandIQ + within-year crop overlay"
    },
    "; ADOY exempt CLASS: ", paste(adoy_gapfill_exempt_classes(), collapse = ","),
    ") ==="
  )

  ensure_adoy_reference()
  ref <- load_adoy_reference()
  panel <- load_adoy_target_panel(gapfill_year)
  out <- fill_adoy_panel(panel, ref, cfg)

  path_out <- file.path(
    path_outputs(),
    sprintf("landiq_adoy_gapfill_year=%d.parquet", gapfill_year)
  )
  arrow::write_parquet(out, path_out)

  filled_n <- sum(out$adoy_source != "observed" & out$adoy_source != "unfilled", na.rm = TRUE)
  message(
    "Wrote ADOY gapfill: ", path_out,
    " (filled=", filled_n, " unfilled=", sum(out$adoy_source == "unfilled", na.rm = TRUE), ")"
  )
  message("ADOY source counts:")
  print(out %>% dplyr::count(adoy_source, sort = TRUE))
  invisible(out)
}
