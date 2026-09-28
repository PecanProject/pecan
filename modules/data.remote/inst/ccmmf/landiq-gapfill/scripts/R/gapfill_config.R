# Resolve gap-fill run mode, temporal neighbors, and bounding-year behavior.
#
# Modes:
#   full         -- entire calendar year has no usable LandIQ (e.g. 2017). Uses CDL +
#                  one or two LandIQ neighbor years + county transition matrices.
#   within_year  -- LandIQ exists for the year but some parcel-season rows are missing
#                  crop identity (SUBCLASS fill via `gapfill.R crop`).
#
# Temporal neighbors (full mode):
#   both         -- avg-of-three: (p_fwd + p_bwd + p_cdl) / 3
#   before_only  -- bounding last year without a later LandIQ neighbor (e.g. 2023 until 2024)
#   after_only   -- bounding first year without an earlier LandIQ neighbor (e.g. 2016)
#   Single-neighbor cases average the available temporal message with p_cdl (divide by 2).
#
# Env:
#   LANDIQ_GAPFILL_AVAILABLE_YEARS    optional comma list; default = years in
#                                     LANDIQ_HARMONIZED crops parquet minus full-gap years
#   LANDIQ_GAPFILL_NEIGHBORING_YEARS  optional override (1 or 2 years); else auto-resolved
#   LANDIQ_GAPFILL_FULL_GAP_YEARS     optional comma list treated as full-year gaps (default 2017)
#   LANDIQ_GAPFILL_START_YEAR         first calendar year in a batch run (with END_YEAR)
#   LANDIQ_GAPFILL_END_YEAR           last calendar year in a batch run (inclusive)
#   LANDIQ_GAPFILL_RUN_YEARS          optional comma list (overrides START/END)
#   LANDIQ_SUBCLASS_PRIOR_YEARS       optional comma list for subclass prior (default: all parquet years - full gaps)
#   CDL_LANDIQ_TRAINING_YEARS         optional comma list for emission training (default: prior years with CDL parquets)
#   CDL_LANDIQ_TRAINING_YEAR_MIN/MAX  optional manual emission range (overrides auto when both set)
#   CDL_LANDIQ_TRAINING_EXCLUDE_YEARS default 2017

.gapfill_parse_year_csv <- function(txt) {
  parts <- trimws(strsplit(txt, ",", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(integer(0))
  }
  y <- suppressWarnings(as.integer(parts))
  if (any(is.na(y))) {
    stop("Invalid year list: ", txt)
  }
  sort(unique(y))
}

resolve_gapfill_run_years <- function() {
  explicit <- trimws(Sys.getenv("LANDIQ_GAPFILL_RUN_YEARS", ""))
  if (nzchar(explicit)) {
    return(.gapfill_parse_year_csv(explicit))
  }

  start_env <- trimws(Sys.getenv("LANDIQ_GAPFILL_START_YEAR", ""))
  end_env <- trimws(Sys.getenv("LANDIQ_GAPFILL_END_YEAR", ""))
  if (!nzchar(start_env) || !nzchar(end_env)) {
    stop(
      "Set LANDIQ_GAPFILL_START_YEAR and LANDIQ_GAPFILL_END_YEAR ",
      "(inclusive range), or LANDIQ_GAPFILL_RUN_YEARS as a comma-separated list."
    )
  }

  y0 <- as.integer(start_env)[1L]
  y1 <- as.integer(end_env)[1L]
  if (is.na(y0) || is.na(y1)) {
    stop(
      "LANDIQ_GAPFILL_START_YEAR and LANDIQ_GAPFILL_END_YEAR must be integers; got: ",
      start_env, ", ", end_env
    )
  }
  if (y1 < y0) {
    stop("LANDIQ_GAPFILL_END_YEAR (", y1, ") must be >= START_YEAR (", y0, ")")
  }
  seq.int(y0, y1)
}

landiq_gapfill_full_gap_years <- function() {
  .gapfill_parse_year_csv(Sys.getenv("LANDIQ_GAPFILL_FULL_GAP_YEARS", "2017"))
}

#' Calendar years present in the (non-gap-filled) LandIQ product.
landiq_product_years <- function() {
  yrs <- arrow::open_dataset(path_landiq_parquet()) %>%
    dplyr::distinct(year) %>%
    dplyr::collect() %>%
    dplyr::pull(year) %>%
    as.integer() %>%
    sort()
  yrs[!is.na(yrs)]
}

#' Years with real LandIQ usable as neighbors / training support.
#' Default: all years in LANDIQ_HARMONIZED minus full-gap years.
landiq_gapfill_available_years <- function() {
  env <- Sys.getenv("LANDIQ_GAPFILL_AVAILABLE_YEARS", "")
  if (nzchar(env)) {
    return(.gapfill_parse_year_csv(env))
  }
  yrs <- setdiff(landiq_product_years(), landiq_gapfill_full_gap_years())
  if (length(yrs) == 0L) {
    stop(
      "No LandIQ years found in ", path_landiq_parquet(),
      ". Set LANDIQ_HARMONIZED or LANDIQ_GAPFILL_AVAILABLE_YEARS."
    )
  }
  yrs
}

landiq_gapfill_bound_min <- function() {
  min(landiq_gapfill_available_years())
}

landiq_gapfill_bound_max <- function() {
  max(landiq_gapfill_available_years())
}

#' Calendar years used to build the subclass prior (P(subclass | CLASS)).
#' Default: every season-2 year in harmonized LandIQ, minus full-gap years (e.g. 2017).
landiq_subclass_prior_years <- function() {
  env <- trimws(Sys.getenv("LANDIQ_SUBCLASS_PRIOR_YEARS", ""))
  if (nzchar(env)) {
    return(.gapfill_parse_year_csv(env))
  }
  yrs <- arrow::open_dataset(path_landiq_parquet()) %>%
    dplyr::filter(season == 2L) %>%
    dplyr::distinct(year) %>%
    dplyr::collect() %>%
    dplyr::pull(year) %>%
    as.integer() %>%
    sort()
  yrs <- yrs[!is.na(yrs)]
  setdiff(yrs, landiq_gapfill_full_gap_years())
}

#' Calendar years for CDL x LandIQ emission training (requires CDL fraction parquets).
#' Default: every landiq_subclass_prior_years() year with cdl_fractions_year=Y.parquet present.
landiq_emission_training_years <- function() {
  explicit <- trimws(Sys.getenv("CDL_LANDIQ_TRAINING_YEARS", ""))
  if (nzchar(explicit)) {
    yrs <- .gapfill_parse_year_csv(explicit)
    return(landiq_emission_apply_exclusions(yrs))
  }

  yr_min_env <- trimws(Sys.getenv("CDL_LANDIQ_TRAINING_YEAR_MIN", ""))
  yr_max_env <- trimws(Sys.getenv("CDL_LANDIQ_TRAINING_YEAR_MAX", ""))
  if (nzchar(yr_min_env) && nzchar(yr_max_env)) {
    yr_min <- as.integer(yr_min_env)[1L]
    yr_max <- as.integer(yr_max_env)[1L]
    if (is.na(yr_min) || is.na(yr_max) || yr_min > yr_max) {
      stop("CDL_LANDIQ_TRAINING_YEAR_MIN/MAX must be integers with MIN <= MAX")
    }
    return(landiq_emission_apply_exclusions(seq.int(yr_min, yr_max)))
  }

  landiq_years <- landiq_subclass_prior_years()
  cdl_dir <- path_cdl_fractions()
  has_cdl <- vapply(landiq_years, function(y) {
    file.exists(file.path(cdl_dir, sprintf("cdl_fractions_year=%d.parquet", y)))
  }, logical(1))
  skipped <- landiq_years[!has_cdl]
  if (length(skipped) > 0L) {
    message(
      "Emission training skips LandIQ years without CDL fractions: ",
      paste(skipped, collapse = ", ")
    )
  }
  landiq_emission_apply_exclusions(landiq_years[has_cdl])
}

landiq_emission_apply_exclusions <- function(years) {
  excluded <- .gapfill_parse_year_csv(
    Sys.getenv("CDL_LANDIQ_TRAINING_EXCLUDE_YEARS", "2017")
  )
  yrs <- sort(setdiff(as.integer(years), excluded))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) == 0L) {
    stop("No emission training years after exclusions")
  }
  yrs
}

resolve_gapfill_mode <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  if (is.na(gapfill_year)) {
    stop("gapfill_year must be a valid integer")
  }
  if (gapfill_year %in% landiq_gapfill_full_gap_years()) {
    return("full")
  }
  "within_year"
}

resolve_gapfill_neighbors <- function(gapfill_year) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  mode <- resolve_gapfill_mode(gapfill_year)
  if (!identical(mode, "full")) {
    stop(
      "resolve_gapfill_neighbors() is for full-year gap fill; year ", gapfill_year,
      " is not in LANDIQ_GAPFILL_FULL_GAP_YEARS (",
      paste(landiq_gapfill_full_gap_years(), collapse = ", "),
      "). Use `gapfill.R crop` (within-year mode) instead."
    )
  }

  override <- Sys.getenv("LANDIQ_GAPFILL_NEIGHBORING_YEARS", "")
  if (nzchar(trimws(override))) {
    neighbor_years <- .gapfill_parse_year_csv(override)
    if (length(neighbor_years) == 0L || length(neighbor_years) > 2L) {
      stop(
        "LANDIQ_GAPFILL_NEIGHBORING_YEARS must list 1 or 2 years; got: ",
        override
      )
    }
    if (length(neighbor_years) == 2L) {
      y_lo <- min(neighbor_years)
      y_hi <- max(neighbor_years)
      if (!(y_lo < gapfill_year && gapfill_year < y_hi)) {
        stop(
          "With two neighboring years, GAPFILL_YEAR (", gapfill_year,
          ") must lie strictly between them (", y_lo, ", ", y_hi, ")"
        )
      }
      temporal_mode <- "both"
    } else if (neighbor_years[1L] < gapfill_year) {
      y_lo <- neighbor_years[1L]
      y_hi <- NA_integer_
      temporal_mode <- "before_only"
    } else {
      y_lo <- NA_integer_
      y_hi <- neighbor_years[1L]
      temporal_mode <- "after_only"
    }
  } else {
    available <- landiq_gapfill_available_years()
    before <- available[available < gapfill_year]
    after <- available[available > gapfill_year]
    y_lo <- if (length(before) > 0L) max(before) else NA_integer_
    y_hi <- if (length(after) > 0L) min(after) else NA_integer_

    if (!is.na(y_lo) && !is.na(y_hi)) {
      temporal_mode <- "both"
    } else if (!is.na(y_lo)) {
      temporal_mode <- "before_only"
    } else if (!is.na(y_hi)) {
      temporal_mode <- "after_only"
    } else {
      stop(
        "No LandIQ neighbor years found for full gap year ", gapfill_year,
        ". Set LANDIQ_GAPFILL_AVAILABLE_YEARS or LANDIQ_GAPFILL_NEIGHBORING_YEARS."
      )
    }
    neighbor_years <- c(y_lo, y_hi)
    neighbor_years <- neighbor_years[!is.na(neighbor_years)]
  }

  bound_min <- landiq_gapfill_bound_min()
  bound_max <- landiq_gapfill_bound_max()
  future <- landiq_gapfill_available_years()
  future <- future[future > gapfill_year]
  next_landiq_year <- if (length(future) > 0L) min(future) else NA_integer_

  list(
    gapfill_year = gapfill_year,
    mode = mode,
    neighbor_years = neighbor_years,
    y_lo = y_lo,
    y_hi = y_hi,
    temporal_mode = temporal_mode,
    n_temporal_signals = if (identical(temporal_mode, "both")) 2L else 1L,
    is_bounding_first = identical(gapfill_year, bound_min),
    is_bounding_last = identical(gapfill_year, bound_max),
    needs_rerun_when_year_available = if (
      identical(gapfill_year, bound_max) && identical(temporal_mode, "before_only") &&
        !is.na(next_landiq_year)
    ) {
      next_landiq_year
    } else {
      NA_integer_
    }
  )
}

#' Default SUBCLASS for vineyard (CLASS V) when no specific subclass is known.
#' LandIQ tabular data often has V with missing subclass; policy treats these as wine grapes.
#' Provenance stays `observed` (not a separate fallback flag).
vineyard_fallback_subclass <- function() {
  trimws(Sys.getenv("LANDIQ_VINEYARD_FALLBACK_SUBCLASS", "2"))
}

#' Canonical subclass_source when identity comes from source LandIQ (or V->V/2 default).
subclass_source_observed <- function() {
  "observed"
}

#' CLASS X / I / YP keep SUBCLASS ** by design (not a failed fill).
subclass_source_no_subclass_x_i_yp <- function() {
  "X/I/YP (no subclass)"
}

classes_no_subclass_star <- function() {
  c("X", "I", "YP")
}

#' Relabel subclass_source for product consistency.
#' - OBSERVED / vineyard_fallback -> observed
#' - unfilled on X/I/YP (or those classes with **) -> X/I/YP (no subclass)
normalize_subclass_source <- function(class, subclass, source) {
  cls <- trimws(as.character(class))
  sub <- trimws(as.character(subclass))
  src <- trimws(as.character(source))
  src[is.na(src) | !nzchar(src)] <- NA_character_
  src[toupper(src) == "OBSERVED" | src == "vineyard_fallback"] <- subclass_source_observed()
  no_sub <- cls %in% classes_no_subclass_star() &
    (
      is.na(sub) | !nzchar(sub) | sub == "**" |
        (!is.na(src) & src == "unfilled")
    )
  src[no_sub] <- subclass_source_no_subclass_x_i_yp()
  src
}

gapfill_run_summary <- function(cfg) {
  msg <- paste0(
    "gapfill year=", cfg$gapfill_year,
    " mode=", cfg$mode,
    " temporal=", cfg$temporal_mode,
    " neighbors=", paste(cfg$neighbor_years, collapse = ",")
  )
  if (!is.na(cfg$needs_rerun_when_year_available)) {
    msg <- paste0(
      msg,
      " | NOTE: rerun when LandIQ ", cfg$needs_rerun_when_year_available, " is available"
    )
  }
  msg
}
