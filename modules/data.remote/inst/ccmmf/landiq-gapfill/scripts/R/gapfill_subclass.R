# SUBCLASS assignment after CLASS is known (full-gap or within-year).
#
# Called from gapfill_run.R / assign_subclass(). Input class_df must include:
#   parcel_id, map_class_avg_mean3 (or class_map_column), neighbor_year_lo/hi
# Within-year: map_class_avg_mean3 is observed LandIQ CLASS.
# Full-gap: map_class_avg_mean3 is predicted CLASS from gapfill_class.R.
#
# Cascade (first hit wins) -> subclass_source labels:
#   1. plurality     -- same parcel + same CLASS in other S2 years; vote
#   2. emission_cdl  -- prior * P(dominant CDL | CLASS::SUBCLASS); score > 0
#   3. prior_only    -- argmax P(SUBCLASS | CLASS)
#   4. unfilled      -- stays **
#
# Special cases after cascade:
#   V still ** -> wine grapes (LANDIQ_VINEYARD_FALLBACK_SUBCLASS); source forced
#                 to observed (product convention, not a fill label)
#   X / I / YP with ** or unfilled -> subclass_source = X/I/YP (no subclass)
#
# Env:
#   LANDIQ_SUBCLASS_PLURALITY_POOL   panel (default) | neighbors
#   LANDIQ_SUBCLASS_PLURALITY_WEIGHT inverse_distance (default) | count
# "panel" = all season-2 years in the harmonized table except gapfill_year.
# "neighbors" = only neighbor_year_lo / neighbor_year_hi.

#' Assign season-2 SUBCLASS for parcels with a known/predicted CLASS.
#'
#' @param gapfill_year calendar year being filled
#' @param class_df one row per parcel with CLASS map column + neighbor years
#' @param emission bundle from load_emission_bundle() (priors + CDL likelihoods)
#' @param use_plurality if FALSE, skip historical vote (emission/prior only)
#' @param class_map_column name of CLASS column on class_df
#' @return class_df plus pred_subclass_assignment, subclass_source, diagnostics
assign_subclass <- function(
    gapfill_year,
    class_df,
    emission,
    use_plurality = TRUE,
    class_map_column = "map_class_avg_mean3") {
  path_landiq_parquet <- path_landiq_parquet()
  path_cdl_dir <- path_cdl_fractions()

  if (!class_map_column %in% names(class_df)) {
    stop("Expected column ", class_map_column, " in class predictions")
  }

  path_cdl_gap <- file.path(path_cdl_dir, sprintf("cdl_fractions_year=%d.parquet", gapfill_year))
  if (!file.exists(path_cdl_gap)) {
    stop("Missing CDL fractions for gap year ", gapfill_year, ": ", path_cdl_gap)
  }

  sub_prob_long <- emission$sub_prob_long
  class_sub_prior <- emission$class_sub_prior
  crop_lk <- emission$crop_lk

  cdl_gap_full <- arrow::read_parquet(path_cdl_gap, as_data_frame = TRUE) %>%
    dplyr::mutate(parcel_id = trimws(as.character(parcel_id)))

  pred <- class_df %>%
    dplyr::transmute(
      parcel_id = as.character(parcel_id),
      gapfill_year = as.integer(gapfill_year),
      pred_class = as.character(.data[[class_map_column]])
    )

  # Empty shells if plurality is off or no votes land.
  plurality <- tibble::tibble(
    parcel_id = character(),
    pred_subclass_plurality = character(),
    n_votes = integer(),
    plurality_vote_weight = numeric(),
    plurality_vote_weight_share = numeric(),
    min_year_dist = numeric(),
    latest_year_at_min_dist = integer(),
    mean_year_dist = numeric()
  )
  total_votes <- tibble::tibble(parcel_id = character(), n_subclass_votes_total = integer())
  n_distinct_sub <- tibble::tibble(parcel_id = character(), n_subclass_candidates = integer())

  # --- Step 1: plurality (same parcel history) ---------------------------------
  if (isTRUE(use_plurality)) {
    if (!all(c("neighbor_year_lo", "neighbor_year_hi") %in% names(class_df))) {
      stop("Class predictions need neighbor_year_lo / neighbor_year_hi for plurality")
    }
    neighboring_years <- sort(unique(c(
      as.integer(class_df$neighbor_year_lo),
      as.integer(class_df$neighbor_year_hi)
    )))
    neighboring_years <- neighboring_years[!is.na(neighboring_years)]

    plurality_pool <- tolower(trimws(Sys.getenv("LANDIQ_SUBCLASS_PLURALITY_POOL", "panel")))
    plurality_weight <- tolower(trimws(Sys.getenv("LANDIQ_SUBCLASS_PLURALITY_WEIGHT", "inverse_distance")))

    # Which other years may cast a vote for this parcel.
    panel_years <- arrow::open_dataset(path_landiq_parquet) %>%
      dplyr::filter(season == 2L) %>%
      dplyr::distinct(year) %>%
      dplyr::collect() %>%
      dplyr::pull(year) %>%
      as.integer() %>%
      sort()
    plurality_years <- if (identical(plurality_pool, "neighbors")) {
      neighboring_years
    } else {
      # panel (default): every S2 year except the fill year
      setdiff(panel_years, gapfill_year)
    }

    liq_hist <- arrow::open_dataset(path_landiq_parquet) %>%
      dplyr::filter(season == 2L) %>%
      dplyr::select(parcel_id, year, CLASS, SUBCLASS) %>%
      dplyr::collect() %>%
      dplyr::mutate(
        parcel_id = as.character(parcel_id),
        year = as.integer(year),
        CLASS = trimws(as.character(CLASS)),
        SUBCLASS = trimws(as.character(SUBCLASS))
      ) %>%
      dplyr::filter(!is.na(CLASS), CLASS != "")

    # Harmonize historical subclasses to 2021 legend before voting.
    liq_hist <- dplyr::bind_rows(lapply(split(liq_hist, liq_hist$year), function(part) {
      apply_landiq_subclass_merge(part, crop_lk$merge, calendar_year = as.integer(part$year[1L]))
    }))

    # Vote only when historical CLASS matches pred_class and SUBCLASS is specific.
    votes_specific <- pred %>%
      dplyr::inner_join(liq_hist %>% dplyr::filter(year %in% plurality_years), by = "parcel_id") %>%
      dplyr::filter(CLASS == pred_class) %>%
      dplyr::mutate(
        is_specific_subclass = !is.na(SUBCLASS) & SUBCLASS != "" & SUBCLASS != "**",
        year_dist = abs(year - gapfill_year),
        vote_w = if (identical(plurality_weight, "count")) 1 else 1 / (1 + year_dist)
      ) %>%
      dplyr::filter(is_specific_subclass)

    vote_stats <- votes_specific %>%
      dplyr::group_by(parcel_id, pred_class, SUBCLASS) %>%
      dplyr::summarise(
        n_votes = dplyr::n(),
        plurality_vote_weight = sum(vote_w, na.rm = TRUE),
        min_year_dist = min(year_dist, na.rm = TRUE),
        latest_year_at_min_dist = max(year[year_dist == min_year_dist], na.rm = TRUE),
        mean_year_dist = mean(year_dist, na.rm = TRUE),
        .groups = "drop"
      )

    plur_weight_total <- vote_stats %>%
      dplyr::group_by(parcel_id) %>%
      dplyr::summarise(plurality_vote_weight_total = sum(plurality_vote_weight, na.rm = TRUE), .groups = "drop")

    # Winner = highest vote weight; ties broken by n_votes, then nearer year, etc.
    plurality <- vote_stats %>%
      dplyr::left_join(plur_weight_total, by = "parcel_id") %>%
      dplyr::mutate(
        plurality_vote_weight_share = plurality_vote_weight / pmax(plurality_vote_weight_total, .Machine$double.eps)
      ) %>%
      dplyr::arrange(
        parcel_id, dplyr::desc(plurality_vote_weight), dplyr::desc(n_votes),
        min_year_dist, dplyr::desc(latest_year_at_min_dist), SUBCLASS
      ) %>%
      dplyr::group_by(parcel_id) %>%
      dplyr::slice_head(n = 1L) %>%
      dplyr::ungroup() %>%
      dplyr::rename(pred_subclass_plurality = SUBCLASS)

    total_votes <- votes_specific %>% dplyr::count(parcel_id, name = "n_subclass_votes_total")
    n_distinct_sub <- vote_stats %>% dplyr::count(parcel_id, name = "n_subclass_candidates")
  }

  # --- Step 2: emission_cdl (dominant CDL code for gap year) -------------------
  # Dominant code = CDL class with largest parcel fraction that year.
  cdl_gap_obs <- cdl_gap_full %>%
    dplyr::mutate(cdl_code = as.integer(cdl_code), parcel_id = as.character(parcel_id)) %>%
    dplyr::filter(!is.na(cdl_code)) %>%
    dplyr::group_by(parcel_id, cdl_code) %>%
    dplyr::summarise(frac_sum = sum(frac, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(parcel_id) %>%
    dplyr::slice_max(order_by = frac_sum, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(obs_key = as.character(cdl_code))

  cdl_keyed <- pred %>%
    dplyr::inner_join(cdl_gap_obs %>% dplyr::select(parcel_id, obs_key, frac_sum), by = "parcel_id")

  # "confusion" naming is historical: score = prior * P(dominant CDL | CLASS::SUBCLASS).
  confusion_expanded <- cdl_keyed %>%
    dplyr::left_join(class_sub_prior, by = c("pred_class" = "CLASS"), relationship = "many-to-many") %>%
    dplyr::left_join(sub_prob_long, by = c("truth_key", "obs_key")) %>%
    dplyr::mutate(prob = dplyr::coalesce(prob, 0))

  confusion_pick <- confusion_expanded %>%
    dplyr::group_by(parcel_id) %>%
    dplyr::mutate(max_prob = max(prob, na.rm = TRUE)) %>%
    dplyr::filter(max_prob > 0, prob > 0) %>%
    dplyr::mutate(score_confusion = prior * prob) %>%
    dplyr::arrange(parcel_id, dplyr::desc(score_confusion), truth_key, SUBCLASS) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      parcel_id,
      pred_subclass_confusion = dplyr::coalesce(SUBCLASS, "**"),
      pred_subclass_confusion_score = score_confusion,
      pred_subclass_confusion_prior = prior,
      has_confusion_subclass = pred_subclass_confusion != "**"
    )

  confusion_map <- pred %>%
    dplyr::left_join(
      cdl_gap_obs %>% dplyr::select(parcel_id, obs_key, frac_sum),
      by = "parcel_id"
    ) %>%
    dplyr::left_join(confusion_pick, by = "parcel_id") %>%
    dplyr::mutate(
      pred_subclass_confusion = dplyr::coalesce(pred_subclass_confusion, "**"),
      pred_subclass_confusion_score = dplyr::if_else(
        pred_subclass_confusion == "**", NA_real_, pred_subclass_confusion_score
      ),
      pred_subclass_confusion_prior = dplyr::if_else(
        pred_subclass_confusion == "**", NA_real_, pred_subclass_confusion_prior
      ),
      has_confusion_subclass = dplyr::coalesce(has_confusion_subclass, FALSE)
    ) %>%
    dplyr::transmute(
      parcel_id,
      cdl_obs_native_code_gap = obs_key,
      cdl_obs_subclass_frac_gap = frac_sum,
      pred_subclass_confusion,
      pred_subclass_confusion_score,
      pred_subclass_confusion_prior,
      has_confusion_subclass
    )

  # --- Step 3: prior_only (no CDL) ---------------------------------------------
  prior_sub <- class_sub_prior %>%
    dplyr::filter(!is.na(SUBCLASS), nzchar(SUBCLASS), SUBCLASS != "**")

  prior_only_map <- pred %>%
    dplyr::left_join(prior_sub, by = c("pred_class" = "CLASS"), relationship = "many-to-many") %>%
    dplyr::group_by(parcel_id) %>%
    dplyr::summarise(
      pred_subclass_prior_only = {
        ok <- !is.na(prior) & !is.na(SUBCLASS) & SUBCLASS != "**"
        if (!any(ok)) "**" else SUBCLASS[ok][which.max(prior[ok])]
      },
      pred_subclass_prior_only_score = {
        ok <- !is.na(prior) & !is.na(SUBCLASS) & SUBCLASS != "**"
        if (!any(ok)) NA_real_ else max(prior[ok], na.rm = TRUE)
      },
      .groups = "drop"
    )

  # --- Combine cascade + special cases ----------------------------------------
  out <- class_df %>%
    dplyr::mutate(parcel_id = as.character(parcel_id)) %>%
    dplyr::left_join(pred, by = "parcel_id") %>%
    dplyr::left_join(
      plurality %>% dplyr::select(
        parcel_id, pred_subclass_plurality, n_votes, plurality_vote_weight,
        plurality_vote_weight_share, min_year_dist, latest_year_at_min_dist, mean_year_dist
      ),
      by = "parcel_id"
    ) %>%
    dplyr::left_join(total_votes, by = "parcel_id") %>%
    dplyr::left_join(n_distinct_sub, by = "parcel_id") %>%
    dplyr::left_join(confusion_map, by = "parcel_id") %>%
    dplyr::left_join(prior_only_map, by = "parcel_id") %>%
    dplyr::mutate(
      pred_subclass_plurality = dplyr::coalesce(pred_subclass_plurality, "**"),
      has_specific_subclass_votes = !is.na(n_subclass_votes_total) & n_subclass_votes_total > 0,
      pred_subclass_confusion = dplyr::coalesce(pred_subclass_confusion, "**"),
      pred_subclass_prior_only = dplyr::coalesce(pred_subclass_prior_only, "**"),
      has_confusion_subclass = dplyr::coalesce(has_confusion_subclass, FALSE),
      subclass_plurality_support = dplyr::if_else(
        !is.na(n_votes) & !is.na(n_subclass_votes_total) & n_subclass_votes_total > 0,
        n_votes / n_subclass_votes_total,
        NA_real_
      ),
      # First non-** among plurality, emission_cdl, prior_only.
      pred_subclass_assignment = dplyr::case_when(
        pred_subclass_plurality != "**" ~ pred_subclass_plurality,
        pred_subclass_confusion != "**" ~ pred_subclass_confusion,
        pred_subclass_prior_only != "**" ~ pred_subclass_prior_only,
        TRUE ~ "**"
      ),
      subclass_source = dplyr::case_when(
        pred_subclass_plurality != "**" ~ "plurality",
        pred_subclass_confusion != "**" ~ "emission_cdl",
        pred_subclass_prior_only != "**" ~ "prior_only",
        TRUE ~ "unfilled"
      ),
      class_map_column = class_map_column
    ) %>%
    dplyr::mutate(
      # Vineyard with no specific subclass: default wine grapes as observed.
      pred_subclass_assignment = dplyr::if_else(
        pred_class == "V" & pred_subclass_assignment == "**",
        vineyard_fallback_subclass(),
        pred_subclass_assignment
      ),
      # V->wine-grapes default is not a separate provenance; keep observed.
      subclass_source = dplyr::if_else(
        pred_class == "V" & subclass_source == "unfilled",
        subclass_source_observed(),
        subclass_source
      ),
      # Idle / young perennial / etc.: leave ** and label as no-subclass by design.
      subclass_source = dplyr::if_else(
        pred_class %in% classes_no_subclass_star() &
          (pred_subclass_assignment == "**" | subclass_source == "unfilled"),
        subclass_source_no_subclass_x_i_yp(),
        subclass_source
      )
    )

  message(
    "Subclass assignment: ",
    sum(out$pred_subclass_assignment != "**", na.rm = TRUE), " / ", nrow(out),
    " (", round(100 * mean(out$pred_subclass_assignment != "**", na.rm = TRUE), 2), "%)"
  )
  invisible(out)
}
