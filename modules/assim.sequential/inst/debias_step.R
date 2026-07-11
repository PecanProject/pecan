#' Debias preprocessing utilities (internal)
#'
#' A small, **pure** helper module that prepares inputs for the residual
#' debiasing step used by `sda.enkf.multisite()`. These functions **do not**
#' read from or mutate `settings`; they operate only on the objects passed in.
#'
#' The module provides:
#' 1. A stable name map between observation variable names and state names.
#' 2. Site filtering helpers (toggle-able):
#'    - drop sites with **incomplete** covariates for the current year;
#'    - drop sites that become **inconsistent** in their observed variables over time
#'      (e.g., a site reported AGB in 2012 but is missing AGB in 2013).
#' 3. Covariate extraction for a specific date/year, aligned to the **column order**
#'    of the state matrix `X` (one row per column of `X`).
#' 4. Observation-vector builder aligned to `X`’s columns for a given time index.
#' 5. Diagnostics utilities (per-column pre/post/obs comparison and per-variable RMSE).
#' 6. A one-step debias application that:
#'    - learns residuals at t from data up to t–1,
#'    - mean-shifts the ensemble at t,
#'    - returns metrics and learner weights for logging.
#'
#' @section Conventions:
#' - **Columns of `X`** correspond to site–variable pairs in the vectors `site_index`
#'   and `col_vars`. All alignment is performed using these two vectors.
#' - **Time indexing `t`** follows the SDA driver (`t` is the current step, `t-1` is
#'   the most-recent completed step with observations to train on).
#' - **Covariates** are provided in a *long* data frame with columns `site`, `year`,
#'   and one column per covariate layer.
#'
#' @keywords internal
#' @name debias_helpers
#' @noRd
NULL

# ------------------------------------------------------------------------------
# (1) Name mapping
# ------------------------------------------------------------------------------

#' Map observation names -> state names
#'
#' This mapping is applied whenever observations are merged into the state layout.
#' If your upstream naming changes, **edit here** to keep the rest of the code stable.
#'
#' @rdname debias_helpers
#' @keywords internal
debias_name_map <- c(
  AGB   = "AbvGrndWood",
  LAI   = "LAI",
  SMP   = "SoilMoistFrac",
  SoilC = "TotSoilCarb"
)

# ------------------------------------------------------------------------------
# (2) Site filtering utilities
# ------------------------------------------------------------------------------

#' Build tidy rows for learner weights (internal)
#'
#' Returns a data.frame with columns: time, var, learner, weight.
#' If `w_named` has no names, they are auto-labeled as learner_1..k.
#'
#' @keywords internal
debias_weights_rows <- function(time_label, var, w_named) {
  if (is.null(names(w_named)) || any(!nzchar(names(w_named)))) {
    names(w_named) <- paste0("learner_", seq_along(w_named))
  }
  data.frame(
    time    = rep(as.character(time_label), length(w_named)),
    var     = rep(as.character(var),        length(w_named)),
    learner = names(w_named),
    weight  = as.numeric(w_named),
    stringsAsFactors = FALSE
  )
}

#' Sites with complete covariates in a given year
#'
#' Returns the subset of `candidate_sites` whose covariate row at `year` has **no NA**
#' in any covariate feature column.
#'
#' @param covariates_df A long data frame with columns `site`, `year`, and one column
#'   per covariate feature (e.g., climate, soils, topography layers).
#' @param year Integer year to check (extracted via `lubridate::year()` elsewhere).
#' @param candidate_sites Character vector of site ids to intersect with.
#'
#' @return Character vector of sites that have no missing covariates in `year`.
#'
#' @examples
#' cov_df <- data.frame(
#'   site = c("A", "A", "B", "B"),
#'   year = c(2012, 2013, 2012, 2013),
#'   cov1 = c(1, NA, 2, 3),
#'   cov2 = c(4, 5, 6, NA)
#' )
#'
#' debias_sites_with_complete_covariates_year(cov_df, 2012, c("A", "B"))
#'
#' @rdname debias_helpers
#' @keywords internal
debias_sites_with_complete_covariates_year <- function(covariates_df, year, candidate_sites) {
  df_year <- covariates_df[
    covariates_df$year == as.integer(year) & covariates_df$site %in% candidate_sites,
    , drop = FALSE
  ]
  if (nrow(df_year) == 0) return(character(0))
  
  cov_cols <- setdiff(names(df_year), c("site", "year"))
  if (length(cov_cols) == 0) return(character(0))
  
  ok_mask <- rowSums(is.na(df_year[, cov_cols, drop = FALSE])) == 0L
  df_year$site[ok_mask]
}

#' Sites that become inconsistent in observed variables over time
#'
#' A site is flagged **inconsistent at `t_idx`** if it is missing *any* variable at time
#' `t_idx` that it had reported in **any earlier time** (1..`t_idx-1`). This prevents
#' training/evaluation on sites that drop variables mid-series.
#'
#' @param obs.mean A list indexed by time (`[[t]]`), each entry a named list by `site`
#'   containing named numeric vectors of observed variables for that site/time.
#'   Names are observation names and will be remapped via `name_map`.
#' @param t_idx Integer time index to assess (1-based, consistent with SDA loop).
#' @param name_map Optional named character vector mapping observation names to state names.
#'
#' @return Character vector of **site ids** that are inconsistent at `t_idx`.
#'
#' @details
#' This function only inspects **presence/absence** of variables, not their values.
#'
#' @rdname debias_helpers
#' @keywords internal
debias_sites_inconsistent_obs <- function(obs.mean, t_idx, name_map = debias_name_map) {
  if (t_idx <= 1L) return(character(0))
  
  observed_vars_at <- function(tt, site_id) {
    om <- obs.mean[[tt]][[as.character(site_id)]]
    if (is.null(om)) return(character(0))
    vn <- names(om)
    if (!is.null(name_map)) {
      keep <- vn %in% names(name_map)
      if (any(keep)) vn[keep] <- unname(name_map[vn[keep]])
    }
    vn
  }
  
  all_sites <- unique(unlist(lapply(obs.mean[seq_len(t_idx)], function(om_t) names(om_t))), use.names = FALSE)
  all_sites <- as.character(all_sites)
  
  inconsistent <- character(0)
  for (s in all_sites) {
    prev_union <- unique(unlist(lapply(seq_len(t_idx - 1L), observed_vars_at, site_id = s), use.names = FALSE))
    if (length(prev_union) == 0) next
    cur_vars <- observed_vars_at(t_idx, s)
    if (length(setdiff(prev_union, cur_vars)) > 0) {
      inconsistent <- c(inconsistent, s)
    }
  }
  unique(inconsistent)
}

# ------------------------------------------------------------------------------
# (3) Covariate accessors aligned to time and X’s layout
# ------------------------------------------------------------------------------

#' Covariates for a date, with optional site filtering
#'
#' Filters sites for the year of `obs_date` according to:
#' - `drop_incomplete_covariates`: remove sites with any NA in covariate features.
#' - `enforce_consistent_obs`: remove sites that became inconsistent up to `t_idx`.
#'
#' Returns a data frame for that **year × eligible sites**, sorted by `site`, with
#' attributes listing which sites were dropped (useful for logging).
#'
#' @param covariates_df Long data frame with columns `site`, `year`, and feature columns.
#' @param obs_date Date–time corresponding to the **previous or current** SDA step.
#'   Only the **year** is used.
#' @param site_index Character or numeric vector giving the **site id per column of X**.
#' @param obs.mean Observation structure (see `debias_sites_inconsistent_obs()`).
#' @param t_idx Integer time index used to evaluate consistency up to t or t–1.
#' @param drop_incomplete_covariates Logical. If `TRUE`, drop sites with any missing
#'   covariate this year; if `FALSE`, keep all sites present in `covariates_df` for that year.
#' @param enforce_consistent_obs Logical. If `TRUE`, drop sites that became inconsistent
#'   in observations up to `t_idx`. Requires `obs.mean` and `t_idx`.
#'
#' @return A data frame with columns `site`, `year`, and covariate features for **eligible sites**.
#'   Attributes:
#'   - `dropped_missing_covariates`: sites removed for missing covariates,
#'   - `dropped_inconsistent_obs`: sites removed for observation inconsistency (if enforced).
#'
#' @title Covariates for a date, with optional site filtering
#' @name debias_get_covariates_for_date
#' @rdname debias_get_covariates_for_date
#' @keywords internal
debias_get_covariates_for_date <- function(covariates_df,
                                           obs_date,
                                           site_index,
                                           obs.mean,
                                           t_idx,
                                           drop_incomplete_covariates = TRUE,
                                           enforce_consistent_obs = TRUE) {
  if (is.null(covariates_df)) {
    stop("covariates_df is NULL. Provide columns: site, year, <features...>.")
  }
  yr <- lubridate::year(obs_date)
  sites_used <- unique(as.character(site_index))
  
  # (1) Filter by complete covariates (optional)
  if (isTRUE(drop_incomplete_covariates)) {
    complete_sites <- debias_sites_with_complete_covariates_year(covariates_df, yr, sites_used)
  } else {
    complete_sites <- intersect(
      sites_used,
      as.character(covariates_df$site[covariates_df$year == as.integer(yr)])
    )
  }
  
  # (2) Optionally enforce observation consistency up to t_idx
  if (isTRUE(enforce_consistent_obs)) {
    if (is.null(obs.mean) || is.null(t_idx)) {
      stop("obs.mean and t_idx must be provided when enforce_consistent_obs = TRUE.")
    }
    inconsistent_sites <- debias_sites_inconsistent_obs(obs.mean, t_idx, name_map = debias_name_map)
    eligible_sites <- setdiff(complete_sites, inconsistent_sites)
  } else {
    eligible_sites <- complete_sites
  }
  
  if (length(eligible_sites) == 0) {
    return(dplyr::tibble(site = character(0), year = integer(0)))
  }
  
  df_year <- covariates_df[
    covariates_df$year == as.integer(yr) & covariates_df$site %in% eligible_sites,
    , drop = FALSE
  ]
  df_year <- df_year[order(df_year$site), , drop = FALSE]
  
  # Annotate drops for diagnostics/logging
  attr(df_year, "dropped_missing_covariates") <- setdiff(sites_used, complete_sites)
  if (isTRUE(enforce_consistent_obs)) {
    attr(df_year, "dropped_inconsistent_obs") <- intersect(sites_used, debias_sites_inconsistent_obs(obs.mean, t_idx))
  }
  
  df_year
}

#' Expand per-site covariates to **row-per-column** alignment
#'
#' Converts the per-site covariate data into a matrix aligned with the **columns of `X`**.
#' For any column whose site was filtered out, the function inserts a row of `NA`
#' features to preserve shape and ordering.
#'
#' @param covariates_df See `debias_get_covariates_for_date()`.
#' @param obs_date Date for which to fetch covariates (year is used).
#' @param site_index Site id per column of `X`.
#' @param obs.mean Observation structure used if enforcing consistency.
#' @param t_idx Time index associated with `obs_date`.
#' @param drop_incomplete_covariates, enforce_consistent_obs See above.
#'
#' @return A numeric matrix with **nrow = length(site_index)** and one column per
#'   covariate feature. Rows align 1:1 with the columns of `X`.
#'
#' @title Expand per-site covariates to row-per-column alignment
#' @name debias_cov_by_columns
#' @rdname debias_cov_by_columns
#' @keywords internal
debias_cov_by_columns <- function(covariates_df,
                                  obs_date,
                                  site_index,
                                  obs.mean,
                                  t_idx,
                                  drop_incomplete_covariates = TRUE,
                                  enforce_consistent_obs = TRUE) {
  df_year <- debias_get_covariates_for_date(
    covariates_df = covariates_df,
    obs_date      = obs_date,
    site_index    = site_index,
    obs.mean      = obs.mean,
    t_idx         = t_idx,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  
  if (nrow(df_year) == 0) {
    # Preserve outer shape; no features to return.
    return(matrix(numeric(0), nrow = length(site_index), ncol = 0))
  }
  
  feat_cols <- setdiff(names(df_year), c("site", "year"))
  idx <- match(as.character(site_index), df_year$site)
  
  # Filler row of NA to keep alignment when a site is missing
  na_row <- as.list(rep(NA_real_, length(feat_cols)))
  names(na_row) <- feat_cols
  filler <- dplyr::as_tibble(na_row)
  
  rows <- lapply(seq_along(idx), function(i) {
    j <- idx[i]
    if (is.na(j)) filler else df_year[j, feat_cols, drop = FALSE]
  })
  out <- dplyr::bind_rows(rows)
  
  as.matrix(out)
}

# ------------------------------------------------------------------------------
# (4) Observation vector aligned to X’s columns
# ------------------------------------------------------------------------------

#' Observation vector for time `t_idx`, aligned to `X` columns
#'
#' Builds a vector with one entry per **column of `X`**, using the `(site, var)` layout
#' defined by `site_index` and `col_vars`. Observation names in `obs.mean` are
#' remapped through `name_map`.
#'
#' @param t_idx Time index (1-based) to pull observations from `obs.mean`.
#' @param site_index Site id per column of `X`.
#' @param col_vars Variable name (state-space name) per column of `X`.
#' @param obs.mean Observation structure (list by time → list by site → named numeric).
#' @param name_map Optional map from observation names → state names.
#'
#' @return Numeric vector of length `length(col_vars)` with `NA` where not observed.
#'
#' @rdname debias_helpers
#' @keywords internal
debias_obs_vec_for_time <- function(t_idx, site_index, col_vars, obs.mean, name_map = debias_name_map) {
  om  <- obs.mean[[t_idx]]
  out <- rep(NA_real_, length(col_vars))
  
  for (s in unique(site_index)) {
    vals <- om[[as.character(s)]]
    if (is.null(vals)) next
    
    # Normalize names into state-space naming
    if (!is.null(name_map)) {
      keep <- names(vals) %in% names(name_map)
      if (any(keep)) names(vals)[keep] <- unname(name_map[names(vals)[keep]])
    }
    
    v_here <- unique(col_vars[site_index == s])
    vnames <- intersect(names(vals), v_here)
    for (v in vnames) {
      idx <- which(site_index == s & col_vars == v)
      if (length(idx)) out[idx] <- as.numeric(vals[[v]][1])
    }
  }
  out
}

# ------------------------------------------------------------------------------
# (5) Diagnostics helpers
# ------------------------------------------------------------------------------

#' Column-wise comparison table (pre/post/obs)
#'
#' @param site_index Site id per column of `X`.
#' @param col_vars Variable name per column of `X`.
#' @param pre_mean Vector of pre-debias column means at time `t`.
#' @param post_mean Vector of post-debias column means at time `t`.
#' @param obs_vec Observation vector for time `t` (aligned).
#'
#' @return Data frame with columns: `site`, `var`, `pre`, `post`, `obs`
#'   sorted by (`var`, `site`).
#'
#' @rdname debias_helpers
#' @keywords internal
debias_build_comp_df <- function(site_index, col_vars, pre_mean, post_mean, obs_vec) {
  df <- data.frame(
    site = site_index,
    var  = col_vars,
    pre  = as.numeric(pre_mean),
    post = as.numeric(post_mean),
    obs  = as.numeric(obs_vec),
    stringsAsFactors = FALSE
  )
  df[order(df$var, df$site), ]
}

#' RMSE by variable (pre/post vs obs)
#'
#' Computes RMSE for each state variable, separately for the pre- and post-debias
#' column means against observations. NAs are ignored per variable.
#'
#' @param comp_df Output of `debias_build_comp_df()`.
#'
#' @return Data frame with columns: `var`, `rmse_pre`, `rmse_post`.
#'
#' @rdname debias_helpers
#' @keywords internal
debias_rmse_by_var <- function(comp_df) {
  rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
  do.call(
    rbind,
    lapply(split(comp_df, comp_df$var), function(d) {
      data.frame(
        var       = d$var[1],
        rmse_pre  = rmse(d$pre,  d$obs),
        rmse_post = rmse(d$post, d$obs),
        stringsAsFactors = FALSE
      )
    })
  )
}

# ------------------------------------------------------------------------------
# (6) Per-step debias application (uses site filtering + covariates)
# ------------------------------------------------------------------------------

#' Apply residual debiasing for a single SDA time step
#'
#' At time `t`, this function:
#' 1. Builds a training set from **t–1**: residuals `y = obs_prev - raw_prev` and features
#'    `[covariates_prev, raw_prev]` for each variable.
#' 2. Trains/updates the Python-side learner (`py$train_full_model`).
#' 3. Predicts residuals at **t** using `[covariates_t, raw_mean_t]`.
#' 4. Mean-shifts the ensemble `X` by adding predicted residuals to `raw_mean_t`.
#' 5. Computes per-variable metrics (RMSE, MAE, bias, R²) pre vs post.
#'
#' @param t Integer current time index (t > 1 required to train from t–1).
#' @param obs.t Character or time label for logging (e.g., ISO date string for `t`).
#' @param X Numeric matrix of the **current** ensemble at time `t` (members × columns).
#' @param raw_prev Numeric vector of the **raw** column mean at `t-1`.
#' @param raw_mean_t Numeric vector of the **raw** column mean at `t`.
#' @param site_index Vector of site ids per column of `X`.
#' @param col_vars Vector of variable names per column of `X` (state-space names).
#' @param obs.times Datetime vector indexed by `t` (length ≥ `t`) for covariate year lookup.
#' @param obs.mean Observation structure (time → site → named numeric vector).
#' @param covariates_df Long data frame with columns `site`, `year`, and feature columns.
#' @param py Python bridge object exposing:
#'   - `train_full_model(name, X, y)`,
#'   - `predict_residual(name, X)`,
#'   - `get_model_weights(name)`,
#'   - `has_model(name)` (logical).
#' @param train_buf R environment used to accumulate training rows per variable across steps.
#' @param name_map Optional map from observation names → state names.
#' @param drop_incomplete_covariates Logical; if `TRUE`, drop sites with missing covariates (per year).
#' @param enforce_consistent_obs Logical; if `TRUE`, drop sites inconsistent up to relevant `t_idx`.
#' @param require_obs_at_t_for_predict Logical; if `TRUE`, only predict residuals for columns with
#' @param state.interval Matrix/data.frame of per-variable bounds; either rownames = variable with columns `min`,`max`,
#'   or a data frame with a `variable` column plus `min`/`max`. Used to clip post-debias values.
#' @param clip_lower_bound Numeric; minimum floor applied to lower bounds (default 0.01).
#'   **observations present at t** (useful for constrained comparisons).
#'
#' @return A list with:
#' \describe{
#'   \item{X}{The **mean-shifted** ensemble matrix at time `t` (same dims as input `X`).}
#'   \item{weights_entry}{Optional named list of learner weights by variable (if provided by `py`).}
#'   \item{weights_df_rows}{A tidy data frame of weights emitted this step (time, var, learner, weight).}
#'   \item{diag}{A list with:
#'     \itemize{
#'       \item `comp`: per-column comparison table (`pre`, `post`, `obs`) for diagnostics.
#'       \item `rmse`: per-variable metrics (RMSE/MAE/bias/R²) **pre vs post**.
#'     }
#'   }
#'   \item{rmse_rows}{A tidy slice of metrics with the `time` column attached for easy logging.}
#' }
#'
#' @note
#' - If covariates are missing (no feature columns) for either `t-1` or `t`, the function
#'   returns `X` unchanged and emits NA metrics (shape-preserving behavior).
#' - Predicted residuals that are non-finite are coerced to 0 to avoid contaminating `X`.
#' - The **mean-shift** keeps the ensemble spread intact: we subtract the raw mean and
#'   add the corrected mean (`raw_mean_t + predicted_residual`).
#'
#' @title Apply residual debiasing for a single SDA time step
#' @name sda_apply_debias_step
#' @rdname sda_apply_debias_step
#' @keywords internal
sda_apply_debias_step <- function(
    t, obs.t, X, raw_prev, raw_mean_t,
    site_index, col_vars,
    obs.times, obs.mean,
    covariates_df, py, train_buf,
    name_map = debias_name_map,
    drop_incomplete_covariates = TRUE,
    enforce_consistent_obs = TRUE,
    require_obs_at_t_for_predict = FALSE,
    state.interval = state.interval,              # <-- pass it through
    clip_lower_bound = 0.01
) {
  # Early return when we cannot train from t-1 or covariates are absent
  if (t <= 1 || is.null(covariates_df)) {
    return(list(
      X = X,
      weights_entry   = NULL,
      weights_df_rows = utils::head(data.frame(time=character(), var=character(), learner=character(), weight=numeric()), 0),
      diag = list(
        comp = debias_build_comp_df(site_index, col_vars, raw_mean_t, raw_mean_t, rep(NA_real_, length(col_vars))),
        rmse = data.frame(
          var       = unique(col_vars),
          rmse_pre  = NA_real_, rmse_post = NA_real_,
          mae_pre   = NA_real_, mae_post  = NA_real_,
          bias_pre  = NA_real_, bias_post = NA_real_,
          r2_pre    = NA_real_, r2_post   = NA_real_
        )
      ),
      rmse_rows = utils::head(data.frame(
        time=character(), var=character(),
        rmse_pre=numeric(), rmse_post=numeric(),
        mae_pre=numeric(),  mae_post=numeric(),
        bias_pre=numeric(), bias_post=numeric(),
        r2_pre=numeric(),   r2_post=numeric()
      ), 0)
    ))
  }
  
  # Build obs/covariates for training (t-1) and prediction (t)
  obs_prev_vec <- debias_obs_vec_for_time(t - 1, site_index, col_vars, obs.mean, name_map)
  
  cov_prev_mat <- debias_cov_by_columns(
    covariates_df = covariates_df, obs_date = obs.times[t - 1],
    site_index = site_index, obs.mean = obs.mean, t_idx = t - 1,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  cov_t_mat <- debias_cov_by_columns(
    covariates_df = covariates_df, obs_date = obs.times[t],
    site_index = site_index, obs.mean = obs.mean, t_idx = t,
    drop_incomplete_covariates = drop_incomplete_covariates,
    enforce_consistent_obs     = enforce_consistent_obs
  )
  
  # If no feature columns, skip debias but keep outputs well-formed
  if (ncol(cov_prev_mat) == 0 || ncol(cov_t_mat) == 0) {
    return(list(
      X = X,
      weights_entry   = NULL,
      weights_df_rows = utils::head(data.frame(time=character(), var=character(), learner=character(), weight=numeric()), 0),
      diag = list(
        comp = debias_build_comp_df(site_index, col_vars, raw_mean_t, raw_mean_t, rep(NA_real_, length(col_vars))),
        rmse = data.frame(
          var       = unique(col_vars),
          rmse_pre  = NA_real_, rmse_post = NA_real_,
          mae_pre   = NA_real_, mae_post  = NA_real_,
          bias_pre  = NA_real_, bias_post = NA_real_,
          r2_pre    = NA_real_, r2_post   = NA_real_
        )
      ),
      rmse_rows = utils::head(data.frame(
        time=character(), var=character(),
        rmse_pre=numeric(), rmse_post=numeric(),
        mae_pre=numeric(),  mae_post=numeric(),
        bias_pre=numeric(), bias_post=numeric(),
        r2_pre=numeric(),   r2_post=numeric()
      ), 0)
    ))
  }
  
  pred_resid <- numeric(ncol(X))
  vars <- unique(col_vars)
  weights_entry <- list()
  weights_df_rows <- utils::head(
    data.frame(time=character(), var=character(), learner=character(), weight=numeric(), stringsAsFactors = FALSE), 0
  )
  feature_rows <- utils::head(
    data.frame(time=character(), var=character(), feature=character(), importance=numeric(),
               stringsAsFactors = FALSE), 0
  )
  
  
  # Optionally restrict predictions to positions with obs at t (diagnostic mode)
  obs_t_avail <- if (require_obs_at_t_for_predict) {
    !is.na(debias_obs_vec_for_time(t, site_index, col_vars, obs.mean, name_map))
  } else rep(TRUE, length(col_vars))
  
  # Train per variable on t-1 residuals; predict at t
  for (v in vars) {
    cols_v    <- which(col_vars == v)
    y_v_all   <- obs_prev_vec[cols_v] - as.numeric(raw_prev[cols_v])  # residuals at t-1
    Xprev_all <- cbind(cov_prev_mat[cols_v, , drop = FALSE],
                       raw = as.numeric(raw_prev[cols_v]))
    mask <- !is.na(y_v_all) & stats::complete.cases(Xprev_all)
    
    fi_logged <- FALSE  # <- NEW: track whether we captured FI for this var at this step
    
    if (any(mask)) {
      # Accumulate per-variable training buffer
      rec <- if (exists(v, train_buf, inherits = FALSE)) get(v, train_buf) else list(X = NULL, y = NULL)
      rec$X <- rbind(rec$X, Xprev_all[mask, , drop = FALSE])
      rec$y <- c(rec$y,  y_v_all[mask])
      assign(v, rec, train_buf)
      fi_ret <- py$train_full_model(
        name = as.character(v),
        X = as.matrix(rec$X),
        y = as.numeric(rec$y),
        feature_names = colnames(rec$X)
      )
      
      # Works whether convert=TRUE or FALSE:
      if (!is.null(fi_ret)) {
        fi_ret <- tryCatch(reticulate::py_to_r(fi_ret), error = function(e) fi_ret)
        fn <- as.character(unlist(fi_ret[["names"]], use.names = FALSE))
        fv <- as.numeric(unlist(fi_ret[["importances"]], use.names = FALSE))
        if (length(fn) == length(fv) && length(fn) > 0) {
          feature_rows <- rbind(
            feature_rows,
            data.frame(
              time       = rep(obs.t, length(fn)),
              var        = rep(as.character(v), length(fn)),
              feature    = fn,
              importance = fv,
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      # Optional: collect mixing weight for diagnostics (e.g., KNN vs TREE)
      w_now <- try(py$get_model_weights(as.character(v)), silent = TRUE)
      if (!inherits(w_now, "try-error") && !is.null(w_now) && is.finite(w_now)) {
        w_now <- min(max(as.numeric(w_now), 0), 1)
        w_named <- c(KNN = w_now, TREE = 1 - w_now)
        weights_entry[[as.character(v)]] <- w_named
        weights_df_rows <- rbind(weights_df_rows, debias_weights_rows(obs.t, as.character(v), w_named))
      }
    }
    
    
    # Predict at t for available positions
    if (py$has_model(as.character(v))) {
      Xt_v <- cbind(cov_t_mat[cols_v, , drop = FALSE],
                    raw = as.numeric(raw_mean_t[cols_v]))
      ok <- stats::complete.cases(Xt_v) & obs_t_avail[cols_v]
      if (any(ok)) {
        preds <- py$predict_residual(as.character(v), Xt_v[ok, , drop = FALSE])
        pred_resid[cols_v[ok]] <- as.numeric(preds)
      }
    }
  }
  
  # Defensive: replace any non-finite predicted residual with 0
  pred_resid[!is.finite(pred_resid)] <- 0
  
  # Compute corrected mean and diagnostics
  pre_mean  <- raw_mean_t
  post_mean <- raw_mean_t + pred_resid
  obs_t_vec <- debias_obs_vec_for_time(t, site_index, col_vars, obs.mean, name_map)
  
  comp_df  <- debias_build_comp_df(site_index, col_vars, pre_mean, post_mean, obs_t_vec)
  
  metric_one <- function(pred, obs) {
    ok   <- is.finite(pred) & is.finite(obs)
    if (!any(ok)) return(c(rmse=NA_real_, mae=NA_real_, bias=NA_real_, r2=NA_real_))
    e    <- pred[ok] - obs[ok]
    rmse <- sqrt(mean(e^2)); mae <- mean(abs(e)); bias <- mean(e)
    sst  <- sum((obs[ok] - mean(obs[ok]))^2)
    r2   <- if (sst > 0 && sum(ok) >= 2) 1 - sum(e^2) / sst else NA_real_
    c(rmse=rmse, mae=mae, bias=bias, r2=r2)
  }
  
  metrics_by_var <- do.call(
    rbind,
    lapply(split(comp_df, comp_df$var), function(d) {
      m_pre  <- metric_one(d$pre,  d$obs)
      m_post <- metric_one(d$post, d$obs)
      data.frame(
        var        = d$var[1],
        rmse_pre   = m_pre["rmse"],  rmse_post = m_post["rmse"],
        mae_pre    = m_pre["mae"],   mae_post  = m_post["mae"],
        bias_pre   = m_pre["bias"],  bias_post = m_post["bias"],
        r2_pre     = m_pre["r2"],    r2_post   = m_post["r2"],
        stringsAsFactors = FALSE
      )
    })
  )
  diag_metrics <- metrics_by_var
  metrics_by_var$time <- obs.t
  rmse_rows <- metrics_by_var[, c("time","var","rmse_pre","rmse_post","mae_pre","mae_post","bias_pre","bias_post","r2_pre","r2_post")]
  
  # Mean-shift ensemble: preserve spread, adjust mean
  offsets   <- sweep(X, 2, raw_mean_t, FUN = "-")
  corrected <- post_mean
  X_new     <- sweep(offsets, 2, corrected, FUN = "+")
  .get_interval <- function(v) {
    if (!is.null(rownames(state.interval)) && v %in% rownames(state.interval)) {
      as.numeric(state.interval[v, , drop = TRUE])
    } else if ("variable" %in% colnames(state.interval)) {
      hit <- which(state.interval[["variable"]] == v)
      if (length(hit) == 1L) {
        as.numeric(state.interval[hit, setdiff(colnames(state.interval), "variable"), drop = TRUE])
      } else c(NA_real_, NA_real_)
    } else c(NA_real_, NA_real_)
  }
  
  for (j in seq_len(ncol(X_new))) {
    v  <- as.character(col_vars[j])
    iv <- .get_interval(v)       # c(min, max)
    lb <- iv[1]; ub <- iv[2]
    lb <- if (is.finite(lb)) max(lb, clip_lower_bound) else clip_lower_bound
    ub <- if (is.finite(ub)) ub else Inf
    X_new[, j] <- pmin(pmax(X_new[, j], lb), ub)
  }
  list(
    X = X_new,
    weights_entry   = if (length(weights_entry)) weights_entry else NULL,
    weights_df_rows = weights_df_rows,
    diag            = list(comp = comp_df, rmse = diag_metrics),
    rmse_rows       = rmse_rows,
    feature_rows    = feature_rows    
  )
}
