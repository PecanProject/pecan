#' Continuous Ranked Probability Score (CRPS)
#'
#' @name metric_CRPS
#' @title Continuous Ranked Probability Score
#' @export
#' @param dat dataframe with column `obvs` and either an `ensemble_matrix` attribute,
#' wide ensemble member columns (`member_1`, `member_2`, ...), or matrix column `model_ensemble`.
#' @param ... ignored
#' @return A numeric value representing the mean CRPS across observations.
#' @details
#' Computes the Continuous Ranked Probability Score (CRPS) to evaluate model uncertainty
#' and ensemble skill against observation data. Uses `scoringRules::crps_sample()` when available,
#' with a pure R analytical sample score fallback when `scoringRules` is not present.

metric_CRPS <- function(dat, ...) {
  if (!"obvs" %in% names(dat)) {
    PEcAn.logger::logger.severe("Metric CRPS requires 'obvs' column in the input dataframe.")
  }

  ens_mat <- NULL

  # Check if dat has ensemble_matrix attribute attached
  if (!is.null(attr(dat, "ensemble_matrix"))) {
    ens_mat <- attr(dat, "ensemble_matrix")
  } else if ("model_ensemble" %in% names(dat) && is.matrix(dat$model_ensemble)) {
    ens_mat <- dat$model_ensemble
  } else {
    # Check for wide member columns (e.g. member_1, member_2, ...) or ens_1, ens_2, ...
    mem_cols <- grep("^(member|ens|param)_?[0-9]+$", names(dat), value = TRUE, ignore.case = TRUE)
    if (length(mem_cols) > 0) {
      ens_mat <- as.matrix(dat[, mem_cols, drop = FALSE])
    }
  }

  if (is.null(ens_mat)) {
    PEcAn.logger::logger.severe("Metric CRPS requires ensemble members matrix or member columns in dataset.")
  }

  valid <- !is.na(dat$obvs) & apply(!is.na(ens_mat), 1, any)
  if (!any(valid)) {
    return(NA_real_)
  }

  y <- dat$obvs[valid]
  ens_sub <- ens_mat[valid, , drop = FALSE]

  PEcAn.logger::logger.info("Metric: Continuous Ranked Probability Score (CRPS)")

  if (requireNamespace("scoringRules", quietly = TRUE)) {
    scores <- scoringRules::crps_sample(y = y, dat = ens_sub)
    return(mean(scores, na.rm = TRUE))
  } else {
    # Pure-R fallback sample CRPS calculation: E|X - y| - 0.5 * E|X - X'|
    PEcAn.logger::logger.info("scoringRules package not available; using pure-R fallback for CRPS sample score.")
    scores <- sapply(seq_along(y), function(i) {
      obs <- y[i]
      samps <- ens_sub[i, !is.na(ens_sub[i, ])]
      if (length(samps) == 0) return(NA_real_)
      term1 <- mean(abs(samps - obs))
      term2 <- mean(abs(outer(samps, samps, "-"))) / 2
      term1 - term2
    })
    return(mean(scores, na.rm = TRUE))
  }
}
