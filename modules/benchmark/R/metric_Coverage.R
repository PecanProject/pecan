#' @name metric_Coverage
#' @title Prediction Interval Coverage
#' @export
#' @param dat dataframe with columns `model_q05` and `model_q95` (or `model_q025` and `model_q975`)
#' @param ... ignored
#' @return A numeric value representing the fraction of observations that fall within the prediction interval.
#' @details
#' Measures the fraction of observations that fall within the model's
#' stated prediction interval (defaults to 90% interval via `model_q05`/`model_q95`, or 95% via `model_q025`/`model_q975`).

metric_Coverage <- function(dat, ...) {
  q_low <- NULL
  q_high <- NULL
  
  if (all(c("model_q05", "model_q95") %in% names(dat))) {
    q_low <- dat$model_q05
    q_high <- dat$model_q95
  } else if (all(c("model_q025", "model_q975") %in% names(dat))) {
    q_low <- dat$model_q025
    q_high <- dat$model_q975
  } else {
    PEcAn.logger::logger.severe("Metric Coverage requires quantile columns ('model_q05'/'model_q95' or 'model_q025'/'model_q975') in the dataset.")
  }
  
  PEcAn.logger::logger.info("Metric: Prediction Interval Coverage")
  
  valid <- !is.na(dat$obvs) & !is.na(q_low) & !is.na(q_high)
  
  if ("obvs_sd" %in% names(dat)) {
    valid <- valid & !is.na(dat$obvs_sd)
    covered <- (dat$obvs[valid] - dat$obvs_sd[valid]) <= q_high[valid] &
               (dat$obvs[valid] + dat$obvs_sd[valid]) >= q_low[valid]
  } else {
    covered <- dat$obvs[valid] >= q_low[valid] & dat$obvs[valid] <= q_high[valid]
  }
  
  return(mean(covered))
}
