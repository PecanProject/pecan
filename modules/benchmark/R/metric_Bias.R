#' Mean Bias Error (BIAS)
#'
#' @name metric_Bias
#' @title Mean Bias Error
#' @export
#' @param dat dataframe with columns `model` and `obvs`
#' @param ... ignored
#' @return A numeric value representing the mean bias (mean(model - obvs)).
#' @details
#' Computes the mean bias error between model predictions and observations.
metric_Bias <- function(dat, ...) {
  if (!all(c("model", "obvs") %in% names(dat))) {
    PEcAn.logger::logger.severe("Metric BIAS requires 'model' and 'obvs' columns in the input dataframe.")
  }

  PEcAn.logger::logger.info("Metric: Mean Bias Error")

  valid <- !is.na(dat$model) & !is.na(dat$obvs)
  if (!any(valid)) {
    return(NA_real_)
  }

  mean(dat$model[valid] - dat$obvs[valid])
}
