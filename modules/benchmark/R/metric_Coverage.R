##' @name metric_Coverage
##' @title Prediction Interval Coverage
##' @export
##' @param dat dataframe with columns `model_q025` and `model_q975`
##' @param ... ignored
##' @details
##' Measures the fraction of observations that fall within the model's
##' stated 95% prediction interval.

metric_Coverage <- function(dat, ...) {
  if (!"model_q025" %in% names(dat) || !"model_q975" %in% names(dat)) {
    PEcAn.logger::logger.severe("Metric Coverage requires 'model_q025' and 'model_q975' columns in the dataset.")
  }
  
  PEcAn.logger::logger.info("Metric: Prediction Interval Coverage")
  
  valid <- !is.na(dat$obvs) & !is.na(dat$model_q025) & !is.na(dat$model_q975)
  covered <- dat$obvs[valid] >= dat$model_q025[valid] & dat$obvs[valid] <= dat$model_q975[valid]
  
  return(mean(covered))
}
