##' @name metric_PPMC
##' @title Pearson Product Moment Correlation
##' @export
##' @param metric_dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery

metric_PPMC <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Pearson Product Moment Correlation")
  # numer <- sum((metric_dat$obvs - mean(metric_dat$obvs)) * (metric_dat$model - mean(metric_dat$model)))
  # denom <- sqrt(sum((metric_dat$obvs - mean(metric_dat$obvs)) ^ 2)) * sqrt(sum((metric_dat$model - mean(metric_dat$model)) ^ 2))
  # return(numer / denom)
  return(stats::cor(metric_dat$obvs, metric_dat$model))
} # metric_PPMC
