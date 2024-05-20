##' @name metric_RAE
##' @title Relative Absolute Error
##' @export
##' @param metric_dat dataframe
##' @param ... ignored
##'
##' @author Betsy Cowdery

metric_RAE <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Relative Absolute Error")
  metric_dat <- stats::na.omit(metric_dat)
  numer <- mean(abs(metric_dat$obvs - metric_dat$model))
  denom <- mean(abs(metric_dat$obvs - mean(metric_dat$obvs)))
  return(numer/denom)
} # metric_RAE
