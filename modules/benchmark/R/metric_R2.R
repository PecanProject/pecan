##' @name metric_R2
##' @title Coefficient of Determination (R2)
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_R2 <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Coefficient of Determination (R2)")
  numer <- sum((dat$obvs - mean(dat$obvs)) * (dat$model - mean(dat$model)))
  denom <- sqrt(sum((dat$obvs - mean(dat$obvs)) ^ 2)) * sqrt(sum((dat$model - mean(dat$model)) ^ 2))
  return((numer / denom) ^ 2)
} # metric_R2
