##' @name metric_RAE
##' @title Relative Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_RAE <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Relative Absolute Error")
  numer <- mean(abs(dat$obvs - dat$model))
  denom <- mean(abs(dat$obvs - mean(dat$obvs)))
  return(numer/denom)
} # metric_RAE
