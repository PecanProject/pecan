##' @name metric.RAE
##' @title Relative Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.RAE <- function(dat, ...) {
  logger.info("Metric: Relative Absolute Error")
  numer <- mean(abs(dat$obvs - dat$model))
  denom <- mean(abs(dat$obvs - mean(dat$obvs)))
  return(numer/denom)
} # metric.RAE
