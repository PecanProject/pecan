##' @name metric.RAE
##' @title Relative Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
metric.RAE <- function(dat, ...) {
  numer <- mean(abs(dat$obvs - dat$model))
  denom <- mean(abs(dat$obvs - mean(dat$obvs)))
  return(numer/denom)
} # metric.RAE
