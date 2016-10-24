##' @name metric.PPMC
##' @title Pearson Product Moment Correlation
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
metric.PPMC <- function(dat, ...) {
  numer <- sum((dat$obvs - mean(dat$obvs)) * (dat$model - mean(dat$model)))
  denom <- sqrt(sum((dat$obvs - mean(dat$obvs)) ^ 2)) * sqrt(sum((dat$model - mean(dat$model)) ^ 2))
  return(numer / denom)
} # metric.PPMC
