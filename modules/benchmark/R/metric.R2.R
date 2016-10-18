##' @name metric.R2
##' @title Coefficient of Determination
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
metric.R2 <- function(dat, ...) {
  numer <- sum((dat$obvs - mean(dat$obvs)) * (dat$model - mean(dat$model)))
  denom <- sqrt(sum((dat$obvs - mean(dat$obvs)) ^ 2)) * sqrt(sum((dat$model - mean(dat$model)) ^ 2))
  (numer / denom) ^ 2
} # metric.R2
