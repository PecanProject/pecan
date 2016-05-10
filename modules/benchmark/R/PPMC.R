##' @name metric.PPMC
##' @title Pearson Product Moment Correlation
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.MSE <- function(dat, ...){
  numer <- mean(abs(dat$model - dat$obvs, na.rm = TRUE), na.rm = TRUE)
  denom <- mean(abs(dat$model - mean(dat$model, na.rm = TRUE), na.rm = TRUE), na.rm = TRUE)
  score <- numer/denom
  return(score)
}

