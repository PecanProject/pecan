##' @name metric.RAE
##' @title Relative Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.RAE <- function(dat, ...){
  numer <- mean(abs(dat$obvs - dat$model, na.rm = TRUE), na.rm = TRUE)
  denom <- mean(abs(dat$obvs - mean(dat$obvs, na.rm = TRUE), na.rm = TRUE), na.rm = TRUE)
  score <- numer/denom
  return(score)
}

