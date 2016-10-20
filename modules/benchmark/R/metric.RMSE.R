##' @name metric.RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
metric.RMSE <- function(dat, ...) {
  return(sqrt(mean((dat$model - dat$obvs) ^ 2)))
} # metric.RMSE
