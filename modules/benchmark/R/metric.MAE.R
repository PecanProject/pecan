##' @name metric.MAE
##' @title Mean Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
metric.MAE <- function(dat, ...) {
  return(mean(abs(dat$model - dat$obvs)))
} # metric.MAE
