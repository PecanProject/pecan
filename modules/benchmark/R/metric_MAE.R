##' @name metric_MAE
##' @title Mean Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
##' 
metric_MAE <- function(dat, ...) {
  logger.info("Metric: Mean Absolute Error")
  return(mean(abs(dat$model - dat$obvs)))
} # metric_MAE
