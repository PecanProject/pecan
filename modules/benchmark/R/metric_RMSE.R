##' @name metric_RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_RMSE <- function(dat, ...) {
  logger.info("Metric: Root Mean Square Error")
  return(sqrt(mean((dat$model - dat$obvs) ^ 2)))
} # metric_RMSE
