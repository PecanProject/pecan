##' @name metric.RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.RMSE <- function(dat, ...) {
  logger.info("Metric: Root Mean Square Error")
  return(sqrt(mean((dat$model - dat$obvs) ^ 2)))
} # metric.RMSE
