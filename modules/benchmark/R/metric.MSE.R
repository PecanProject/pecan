##' @name metric.MSE
##' @title Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.MSE <- function(dat, ...) {
  logger.info("Metric: Mean Square Error")
  return(mean((dat$model - dat$obvs) ^ 2))
} # metric.MSE
