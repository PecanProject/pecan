##' @name metric_MSE
##' @title Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_MSE <- function(dat, ...) {
  PEcAn.utils::logger.info("Metric: Mean Square Error")
  return(mean((dat$model - dat$obvs) ^ 2))
} # metric_MSE
