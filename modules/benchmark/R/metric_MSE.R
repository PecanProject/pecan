##' @name metric_MSE
##' @title Mean Square Error
##' @export
##' @param dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery

metric_MSE <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Mean Square Error")
  return(mean((dat$model - dat$obvs) ^ 2,na.rm=TRUE))
} # metric_MSE
