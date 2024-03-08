##' @name metric_RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery

metric_RMSE <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Root Mean Square Error")
  return(sqrt(mean((dat$model - dat$obvs) ^ 2,na.rm=TRUE)))
} # metric_RMSE
