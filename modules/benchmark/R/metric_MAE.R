##' @name metric_MAE
##' @title Mean Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery
##' 
metric_MAE <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Mean Absolute Error")
  return(mean(abs(dat$model - dat$obvs),na.rm=TRUE))
} # metric_MAE
