##' Mean Absolute Error
##' @param dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery
##' 
##' @export
metric_MAE <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Mean Absolute Error")
  return(mean(abs(dat$model - dat$obvs),na.rm=TRUE))
} # metric_MAE
