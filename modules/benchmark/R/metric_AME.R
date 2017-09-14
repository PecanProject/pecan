##' @name metric_AME
##' @title Absolute Maximum Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_AME <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Absolute Maximum Error")
  return(max(abs(dat$model - dat$obvs),na.rm = TRUE))
} # metric_AME
