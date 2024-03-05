##' Absolute Maximum Error
##'
##' @param dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery
##' @export

metric_AME <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Absolute Maximum Error")
  return(max(abs(dat$model - dat$obvs),na.rm = TRUE))
} # metric_AME
