##' @name metric.AME
##' @title Absolute Maximum Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.AME <- function(dat, ...) {
  return(max(abs(dat$model - dat$obvs)))
} # metric.AME
