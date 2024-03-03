##' @name metric_R2
##' @title Coefficient of Determination (R2)
##' @export
##' @param metric_dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery

metric_R2 <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Coefficient of Determination (R2)")
  numer <- sum((metric_dat$obvs - mean(metric_dat$obvs)) * (metric_dat$model - mean(metric_dat$model)))
  denom <- sqrt(sum((metric_dat$obvs - mean(metric_dat$obvs)) ^ 2)) * sqrt(sum((metric_dat$model - mean(metric_dat$model)) ^ 2))
  
  out <- (numer / denom) ^ 2
  
  if(is.na(out)){
    fit <- stats::lm(metric_dat$model ~ metric_dat$obvs)
    out <- summary(fit)$r.squared
  }
  
  return(out)
  
} # metric_R2
