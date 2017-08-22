##' @name metric_cor
##' @title Correlation Coefficient
##' @export
##' @param dat dataframe
##' 
##' @author Mike Dietze

metric_cor <- function(dat, ...) {
  PEcAn.logger::logger.info("Metric: Correlation Coefficient")
  return(cor(dat$model,dat$obvs,use ="pairwise.complete.obs"))
} # metric_cor
