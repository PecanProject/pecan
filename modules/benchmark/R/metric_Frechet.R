##' @name metric_Frechet
##' @title Frechet Distance
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric_Frechet <- function(dat, ...) {
  PEcAn.utils::logger.info("Metric: Frechet Distance")
  Fdist <- SimilarityMeasures::Frechet(as.matrix(dat$obvs), as.matrix(dat$model))
  return(Fdist)
} # metric_Frechet
