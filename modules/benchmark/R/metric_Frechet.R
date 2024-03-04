##' @name metric_Frechet
##' @title Frechet Distance
##' @export
##' @param metric_dat dataframe
##' @param ... ignored
##' 
##' @author Betsy Cowdery

metric_Frechet <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Frechet Distance")
  dat.no.na <- stats::na.omit(metric_dat)
  Fdist <- SimilarityMeasures::Frechet(as.matrix(dat.no.na$obvs), as.matrix(dat.no.na$model))
  return(Fdist)
} # metric_Frechet
