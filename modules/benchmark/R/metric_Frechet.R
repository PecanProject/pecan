##' @name metric_Frechet
##' @title Frechet Distance
##' @export
##' @param metric_dat dataframe
##' @importFrom SimilarityMeasures Frechet
##' @importFrom PEcAn.utils logger.info
##' 
##' @author Betsy Cowdery

metric_Frechet <- function(metric_dat, ...) {
  logger.info("Metric: Frechet Distance")
  dat.no.na <- na.omit(metric_dat)
  Fdist <- Frechet(as.matrix(dat.no.na$obvs), as.matrix(dat.no.na$model))
  return(Fdist)
} # metric_Frechet
