##' @name metric_R2
##' @title Squared Pearson Correlation (R²)
##' @export
##' @param metric_dat dataframe with columns \code{model} and \code{obvs}
##' @param ... ignored
##'
##' @details
##' Computes R-squared using the correlation-based formula:
##' \eqn{R^2 = \left(\frac{\sum(obs - \bar{obs})(mod - \bar{mod})}
##' {\sqrt{\sum(obs - \bar{obs})^2} \cdot \sqrt{\sum(mod - \bar{mod})^2}}\right)^2}
##'
##' @author Betsy Cowdery
metric_R2 <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Squared Pearson Correlation (R²)")
  stats::cor(metric_dat$model, metric_dat$obvs, use = "pairwise.complete.obs")^2
} # metric_R2