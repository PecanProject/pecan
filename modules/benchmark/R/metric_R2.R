##' @name metric_R2
##' @title Squared Pearson Correlation (R2)
##' @export
##' @param metric_dat dataframe with columns \code{model} and \code{obvs}
##' @param ... ignored
##'
##' @details
##' Computes R-squared using the correlation-based formula:
##' \eqn{R^2 = \left(\frac{\sum(obs - \bar{obs})(mod - \bar{mod})}
##' {\sqrt{\sum(obs - \bar{obs})^2} \cdot \sqrt{\sum(mod - \bar{mod})^2}}\right)^2}
##' 
##' Note: Because this is a correlation-based R2, it is invariant to bias and slope.
##' A model that is perfectly correlated but biased (e.g., model = obs + 100) will still
##' score 1. This is distinct from variance explained or Nash-Sutcliffe Efficiency (NSE).
##'
##' @author Betsy Cowdery
metric_R2 <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Squared Pearson Correlation (R2)")
  stats::cor(metric_dat$model, metric_dat$obvs, use = "pairwise.complete.obs")^2
} # metric_R2
