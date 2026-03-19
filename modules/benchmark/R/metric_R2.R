##' @name metric_R2
##' @title Coefficient of Determination (R2)
##' @export
##' @param metric_dat dataframe with columns \code{model} and \code{obvs}
##' @param ... ignored
##'
##' @details
##' Computes R-squared using the correlation-based formula:
##' \eqn{R^2 = \left(\frac{\sum(obs - \bar{obs})(mod - \bar{mod})}
##' {\sqrt{\sum(obs - \bar{obs})^2} \cdot \sqrt{\sum(mod - \bar{mod})^2}}\right)^2}
##'
##' If this formula returns \code{NA} (e.g. when model output is constant
##' across all observations), the function silently falls back to an
##' \code{lm()}-based R-squared via \code{summary(lm())$r.squared}.
##' This fallback may produce unreliable results and triggers a warning
##' from \code{stats::summary.lm}: "essentially perfect fit: summary may
##' be unreliable". Consider checking for constant model output before
##' calling this function.
##'
##' @author Betsy Cowdery
metric_R2 <- function(metric_dat, ...) {
  PEcAn.logger::logger.info("Metric: Coefficient of Determination (R2)")
  numer <- sum((metric_dat$obvs - mean(metric_dat$obvs)) * (metric_dat$model - mean(metric_dat$model)))
  denom <- sqrt(sum((metric_dat$obvs - mean(metric_dat$obvs)) ^ 2)) * sqrt(sum((metric_dat$model - mean(metric_dat$model)) ^ 2))
  
  out <- (numer / denom) ^ 2
  
  # If correlation formula returns NA (e.g. constant model output),
  # fall back to lm()-based R-squared. Note: this fallback may trigger
  # "essentially perfect fit" warning from stats::summary.lm and
  # produce unreliable results in edge cases.
  if(is.na(out)){
    fit <- stats::lm(metric_dat$model ~ metric_dat$obvs)
    out <- summary(fit)$r.squared
  }
  
  return(out)
  
} # metric_R2