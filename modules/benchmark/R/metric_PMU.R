##' @name metric_PMU
##' @title Predictive Model Uncertainty (PMU)
##' @export
##' @param dat dataframe with columns `obs_se` and `obs_n`
##' @param ... ignored
##' @details
##' Calculates the pooled measurement uncertainty.
##' Requires `obs_se` (standard error) and `obs_n` (replicate counts) in the observation data.

metric_PMU <- function(dat, ...) {
  if (!"obs_se" %in% names(dat) || !"obs_n" %in% names(dat)) {
    PEcAn.logger::logger.severe("Metric PMU requires 'obs_se' and 'obs_n' columns in the dataset.")
  }
  
  PEcAn.logger::logger.info("Metric: Predictive Model Uncertainty (PMU)")
  
  # Calculate pooled standard error
  # Formula: sqrt(sum(SE^2 * n) / sum(n))
  valid <- !is.na(dat$obs_se) & !is.na(dat$obs_n)
  
  se2_n <- (dat$obs_se[valid]^2) * dat$obs_n[valid]
  pooled_var <- sum(se2_n) / sum(dat$obs_n[valid])
  
  return(sqrt(pooled_var))
}
