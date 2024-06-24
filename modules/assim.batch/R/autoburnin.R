#' @title Calculate burnin value
#' 
#' @description Automatically detect burnin based on one of several methods.
#' @param jags_out List of MCMC sample matrices or `mcmc.list` object
#' @param threshold Maximum value of Gelman diagnostic
#' @param method Character string indicating method. Options are 
#' "moving.window" (default) or "gelman.plot".
#' @param use.confidence Logical. If TRUE (default), use 95% confidence 
#' interval for Gelman Diagnostic. If FALSE, use the point estimate.
#' @param plotfile path
#' @param ... Other parameters to methods
#' 
#' @details 
#' See "gelman_diag_mw" and "gelman_diag_gelmanPlot"
#' 
#' @examples
#'      z1 <- coda::mcmc(c(rnorm(2500, 5), rnorm(2500, 0)))
#'      z2 <- coda::mcmc(c(rnorm(2500, -5), rnorm(2500, 0)))
#'      z <- coda::mcmc.list(z1, z2)
#'      burnin <- getBurnin(z, threshold = 1.05)
#' @author Alexey Shiklomanov, Michael Dietze
#' @export

getBurnin <- function(jags_out,
                      threshold = 1.1, 
                      use.confidence = TRUE,
                      method = "moving.window",
                      plotfile = "/dev/null",
                      ...) {
  if (method == "moving.window") {
    GBR <- try(gelman_diag_mw(jags_out, ...))
  } else if (method == "gelman.plot") {
    GBR <- try(gelman_diag_gelmanPlot(jags_out, ...))
  } else {
    stop("Unknown method: ", method)
  }
  if (inherits(GBR, "try-error")) {
    message("Unable to calculate Gelman diagnostic. Assuming no convergence.")
    return(1)
  }
  column <- ifelse(use.confidence, 2, 1)
  gbr_values <- GBR[, -(1:2), column, drop = FALSE]
  gbr_exceed <- gbr_values > threshold
  if (all(!gbr_exceed)) {
    # Chains converged instantly -- no burnin required
    burnin <- 2     # This isn't 1 to allow testing for convergence with `burnin == 1`
  } else {
    index <- utils::tail(which(rowSums(gbr_exceed) > 0), 1) + 1
    stopifnot(length(index) == 1,
               inherits(index, c("numeric", "integer")))
    if (index > dim(GBR)[1]) {
      burnin <- NA
    } else {
      burnin <- GBR[index, "Start", column]
    }
  }
  if (is.na(burnin)) {
    message("*** Chains have not converged yet ***")
    mvals <- as.data.frame(matrix(gbr_values, nrow(gbr_values), ncol(gbr_values)))
    colnames(mvals) <- colnames(gbr_values)
    mex <- as.data.frame(matrix(gbr_exceed, nrow(gbr_exceed), ncol(gbr_exceed)))
    colnames(mex) <- sprintf("PSRF %s > %.2f", colnames(gbr_exceed), threshold)
    print(cbind(utils::tail(mvals), utils::tail(mex)))
    burnin <- 1
  }
  return(burnin)
} # getBurnin

#' @title Automatically calculate and apply burnin value
#'
#' @author Michael Dietze, Alexey Shiklomanov
#' @param jags_out JAGS output
#' @param return.burnin Logical. If `TRUE`, return burnin value in addition to 
#' samples (as list). Default = FALSE.
#' @param ... Additional arguments for \code{getBurnin}, \code{gelman_diag_mw}, 
#' and \code{gelman.diag}.
#' @examples
#'      z1 <- coda::mcmc(c(rnorm(2500, 5), rnorm(2500, 0)))
#'      z2 <- coda::mcmc(c(rnorm(2500, -5), rnorm(2500, 0)))
#'      z <- coda::mcmc.list(z1, z2)
#'      z_burned <- autoburnin(z)
#' @export
autoburnin <- function(jags_out, return.burnin = FALSE, ...) {
  burnin <- getBurnin(jags_out, ...)
  if (burnin == 1) {
    samples <- jags_out
  } else if (burnin > 1) {
    samples <- stats::window(jags_out, start = burnin)
  } else {
    stop("Bad return value for burnin: \n",
         burnin)
  }
  if (return.burnin) {
    out <- list(samples = samples, burnin = burnin)
  } else {
    out <- samples
  }
  return(out)
} # autoburnin

#' @title Make MCMC list from samples list
#'
#' @param samps samples list (output from invert.custom)
#' @export
makeMCMCList <- function(samps) {
  samps.mcmc <- lapply(samps, coda::mcmc)
  stopifnot(all(sapply(samps.mcmc, coda::is.mcmc)))
  samps.mcmc.list <- coda::mcmc.list(samps.mcmc)
  stopifnot(coda::is.mcmc.list(samps.mcmc.list))
  return(samps.mcmc.list)
} # makeMCMCList
