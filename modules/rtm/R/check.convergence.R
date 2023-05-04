#' Check convergence of multiple MCMC chains
#' 
#' Uses Gelman multivariate Gelman-Rubin diagnostic to check if 
#' multiple MCMC chains have converged
#' @param jags_out mcmc.list object (from coda package) containing 
#' samples from MCMC chains.
#' @param threshold Gelman-Rubin diagnostic parameter threshold. Default = 1.1
#' @param verbose If TRUE, print convergence result. Default = TRUE
#' @param ... Additional arguments to `gelman.diag` (from coda package)
#' @return List length 3 containing the following:
#'      * convergence: Logical. Whether or not convergence was achieved.
#'      * diagnostics: Numerical value of Gelman-Rubin diagnostics for each parameter and multivariate diagnostic
#'      * error: Logical. Whether or not an error occured in the Gelman-Rubin calculation.  
#' @export
check.convergence <- function(jags_out,
                              threshold = 1.1,
                              verbose = TRUE,
                              ...){
  if (!coda::is.mcmc.list(jags_out)) {
    stop("Input needs to be of class 'mcmc.list'")
  }
  gd <- try(coda::gelman.diag(jags_out, ...))
  if (inherits(gd, "try-error")) {
    warning("Could not calculate Gelman diag. Assuming no convergence.")
    converged <- FALSE
    diagnostics <- NULL
    error <- TRUE
  } else {
    error <- FALSE
    diagnostics <- c(gd$psrf[, 2], mpsrf = gd$mpsrf)
    if (all(diagnostics < threshold)) {
      converged <- TRUE
      msg <- sprintf("Converged with all Gelman diag <= %.3f", min(diagnostics))
    } else {
      converged <- FALSE
      too_large <- diagnostics[diagnostics > threshold]
      msg <- sprintf(
        "The following parameters did not converge: %s",
        paste(sprintf("%s (%.3f)", names(too_large), too_large), collapse = ", ")
      )
    }
    if (verbose) {
      print(msg)
    }
  }
  return(list(converged = converged,
              diagnostics = diagnostics,
              error = error))
}
