#' @name check.convergence
#' @title Check convergence of multiple MCMC chains
#' @details Uses Gelman multivariate Gelman-Rubin diagnostic to check if 
#' multiple MCMC chains have converged
#' @param mcmc.samples.list mcmc.list object (from coda package) containing 
#' samples from MCMC chains.
#' @param threshold Gelman-Rubin diagnostic parameter threshold. Default = 1.1
#' @param verbose If TRUE, print convergence result. Default = TRUE
#' @param ... Additional arguments to `gelman.diag` (from coda package)
#' @return List length 3 containing the following:
#'
#'      convergence: Logical. Whether or not convergence was achieved.
#'
#'      diagnostic: Numerical value of the Gelman-Rubin multivariate diagnostic
#'
#'      error: Logical. Whether or not an error occured in the Gelman-Rubin calculation.  
#' @export
check.convergence <- function(mcmc.samples.list, 
                              threshold = 1.1,
                              verbose = TRUE,
                              ...){
    library(coda)
    if(class(mcmc.samples.list) != "mcmc.list") stop("Input needs to be of class 'mcmc.list'")
    gd <- try(gelman.diag(mcmc.samples.list, ...))
    if(class(gd) == "try-error"){
        warning("Could not calculate Gelman diag. Assuming no convergence.")
        converged <- FALSE
        diagnostic <- NULL
        error <- TRUE
    } else {
        error <- FALSE
        diagnostic <- gd$mpsrf
        if(diagnostic < threshold){
            converged <- TRUE
            msg <- sprintf("Converged with Gelman diag = %.3f", diagnostic)
        } else {
            converged <- FALSE
            msg <- sprintf("Did not converge (Gelman Diag = %.3f).",
                           diagnostic)
        }
        if(verbose) print(msg)
    }
    return(list(converged = converged,
                diagnostic = diagnostic,
                error = error))
}

