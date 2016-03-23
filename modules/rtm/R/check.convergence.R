#' @name check.convergence
#' @title Check convergence of multiple MCMC chains
#' @details Uses Gelman multivariate Gelman-Rubin diagnostic to check if 
#' multiple MCMC chains have converged
#' @param mcmc.samples.list mcmc.list object (from coda package) containing 
#' samples from MCMC chains.
#' @param threshold Gelman-Rubin diagnostic parameter threshold. Default = 1.1
#' @param verbose If TRUE, print convergence result. Default = TRUE
#' @param autoburnin Whether or not to automaticall perform burnin in 
#' calculation of Gelman-Rubin diagnostic. Default = FALSE
#' @param ... Additional arguments to `gelman.diag` (from coda package)
#' @return List length 3 containing the following:
#'
#'      convergence: Logical. Whether or not convergence was achieved.
#'
#'      diagnostic: Numerical value of the Gelman-Rubin multivariate diagnostic
#'
#'      error: Logical. Whether or not an error occured in the Gelman-Rubin calculation.  
check.convergence <- function(mcmc.samples.list, 
                              threshold = 1.1,
                              verbose = TRUE,
                              autoburnin = FALSE,
                              ...){
    require(coda)
    if(class(mcmc.samples.list) != "mcmc.list") stop("Input needs to be of class 'mcmc.list'")
    gd <- try(gelman.diag(mcmc.samples.list, autoburnin = autoburnin, ...))
    if(class(gd) == "try-error"){
        warning("Could not calculate Gelman diag. Returning NULL")
        converged <- NULL
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
            msg <- sprintf("Did not converge (Gelman Diag = %.3f). Trying again.",
                           diagnostic)
        }
        if(verbose) print(msg)
    }
    return(list(converged = converged,
                diagnostic = diagnostic,
                error = error))
}
