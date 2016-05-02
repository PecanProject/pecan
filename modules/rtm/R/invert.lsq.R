#'@name invert.lsq
#'@title Least squares model inversion
#'@author Alexey Shiklomanov
#'@details Performs a least-squares inversion of an arbitrary radiative 
#'transfer model (passed as an R function). The inversion attempts to minimize 
#'the sum of residual least squares between modeled and observed spectra via 
#'the Levenberg-Marquardt algorithm (`nls.lm` function from the `minpack.lm` 
#'package).
#'@param observed Vector of observations (e.g. a reflectance spectrum).
#'@param inits Vector of initial conditions for the parameters.
#'@param model An R function that calls the RTM and returns the error to be 
#'minimized. Be sure to include constants here.
#'@param lower Lower bounds on parameters (default=NULL, which means -Inf).
#'@param uppper Upper bounds on parameters (default=NULL, which means +Inf).

invert.lsq <- function(observed, inits, model, lower=NULL, upper=NULL){
    observed <- as.matrix(observed)
    merit <- function(params){
        spec <- model(params)
        error <- spec - observed
        return(error)
    }
    fit <- nls.lm(par=inits, lower=lower, upper=upper, fn=merit)
    return(fit)
}
