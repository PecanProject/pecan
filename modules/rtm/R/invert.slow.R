#' @name invert.slow
#' @title Bayesian inversion of a model
#' @details Performs a Bayesian inversion of an arbitrary function
#'      using the Metropolis-Hastings algorithm.
#'      This is substantially (~80x) slower than 
#'      `invert.fast` because it is implemented as a basic R loop,
#'      but it is also more versatile and is intended for use with
#'      custom models.
#' @param observed Vector, matrix, or data frame (coerced to matrix) of
#'      observed values. For spectral data, wavelengths are rows and 
#'      spectra are columns.
#' @param inits Vector of initial values of model parameters to be
#'      inverted.
#' @param constants Vector of model constants.
#' @param ngibbs Number of MCMC iterations
#' @param prior List of functions used as priors (e.g. dnorm(x, 3, 0.5, 1)).
#'      NOTE: These should return the LOG density of the value
#'      (i.e. log=TRUE should be set)
#' @param pm Vector of minimum values for inversion parameters
#' @param model The model to be inverted. This should be an R function 
#'      that takes `inits` and `constants` as input and returns 
#'      one column of `observed` (nrows should be the same).
invert.slow <- function(observed, inits, constants, ngibbs, prior, pm,
                        model){
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    nwl <- nrow(observed)
    npars <- length(inits)
    rp1 <- 0.001 + nspec*nwl/2
    rsd <- 0.5
    PrevSpec <- model(inits, constants)
    PrevError <- PrevSpec - observed
    Jump <- inits * 0.05
    results <- matrix(NA, nrow=ngibbs, ncol=npars+1)
    ar <- numeric(npars)
    adapt <- 20
    adj_min <- 0.1
    for(ng in 1:ngibbs){
        if(ng %% adapt < 1){
            adj <- ar / adapt / 0.75
            adj[adj < adj_min] <- adj_min
            Jump <- Jump * adj
            ar <- numeric(npars)
        }
        for(p in 1:npars){
            tvec <- inits
            tvec[p] <- rtnorm(inits[p],Jump[p],pm[p])
            TrySpec <- model(tvec, constants)
            TryError <- TrySpec - observed
            TryPost <- sum(dnorm(TryError,0,rsd,1)) + prior[[p]](tvec[p])
            PrevPost <- sum(dnorm(PrevError,0,rsd,1)) + prior[[p]](inits[p])
            JN <- dtnorm(tvec[p], inits[p], Jump[p], pm[p])
            JD <- dtnorm(inits[p], tvec[p], Jump[p], pm[p])
            a <- exp((TryPost - JN) - (PrevPost - JD))
            if(is.na(a)) a <- -1
            if(a > runif(1)){
                inits[p] <- tvec[p]
                PrevError <- TryError
                ar[p] <- ar[p] + 1
            }
            results[ng,p] <- inits[p]
        }
        rp2 <- 0.001 + sum(PrevError * PrevError)/2
        rinv <- rgamma(1, rp1, rp2)
        rsd <- 1/sqrt(rinv)
        results[ng,npars+1] <- rsd
    }
    return(results)
}

