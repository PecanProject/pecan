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
#' @param adapt Number of steps for adapting covariance matrix (i.e. adapt 
#' every 'n' steps. Default=100
#' @param adj_min Minimum threshold for rescaling Jump standard deviation.  
#' Default = 0.1
#' @param target Target acceptance rate. Default=0.44
#' @param lsq.first Perform least squares optimization first, and use outputs 
#' to initialize Metropolis Hastings. This dramatically improves the mixing 
#' time, but may prevent full exploration of the parameter space and may result 
#' in getting caught in a local minimum. Default=TRUE
invert.slow <- function(observed, inits, constants, ngibbs, prior, pm,
                        model, adapt=100, adj_min=0.1, target=0.44, do.mle=TRUE){
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    nwl <- nrow(observed)
    npars <- length(inits)
    if(do.mle){
        fit <- invert.lsq(observed, inits, constants, model, lower=pm)$par
        print(fit)
        inits <- fit$par
    }
    rp1 <- 0.001 + nspec*nwl/2
    rsd <- 0.5
    PrevSpec <- model(inits, constants)
    PrevError <- PrevSpec - observed
    initsd <- inits * 0.05
    Jump <- diag(initsd)
    results <- matrix(NA, nrow=ngibbs, ncol=npars+1)
    ar <- 0
    for(ng in 1:ngibbs){
        if(ng %% adapt < 1){
            print(ng)
            if(ar < 2){
                rescale <- diag(rep(adj_min,4))
                Jump <- rescale %*% Jump %*% rescale
            } else{
                adj <- max(ar / adapt / target, adj_min)
                region <- seq(ng-adapt, ng-1)
                stdev <- apply(results[region,1:npars], 2, sd)
                rescale <- diag(stdev * adj)
                cormat <- cor(results[region,1:npars])
                if(any(is.na(cormat))) cormat <- diag(rep(1,4))
                Jump <- rescale %*% cormat %*% rescale
            }
            ar <- 0
        }
        tvec <- mvrnorm(1, inits, Jump)
        if(all(tvec > pm)){
            TrySpec <- model(tvec, constants)
            TryError <- TrySpec - observed
            TryPrior <- sum(sapply(1:npars, function(p) prior[[p]](tvec[p])))
            TryPost <- sum(dnorm(TryError,0,rsd,1)) + TryPrior
            PrevPrior <- sum(sapply(1:npars, function(p) prior[[p]](inits[p])))
            PrevPost <- sum(dnorm(PrevError,0,rsd,1)) + PrevPrior
            a <- exp(TryPost - PrevPost)
            if(is.na(a)) a <- -1
            if(a > runif(1)){
                inits <- tvec
                PrevError <- TryError
                ar <- ar + 1
            }
        }
        results[ng,1:npars] <- inits
        rp2 <- 0.001 + sum(PrevError * PrevError)/2
        rinv <- rgamma(1, rp1, rp2)
        rsd <- 1/sqrt(rinv)
        results[ng,npars+1] <- rsd
    }
    return(results)
}

