#' @name invert.slow
#' @title Bayesian inversion of a model
#' @details Performs an inversion of an arbitrary model using a modified 
#' Metropolis Hastings algorithm with block sampling. This may be slightly 
#' slower than the implementation in Fortran, but is much more customizable, as 
#' the model can be any R function.
#' @param observed Vector, matrix, or data frame (coerced to matrix) of 
#' observed values. For spectral data, wavelengths are rows and spectra are 
#' columns.
#' @param inits Vector of initial values of model parameters to be inverted.
#' @param constants Vector of model constants.
#' @param ngibbs Number of MCMC iterations
#' @param prior Function for use as prior. Should take a vector of parameters 
#' as input and return a single value -- the sum of their log-densities -- as 
#' output.
#' @param pm Vector of minimum values for inversion parameters
#' @param model The model to be inverted. This should be an R function that 
#' takes `inits` and `constants` as input and returns one column of `observed` 
#' (nrows should be the same).
#' @param adapt Number of steps for adapting covariance matrix (i.e. adapt 
#' every 'n' steps. Default=100
#' @param adj_min Minimum threshold for rescaling Jump standard deviation.  
#' Default = 0.1.
#' @param target Target acceptance rate. Default=0.44
#' @param lsq.first Perform least squares optimization first, and use outputs 
#' to initialize Metropolis Hastings. This dramatically improves the mixing 
#' time. Default=TRUE
#' @param quiet Don't print steps and status messages. Default=FALSE
invert.slow <- function(observed, inits, constants, ngibbs, prior, pm, model, 
                        adapt=100, adj_min=0.1, target=0.44, do.mle=TRUE, quiet=FALSE){
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    nwl <- nrow(observed)
    npars <- length(inits)
    if(do.mle){
        fit <- invert.lsq(observed, inits, constants, model, lower=pm)
        if(!quiet) print(fit)
        inits <- fit$par
    }
    rp1 <- 0.001 + nspec*nwl/2
    rsd <- 0.5
    PrevSpec <- model(inits, constants)
    PrevError <- PrevSpec - observed
    initsd <- inits * 0.05
    Jump <- diag(initsd)
    results <- matrix(NA, nrow=ngibbs, ncol=npars+1)
    if(!is.null(names(inits))) cnames <- names(inits)
    else cnames <- sprintf("par%d", 1:length(inits))
    colnames(results) <- c(cnames, "residual")
    ar <- 0
    for(ng in 1:ngibbs){
        if(ng %% adapt < 1){
            if(!quiet) print(ng)
            if(ar < 2){
                rescale <- diag(rep(adj_min,npars))
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
            TryPost <- sum(dnorm(TryError,0,rsd,1)) + prior(tvec)
            PrevPost <- sum(dnorm(PrevError,0,rsd,1)) + prior(inits)
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

