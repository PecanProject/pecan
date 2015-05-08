#' Truncated normal distribution functions
#' 
#' @name rtnorm
#' @title Random sampling from one-sided truncated normal distribution
#' @author Alexey Shiklomanov
#' @details Draws a random number and, if it doesn't fall within the 
#'      specified range, resample using an adjusted Normal CDF. This isn't
#'      performed immediately because CDF sampling calls three functions --
#'      qnorm, runif, and pnorm--and therefore is much less efficient than a
#'      simple random sample.
#' @param mu The mean parameter of the truncated normal. NOTE that this is NOT
#'      equal to the mean of the distribution
#' @param sd The standard deviation parameter of the truncated normal. Again, NOTE
#'      tht this is not the SD of the distribution.
#' @param MIN The minimum value, which defines the truncation.
#' @return Numeric length one.
rtnorm <- function(mu, sd, MIN){
    x <- rnorm(1, mu, sd)
    if(x < MIN)
        x <- qnorm(runif(1, pnorm(MIN, mu, sd, 1, 0), 1), mu, sd, 1, 0)
    return(x)
}

#' @name dtnorm
#' @title Truncated normal distribution density
#' @author Alexey Shiklomanov
#' @details Calculates the log density of a univariate truncated normal variable
#' @param x A random variable
#' @param mu The mean parameter of the distribution; NOTE this is not equal to the mean
#' @param sd The standard deviation parameter of the distribution
#' @param MIN Value at which the truncation takes place
#' @return Numeric; log density of the distribution, or -1e15 if the x < MIN
dtnorm <- function(x, mu, sd, MIN){
    if(x < MIN)
        return(-1e15)
    else
        return(dnorm(x, mu, sd, 1) - log(1-pnorm(MIN, mu, sd, 1, 0)))
}

ps.aviris <- function(params, constants)
    aviris.refl(pro4sail(params, constants))

data(aviris.wl)
av.wl <- round(aviris.wl[aviris.wl >= 400]) - 399

aviris.sail <- function(params, constants){
    r <- pro4sail(params, constants)
    refl <- r[av.wl]
    return(refl)
}

invert.sail <- function(observed, inits, constants, ngibbs, prior, pm,
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

            
## Default SAIL parameters
prosp.def <- c("N" = 1.5, 
               "Cab" = 40, 
               "Car" = 8, 
               "Cbrown" = 0, 
               "Cw" = 0.01, 
               "Cm" = 0.009)
sail.def = c(prosp.def,
             "LIDFa" = -0.35, 
             "LIDFb" = -0.15,
             "LIDFtype" = as.integer(1), 
             "LAI" = 3,
             "q" = 0.01,
             "tts" = 30,
             "tto" = 10,
             "psi" = 0,
             "psoil" = 1)

## Prepare sail constants based on default values
sail.constants <- function(params){
    par.ind <- names(sail.def) %in% names(params)
    constants <- sail.def[!par.ind]
    return(constants)
}

## Uninformative priors (lognormal)
sail.priors <- function(n, std=100){
    prior <- list()
    for(i in 1:n) prior[[i]] <- function(x) dnorm(log(x), 0, std, 1)
    return(prior)
}

