## Simple wrapper for PROSAIL
path.lib <- "~/Documents/Unsynced/pecan/modules/rtm/src/RTM/prosail.so"
dyn.load(path.lib)

## Truncated normal distribution functions
rtnorm <- function(mu, sd, MIN){
    x <- rnorm(1, mu, sd)
    if(x < MIN)
        x <- qnorm(runif(1, pnorm(MIN, mu, sd, 1, 0), 1), mu, sd, 1, 0)
    return(x)
}
dtnorm <- function(x, mu, sd, MIN){
    if(x < MIN)
        return(-1e15)
    else
        return(dnorm(x, mu, sd, 1) - log(1-pnorm(MIN, mu, sd, 1, 0)))
}

## Convert model reflectance to sensor
aviris.refl <- function(obs){
    ### Table of AVIRIS bands
    #   Spectrometer    Wavelength  nBands  Bandwidth
    #   1               410 - 700   31      9.4
    #   2               680 - 1270  63      9.4
    #   3               1250 - 1860 63      9.7
    #   4               1840 - 2450 63      9.7
    ## These are just 10nm means, not exactly AVIRIS, but close
    smod <- colMeans(matrix(obs[-2101], nrow=10))
    return(smod)
}

## Call PRO4SAIL model
pro4sail <- function(params, constants){
    param.order <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm",
                     "LIDFa", "LIDFb", "LIDFtype", "LAI", "q",
                     "tts", "tto", "psi", "psoil")
    pass.in <- c(as.list(params), as.list(constants))
    pass.in <- pass.in[param.order]
    r <- numeric(2101)
    p <- c(list("PRO4SAIL"), pass.in, rep(list(r),4))
    lp <- length(p)-1
    f <- do.call(.Fortran, p)
    #     out <- do.call(cbind, f[(lp-3):lp])
    out <- f[[(lp-3)]]
    return(out)
}

ps.aviris <- function(params, constants)
    aviris.refl(pro4sail(params, constants))

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

