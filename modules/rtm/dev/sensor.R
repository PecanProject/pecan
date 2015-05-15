### Table of AVIRIS bands
#   Spectrometer    Wavelength  nBands  Bandwidth
#   1               410 - 700   31      9.4
#   2               680 - 1270  63      9.4
#   3               1250 - 1860 63      9.7
#   4               1840 - 2450 63      9.7

#' Simplified AVIRIS reflectance function (every 10 nm)
aviris.refl <- function(obs){
    ## These are just 10nm means, not exactly AVIRIS, but close
    smod <- colMeans(matrix(obs[-2101], nrow=10))
    return(smod)
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

