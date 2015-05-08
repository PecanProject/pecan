#' Simple wrapper for PROSAIL

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

data(aviris.wl)
av.wl <- round(aviris.wl[aviris.wl >= 400]) - 399

aviris.sail <- function(params, constants){
    r <- pro4sail(params, constants)
    refl <- r[av.wl]
    return(refl)
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

