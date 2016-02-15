#' @name prospect
#' @title PROSPECT (4, 5, or 5B) model
#' @author Alexey Shiklomanov
#' @details R wrapper for PROSPECT models
#' @param param Vector of PROSPECT parameter values:
#'     N: Effective number of leaf layers (>1)
#'     Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'     (5) Car: Leaf carotenoid content (ug/cm2) (>0)
#'     (5B) Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'     Cw: Leaf water content (cm) (>0)
#'     Cm: Leaf dry matter content (ug/cm2) (>0)
#' @param version PROSPECT version: 4, 5, or "5B"
#' @param include.wl Whether or not to append wavelengths to output matrix.  
#' Included to provide some backward compatibility and benchmarking, but will 
#' soon be deprecated.
#' @return Matrix (2101 x 3) of simulated reflectance (column 1, "R"), 
#'      transmittance (column 2, "T"), and wavelength (column 3, "wl") values 
#'      from 400:2500 nm

prospect <- function(param, version, include.wl = FALSE){
    version <- toupper(as.character(version))
    plist <- as.list(param)
    plist$RT <- matrix(0, 2101, 2)
    if (version == "4"){
        if(length(plist) != 5) stop("Wrong number of parameters")
        inlist <- c("prospect_4", plist)
    }
    else if (version == "5") {
        if(length(plist) != 6) stop("Wrong number of parameters")
        inlist <- c("prospect_5", plist)
    }
    else if (version == "5B") {
        if(length(plist) != 7) stop("Wrong number of parameters")
        inlist <- c("prospect_5b", plist)
    }
    else stop("Version must be 4, 5, or 5B") 
    outlist <- do.call(.Fortran, inlist)
    if(include.wl){
        RTL <- cbind(outlist[[length(outlist)]], 400:2500)
        names(RTL) <- c("R", "T", "wl")
        return(RTL)
    } else {
        return(outlist[[length(outlist)]])
    }
}

# Shortcut lists for PROSPECT parameter names
params.prospect4 <- c("N", "Cab", "Cw", "Cm")
params.prospect5 <- c("N", "Cab", "Car", "Cw", "Cm")
params.prospect5b <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")

# Default settings for PROSPECT inversion
default.settings.prospect <- list(
    model = function(params) prospect(params, 5)[,1],
    inits.function = function() 
        with(prior.defaultvals.prospect(sd.inflate=3), 
             rlnorm(5, mu, sigma) + c("N"=1,"Cab"=0,"Car"=0,"Cw"=0,"Cm"=0)),
    prior.function = with(prior.defaultvals.prospect(sd.inflate=3), priorfunc.prospect(mu,sigma)),
    param.mins = c(1, 0, 0, 0, 0),
    ngibbs = 100000,
    nchains = 5,
    burnin = 80000,
    n.tries = 5,
    return.samples = TRUE,
    target = 0.234,
    target.adj = 0.8,
    do.lsq.first = FALSE,
    do.lsq.after = 3,
    save.samples = NULL,
    quiet = FALSE,
    adapt = 100,
    adj_min=0.1)
