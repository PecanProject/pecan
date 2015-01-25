##' @name p.invert
##' @title Prospect Model MLE Inversion
##' @details {
##' Maximum likelihood inversion of PROSPECT model. Used for generating initial conditions in Bayesian inversion.
##' }
##' @param observed Matrix of observed reflectance values. Each row is one wavelength, each column is one spectrum. 
##' @param func PROSPECT model used for inversion (default is 'prospect4', no others work as of now)
##' @param inits Vector length 4 (N, Cab, Cw, Cm) of initial conditions for optimization procedure (default is 'guess.inits' vector).
##' @return Vector of parameter values (N, Cab, Cw, Cm) from optimization procedure.
##' @export
##' @author Alexey Shiklomanov

setwd("R/")
source("prospect.R")

p.invert <- function(observed, func=prospect4, inits=unlist(guess.inits)[1:4])
{
        # Merit function based on sum-of-squares difference
        ssd <- function(P, ref.obs){
                P <- as.list(P)
                prospect <- func(P$N, P$Cab, P$Cw, P$Cm, n.a, cab.a, w.a, m.a)
                ssd <- log(sum((prospect - ref.obs)^2))
                return(ssd)
        }
        observed <- rowMeans(observed)
        a1 <- optim(inits, ssd, ref.obs=observed)
        return(a1$par)
}

