library(PEcAnRTM)
m1 <- function(params, constants){
    inpars <- params
    r <- prospect(params, 4)
    return(r[,1])
}

inits <- c(1.1, 5, 0.01, 0.01, 0.5)
constants <- 0
observed <- prospect(defparam("prospect_4"), 4)[,1]

z <- invert.mle(observed, inits, constants, m1)
zp <- z$par[1:4]
zprosp <- prospect(zp, 4)[,1]
plot(observed, type='l')
lines(zprosp, col=2)

