### Simplified R inversion
library(Rcpp)
load("Rcpp/cp4.Rdata")
wl <- 2101
load("nimble/pinv_nimble_testdat.Rdata")
rm(dat.p)
sourceCpp("Rcpp/prospect.cpp")

pinvbayes <- function(ngibbs, Observed){
    ## Initial conditions
    N = 1.4
    Cab = 30
    Cw = 0.01
    Cm = 0.01
    rsd = 0.5

    logN = log(N-1)
    logCab = log(Cab)
    logCw = log(Cw)
    logCm = log(Cm)

    JRSD = 0.5
    JumpN = JRSD
    JumpCab = JRSD
    JumpCw = JRSD
    JumpCm = JRSD

    nspec = ncol(Observed)

    # Priors
    priorN = function(logN) dnorm(logN, -0.916, 2.2)
    priorCab = function(logCab) dnorm(logCab, 3.4, 0.9)
    priorCw = function(logCw) dnorm(logCw, -6.377, 0.5)
    priorCm = function(logCm) dnorm(logCm, -5.116, 0.9)

    # Shortcut functions
    SpecError = function(Model, Observed) Model - Observed
    cppFunction("double Likelihood(NumericMatrix Error, double rsd){
            NumericVector LL;
            LL = dnorm(Error, 0, rsd, true);
            return sum(LL);
    }")
        #function(Error, rsd) sum(dnorm(Error, 0, rsd, TRUE))
    rp1 = 0.001 + nspec*wl/2

    # Precalculate first model
    PrevSpec = prospect4(N, Cab, Cw, Cm)
    PrevError = SpecError(PrevSpec, Observed)

    # MCMC loop
    Results = matrix(NA, nrow = ngibbs, ncol = 5)
    for(ng in 1:ngibbs){
        cat("+")

        # Sample N
        TlogN = rnorm(1, logN, JumpN)
        TN = exp(TlogN) + 1
        TrySpec = prospect4(TN, Cab, Cw, Cm)
        TryError = SpecError(TrySpec, Observed)
        TryPost = Likelihood(TryError, rsd) + priorN(TlogN)
        PrevPost = Likelihood(PrevError, rsd) + priorN(logN)
        a = exp(TryPost - PrevPost)
        if(a > runif(1)){
            logN = TlogN
            N = TN
            PrevError = TryError
        }

        # Sample rsd
        rp2 = 0.001 + sum(PrevError*PrevError)/2
        rinv = rgamma(1, rp1, rp2)
        rsd = 1/sqrt(rinv)

        Results[ng, ] = c(N, Cab, Cw, Cm, rsd)
    }
    return(Results)
}

Rprof(memory.profiling = TRUE)
z <- pinvbayes(1000, obs.spec)
Rprof(NULL)
print(summaryRprof())
