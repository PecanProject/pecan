### Simplified R inversion
library(Rcpp)
load("Rcpp/cp4.Rdata")
wl <- 2101
load("nimble/pinv_nimble_testdat.Rdata")
rm(dat.p)
sourceCpp("Rcpp/prospect.cpp")

Jmin <- 0.5

pinvbayes <- function(ngibbs, Observed, adapt=50){
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

    Jump <- abs(0.05*c(logN, logCab, logCw, logCm))
    nspec = ncol(Observed)

    # Priors
    priorN = function(logN) dnorm(logN, -0.916, 2.2, 1)
    priorCab = function(logCab) dnorm(logCab, 3.4, 0.9, 1)
    priorCw = function(logCw) dnorm(logCw, -6.377, 0.5, 1)
    priorCm = function(logCm) dnorm(logCm, -5.116, 0.9, 1)

    # Shortcut functions
    SpecError = function(Model, Observed) Model - Observed

	cppFunction("double Likelihood(NumericMatrix Error, double rsd){
            NumericVector LL;
            LL = dnorm(Error, 0, rsd, true);
            return sum(LL);
    }")

	cppFunction("double p2(NumericMatrix PrevError){
    			double rp2;
				rp2 = 0.001 + sum(PrevError*PrevError)/2;
    			return rp2;
    			}")

    rp1 = 0.001 + nspec*wl/2

    # Precalculate first model
    PrevSpec = prospect4(N, Cab, Cw, Cm)
    PrevError = SpecError(PrevSpec, Observed)

    # MCMC loop
    Results = matrix(NA, nrow = ngibbs, ncol = 5)
    ar <- rep(0, 4)
    for(ng in 1:ngibbs){
        ## Adapt
        if(ng %% adapt == 0){
            print(ng)
            print(ar)
#             adj <- ar / adapt / 0.75
#             adj[adj < 1e-10] <- Jmin
#             Jump <- Jump * adj
            ar <- rep(0, 4)
#             print(Jump)
        }

        # Sample N
        TlogN = rnorm(1, logN, Jump[1])
        TN = exp(TlogN) + 1
        TrySpec = prospect4(TN, Cab, Cw, Cm)
        TryError = SpecError(TrySpec, Observed)
        TryPost = Likelihood(TryError, rsd) + priorN(TlogN)
        PrevPost = Likelihood(PrevError, rsd) + priorN(logN)
        a = exp(TryPost - PrevPost)
        if(is.finite(a) & a > runif(1)){
            logN = TlogN
            N = TN
            PrevError = TryError
            ar[1] <- ar[1] + 1
        }

        # Sample Cab
        TlogCab = rnorm(1, logCab, Jump[2])
        TCab = exp(TlogCab)
        TrySpec = prospect4(N, TCab, Cw, Cm)
        TryError = SpecError(TrySpec, Observed)
        TryPost = Likelihood(TryError, rsd) + priorCab(TlogCab)
        PrevPost = Likelihood(PrevError, rsd) + priorCab(logCab)
        a = exp(TryPost - PrevPost)
        if(is.finite(a) & a > runif(1)){
        	logCab = TlogCab
        	Cab = TCab
        	PrevError = TryError
            ar[2] <- ar[2] + 1
        }

        # Sample Cw
        TlogCw = rnorm(1, logCw, Jump[3])
        TCw = exp(TlogCw)
        TrySpec = prospect4(N, Cab, TCw, Cm)
        TryError = SpecError(TrySpec, Observed)
        TryPost = Likelihood(TryError, rsd) + priorCw(TlogCw)
        PrevPost = Likelihood(PrevError, rsd) + priorCw(logCw)
        a = exp(TryPost - PrevPost)
        if(is.finite(a) & a > runif(1)){
            logCw = TlogCw
            Cw = TCw
            PrevError = TryError
            ar[3] <- ar[3] + 1
        }

        # Sample Cm
        TlogCm = rnorm(1, logCm, Jump[4])
        TCm = exp(TlogCm)
        TrySpec = prospect4(N, Cab, Cw, TCm)
        TryError = SpecError(TrySpec, Observed)
        TryPost = Likelihood(TryError, rsd) + priorCm(TlogCm)
        PrevPost = Likelihood(PrevError, rsd) + priorCm(logCm)
        a = exp(TryPost - PrevPost)
        if(is.finite(a) & a > runif(1)){
            logCm = TlogCm
            Cm = TCm
            PrevError = TryError
            ar[4] <- ar[4] + 1
        }

        # Sample rsd
		rp2 <- p2(PrevError)
        rsd = 1/sqrt(rgamma(1, shape = rp1, scale = rp2))
        print(rsd)

        Results[ng, ] = c(N, Cab, Cw, Cm, rsd)
    }
    return(Results)
}

Rprof(memory.profiling = TRUE, line.profiling = TRUE)
z <- pinvbayes(1000, obs.spec, 50)
Rprof(NULL)
print(summaryRprof())

par(mfrow=c(2,2))
plot(z[,1], type='l')
plot(z[,2], type='l')
plot(z[,3], type='l')
plot(z[,4], type='l')
par(mfrow = c(1,1))
