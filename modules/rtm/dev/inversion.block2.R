library(mvtnorm)
source("testinv.setup.R")

observed <- obs
inits <- initial()
constants <- cons
prior <- inv.priors
pm <- inv.pmin
model <- ps.aviris
ngibbs <- 5000
# invert.block <- function(observed, inits, constants, ngibbs, prior, pm,
#                         model){
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    nwl <- nrow(observed)
    npars <- length(inits)
    rp1 <- 0.001 + nspec*nwl/2
    rsd <- 0.5
    PrevSpec <- model(inits, constants)
    PrevError <- PrevSpec - observed
    Jump.init <- diag(inits * 0.05)
    Jump <- Jump.init
    results <- matrix(NA, nrow=ngibbs, ncol=npars+1)
    adapt <- 50
    pcov <- 0.05
    results[1,] <- c(inits, rsd)
    ng <- 2
    ngreedy <- 1000

## Greedy sampler -- for the first 300 values, only use accepted values
    while(ng < ngreedy){
        tm <- rmvnorm(1, inits, Jump)
        tvec <- as.numeric(tm)
        names(tvec) <- colnames(tm)
        if(any(tvec < pm)) next
        TrySpec <- model(tvec, constants)
        TryError <- TrySpec - observed
        ptry <- mapply(do.call, prior, lapply(tvec, list))
        pinit <- mapply(do.call, prior, lapply(inits, list))
        TryPost <- sum(dnorm(TryError,0,rsd,1)) + sum(ptry)
        PrevPost <- sum(dnorm(PrevError,0,rsd,1)) + sum(pinit)
        JN <- dmvnorm(tvec, inits, Jump, 1)
        JD <- dmvnorm(inits, tvec, Jump, 1)
        a <- exp((TryPost - JN) - (PrevPost - JD))
        if(is.na(a)) a <- -1
        if(a > runif(1)){
            inits <- tvec
            PrevError <- TryError
            rp2 <- 0.001 + sum(PrevError * PrevError)/2
            rinv <- rgamma(1, rp1, rp2)
            rsd <- 1/sqrt(rinv)
            results[ng,npars+1] <- rsd
            results[ng,1:npars] <- inits
            nsamp = seq(max(ng-100, 1), ng)
            Scov <- cov(results[nsamp, 1:npars])
            Jump <- pcov * Jump.init + (1-pcov) * Scov
            ng <- ng + 1
            cat(ng, " ")
        } else Jump <- Jump / 2
    }

