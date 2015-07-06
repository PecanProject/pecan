library(mvtnorm)
source("testinv.setup.R")

observed <- obs
inits <- initial()
constants <- cons
prior <- inv.priors
pm <- inv.pmin
model <- ps.aviris
ngibbs <- 1000
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
    accept <- 0

    for(ng in 1:ngibbs){
        cat(sprintf("%s ", ng))
        if((ng > 1) && (ng %% adapt < 1)){
            adj <- max(accept / adapt / 0.75, 0.25)
            Scov <- cov(results[1:ng-1, 1:npars])
            Jump <- pcov * Jump.init + (1-pcov) * Scov*adj
            accept <- 0
        }
        tm <- rmvnorm(1, inits, Jump)
        tvec <- as.numeric(tm)
        names(tvec) <- colnames(tm)
        if(any(tvec < pm)){
            results[ng,1:npars] <- inits
            ng <- ng + 1
            next
        }
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
            accept <- accept + 1
        }
        results[ng,1:npars] <- inits
        rp2 <- 0.001 + sum(PrevError * PrevError)/2
        rinv <- rgamma(1, rp1, rp2)
        rsd <- 1/sqrt(rinv)
        results[ng,npars+1] <- rsd
        ng <- ng + 1
    }

#    return(results)
#}

# Perform inverison
#ng <- 500
#inv.av1 <- invert.block(obs, initial(), cons, ng,
#                        inv.priors, inv.pmin, ps.aviris)
