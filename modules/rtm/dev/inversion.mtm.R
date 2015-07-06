## Mutliple try Metropolis
library(mvtnorm)
source("testinv.setup.R")

weight <- function(tm, inits, nsamp, Jump,
                   model, constants, 
                   prior, pm) {
    w <- numeric(nsamp)
    posvec <- !apply(tm, 1, function(x) any(x < pm))
    if(sum(posvec) < 2) return(numeric(nsamp))
    w[!posvec] <- 0
    tmp <- tm[posvec,]
    TrySpec <- apply(tmp, 1, model, constants)
    TryError <- apply(TrySpec, 2, "-", observed)
    ptry <- apply(tmp, 1, function(x)
                  mapply(do.call, prior, lapply(x, list)))
    pinit <- mapply(do.call, prior, lapply(inits, list))
    TryLike <- apply(TryError, 2, function(x) sum(dnorm(x, 0, rsd, 1)))
    TryPost <- TryLike + colSums(ptry)
    w[posvec] <- exp(dmvnorm(tmp, inits, Jump, 1) + TryPost)
    w[is.na(w)] <- 0
    return(w)
}

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
    Jump.init <- cov(cv[,-5])
    Jump <- Jump.init
    results <- matrix(NA, nrow=ngibbs, ncol=npars+1)
    adapt <- 50
    pcov <- 0.05
    results[1,] <- c(inits, rsd)
    accept <- 0
    reject <- 0
    adapt.ar <- 30
    adapt.cov <- 50
    adapt.min <- 0.2
    nsamp <- 15
    t1 <- proc.time()
## Block sampler
    for(ng in 2:ngibbs){
        cat(ng, " ")
        tm.y <- rmvnorm(nsamp, inits, Jump)
        w.y <- weight(tm.y, inits, nsamp, Jump, 
                      model, constants, prior, pm)
        if(!any(w.y > 0)){
            a <- 0
        } else {
            y.samp <- tm.y[sample(1:nrow(tm.y), 1, prob = w.y),]
            tm.x <- rmvnorm(nsamp, y.samp, Jump)
            w.x <- weight(tm.x, y.samp, nsamp, Jump,
                        model, constants, prior, pm)
            a <- sum(w.y)/sum(w.x)
        }
        if(is.na(a)) a <- -1
        if(a > runif(1)){
            inits <- y.samp
        }
        rp2 <- 0.001 + sum(PrevError * PrevError)/2
        rinv <- rgamma(1, rp1, rp2)
        rsd <- 1/sqrt(rinv)
        results[ng,] <- c(inits, rsd)
        if(ng > adapt.cov) {
            jsamp <- seq(1, ng)
            Scov <- cov(results[jsamp, 1:npars])
            Jump <- pcov * Jump.init + (1-pcov) * Scov
#           adapt <- accept / (accept + reject) / 0.75
#           Jump <- Jump * adapt
        }
    }
    print(" ")
    t2 <- proc.time()
    print(t2 - t1)
    par(mfrow=c(2,2))
    plot(results[,1], type='l')
    plot(results[,2], type='l')
    plot(results[,3], type='l')
    plot(results[,4], type='l')

