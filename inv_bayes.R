#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.R")
source("truncnorm.R")

guess.inits <- c(N=1.4, 
              Cab=30,
              Cw=0.017,
              Cm=0.006
              )

source("mle_inversion.R")

## NOTE: obs.spec must be a matrix as follows:
## Column 1 : Wavelengths (400:2500)
## Columns 2-n : Reflectance observations
## Use specdatproc script to generate correct matrices from data.
pinvbayes <- function(obs.spec,
                      ngibbs=100,
                      JumpRSD=0.1,
                      local.store=FALSE,
                      single.precision=TRUE,
                      random.effects='none',
                      inits='mle',
                      ar.step=100,
                      ar.min=0.1,
                      ar.max=0.9,
                      ar.target=0.75,
                      fname = "runs/test_run.dat"
                      )
{
        wl <- min(obs.spec[,1]):max(obs.spec[,1])
        nwl <- length(wl)
        nspec <- ncol(obs.spec)
        JumpSD <- JumpRSD * unlist(guess.inits)


        ### Priors ###
        N.prior <- function(N) dnorm(N - 1, 0, 1.5, log=TRUE) + log(2)
        Cab.prior <- function(Cab) dlnorm(Cab, log(30), 0.9, log=TRUE)
        Cw.prior <- function(Cw) dlnorm(Cw, log(0.017), 0.5, log=TRUE)
        Cm.prior <- function(Cm) dlnorm(Cm, log(0.006), 0.9, log=TRUE)

        # Error
        pwl.p <- c(0.001, 0.001)          # Inverse gamma


        ### Initial conditions ###

        if(inits == "guess"){
                ic <- guess.inits
        } else if(inits == "mle"){
                ic <- p.invert(obs.spec)
        } else {
                ic <- c(abs(rnorm(1, 0, 1.5)) + 1,
                        rlnorm(1, log(30), 0.9),
                        rlnorm(1, log(0.017), 0.5),
                        rlnorm(1, log(0.006), 0.9)
                        )
        }
        N.i <- ic[1]
        Cab.i <- ic[2]
        Cw.i <- ic[3]
        Cm.i <- ic[4]
        print(sprintf("Initial conditions: N %g, Cab %g, Cw %g, Cm %g",
                      N.i, Cab.i, Cw.i, Cm.i))
        sd.i <- rep(1, 2101)
        if(single.precision) sd.i <- sd.i[1]

        ### Extract indices for random effects ###
        if(random.effects != 'none'){
                regxp.list <- c(leaf = "(^.*)_[0-9]{5}.csv",
                                plot = "^[0-9]{4}[A-Za-z]+[0-9]{2}__[A-Za-z]+_([A-Za-z0-9]+)_.*")
                randeff.regxp <- regxp.list[random.effects]
                randeffs <- unique(gsub(randeff.regxp, "\\1", colnames(obs.spec)))
                if(length(randeffs) == 1) {
                        print("Random effect feature of size 1, so not estimated.")
                        random.effects <- "none"
                } else {
                        randeff.list <- lapply(randeffs, grep, colnames(obs.spec))
                        print(randeff.list)
                        nre <- length(randeff.list)
                        
                        ### Random effects initial conditions
                        alphaN.i <- rep(0, nre)
                        alphaCab.i <- rep(0, nre)
                        alphaCw.i <- rep(0, nre)
                        alphaCm.i <- rep(0, nre)
                        
                        sdreN <- 1
                        sdreCab <- 1
                        sdreCw <- 1
                        sdreCm <- 1
                        
                        # Random effects Prior
                        randeff.s <- c(0.001, 0.001)              # Inverse gamma
                }
        }
        
        ### Shortcut functions ###
        prospect <- function(N, Cab, Cw, Cm) prospect4(N, Cab, Cw, Cm, n.a, cab.a, w.a, m.a)

        spec.error <- function(mod.spec, obs.spec){
                if(length(dim(obs.spec))){
                        return(-apply(obs.spec, 2, "-", mod.spec))
                } else {
                        return(mod.spec - obs.spec)
                }
        }

        likelihood <- function(guess.error, sd.i) sum(dnorm(guess.error, 0, sd.i, log=TRUE))

        # Precalculate first model
        prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
        prev.error <- spec.error(prev.spec, obs.spec)


        ### MCMC storage
        if (local.store){
                N.store <- numeric(ngibbs)
                Cab.store <- numeric(ngibbs)
                Cw.store <- numeric(ngibbs)
                Cm.store <- numeric(ngibbs)
                if(single.precision){
                        sd.store <- numeric(ngibbs)
                } else {
                        sd.store <- matrix(NA, nrow=ngibbs, ncol=nwl)
                }
                if(random.effects != 'none'){
                        sdplotN.store <- numeric(ngibbs)
                        sdplotCab.store <- numeric(ngibbs)
                        sdplotCw.store <- numeric(ngibbs)
                        sdplotCm.store <- numeric(ngibbs)
                        alphaN.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCab.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCw.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCm.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                }
                
        } else {
                if(single.precision) {
                        sdvec <- "sd"
                } else {
                        sdvec <- paste("sd", wl, sep='')
                }
                if(random.effects != "none"){
                        header <- c("N", "Cab", "Cw", "Cm", 
                                    "sdreN", "sdreCab", "sdreCw", "sdreCm",
                                    sdvec)
                } else {
                        header <- c("N", "Cab", "Cw", "Cm", 
                                    sdvec)
                }
                write(header,
                      ncolumns=length(header),
                      file=fname, 
                      sep=",", 
                      append=FALSE)
        }

        ## MCMC loop
        tstart <- proc.time()
        ar <- 0
        ar.alpha <- 0
        arp <- 0
        for(g in 1:ngibbs){
                arate <- ar/(4*g)
                if((g == 5) | (g %% (ngibbs/20) == 0) & local.store) laptime(tstart, g, ngibbs)

                if(g %% ar.step == 0){
                        ## Tweak JumpRSD based on acceptance rate
                        arate <- (ar - arp)/(4*ar.step)
                        if(arate < ar.min){
                                tweak <- max(arate/ar.target, 0.001)
                                JumpSD <- JumpSD * tweak
                                cat(sprintf("   Iter %d, AR %.3f , JSD x %g \n", g, arate, tweak))
                        }
                        if(arate > ar.max){
                                tweak <- min(ar.target/arate, 10000)
                                JumpSD <- JumpSD * tweak
                                cat(sprintf("   Iter %d, AR %.3f , JSD x %g \n", g, arate, tweak))
                        }
                        arp <- ar
                }
                ### Sample core PROSPECT parameters ###

                # Sample N
                guess.N <- rtnorm(1, N.i, JumpSD["N"], Min=1)
                if(random.effects != "none"){
                        guess.error.alpha <- lapply(1:nre,
                                                    function(i) {
                                                            guess.spec <- prospect(guess.N + alphaN.i[i],
                                                                                   Cab.i + alphaCab.i[i],
                                                                                   Cw.i + alphaCw.i[i],
                                                                                   Cm.i + alphaCm.i[i]
                                                                                   )
                                                            guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                            return(guess.error)
                                                    }
                                                    )
                        guess.error <- do.call(cbind, guess.error.alpha)
                } else {
                        guess.spec <- prospect(guess.N, Cab.i, Cw.i, Cm.i)
                        guess.error <- spec.error(guess.spec, obs.spec)
                }
                guess.posterior <- likelihood(guess.error, sd.i) + N.prior(guess.N)              
                prev.posterior <- likelihood(prev.error, sd.i) + N.prior(N.i)
                jnum <- dtnorm(guess.N, N.i, JumpSD["N"], Min=1)
                jden <- dtnorm(N.i, guess.N, JumpSD["N"], Min=1)
                a <- exp((guess.posterior - jnum ) - (prev.posterior - jden ))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        N.i <- guess.N
                        prev.error <- guess.error
                        ar <- ar + 1
                }
                # Sample Cab
                guess.Cab <- rtnorm(1, Cab.i, JumpSD["Cab"])
                if(random.effects != "none"){
                        guess.error.alpha <- lapply(1:nre,
                                                    function(i) {
                                                            guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                   guess.Cab + alphaCab.i[i],
                                                                                   Cw.i + alphaCw.i[i],
                                                                                   Cm.i + alphaCm.i[i]
                                                                                   )
                                                            guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                            return(guess.error)
                                                    }
                                                    )
                        guess.error <- do.call(cbind, guess.error.alpha)
                } else {
                        guess.spec <- prospect(N.i, guess.Cab, Cw.i, Cm.i)
                        guess.error <- spec.error(guess.spec, obs.spec)
                }
                guess.posterior <- likelihood(guess.error, sd.i) + Cab.prior(guess.Cab)
                prev.posterior <- likelihood(prev.error, sd.i) + Cab.prior(Cab.i)
                jnum <- dlnorm(guess.Cab, Cab.i, JumpSD["Cab"])
                jden <- dlnorm(Cab.i, guess.Cab, JumpSD["Cab"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cab.i <- guess.Cab
                        prev.error <- guess.error
                        ar <- ar + 1
                }

                # Sample Cw
                guess.Cw <- rtnorm(1, Cw.i, JumpSD["Cw"])
                if(random.effects != "none"){
                        guess.error.alpha <- lapply(1:nre,
                                                    function(i) {
                                                            guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                   Cab.i + alphaCab.i[i],
                                                                                   guess.Cw + alphaCw.i[i],
                                                                                   Cm.i + alphaCm.i[i]
                                                                                   )
                                                            guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                            return(guess.error)
                                                    }
                                                    )
                        guess.error <- do.call(cbind, guess.error.alpha)
                } else {
                        guess.spec <- prospect(N.i, Cab.i, guess.Cw, Cm.i)
                        guess.error <- spec.error(guess.spec, obs.spec)
                }
                guess.posterior <- likelihood(guess.error, sd.i) + Cw.prior(guess.Cw)
                prev.posterior <- likelihood(prev.error, sd.i) + Cw.prior(Cw.i)
                jnum <- dlnorm(guess.Cw, Cw.i, JumpSD["Cw"])
                jden <- dlnorm(Cw.i, guess.Cw, JumpSD["Cw"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cw.i <- guess.Cw
                        prev.error <- guess.error
                        ar <- ar + 1
                }

                # Sample Cm
                guess.Cm <- rtnorm(1, Cm.i, JumpSD["Cm"])
                if(random.effects != "none"){
                        guess.error.alpha <- lapply(1:nre,
                                                    function(i) {
                                                            guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                   Cab.i + alphaCab.i[i],
                                                                                   Cw.i + alphaCw.i[i],
                                                                                   guess.Cm + alphaCm.i[i]
                                                                                   )
                                                            guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                            return(guess.error)
                                                    }
                                                    )
                        guess.error <- do.call(cbind, guess.error.alpha)
                } else {
                        guess.spec <- prospect(N.i, Cab.i, Cw.i, guess.Cm)
                        guess.error <- spec.error(guess.spec, obs.spec)
                }
                guess.posterior <- likelihood(guess.error, sd.i) + Cm.prior(guess.Cm)
                prev.posterior <- likelihood(prev.error, sd.i) + Cm.prior(Cm.i)
                jnum <- dlnorm(guess.Cm, Cm.i, JumpSD["Cm"])
                jden <- dlnorm(Cm.i, guess.Cm, JumpSD["Cm"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cm.i <- guess.Cm
                        prev.error <- guess.error
                        ar <- ar + 1
                }

                ### Sample random effects ###

                if(random.effects != 'none'){
                        ## Sample alphaN
                        for (i in 1:nre){
                                guess.alphaN <- alphaN.i
                                guess.alphaN[i] <- rnorm(1, alphaN.i[i], JumpRSD * alphaN.i[i])
                                guess.error.alpha <- lapply(1:nre,
                                                            function(i) {
                                                                    guess.spec <- prospect(N.i + guess.alphaN[i],
                                                                                           Cab.i + alphaCab.i[i],
                                                                                           Cw.i + alphaCw.i[i],
                                                                                           Cm.i + alphaCm.i[i]
                                                                                           )
                                                                    guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                                    return(guess.error)
                                                            }
                                                            )
                                guess.error <- do.call(cbind, guess.error.alpha)
                                guess.posterior <- likelihood(guess.error, sd.i) + dnorm(guess.alphaN[i], 0, sdreN)
                                prev.posterior <- likelihood(prev.error, sd.i) + dnorm(alphaN.i[i], 0, sdreN)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaN.i <- guess.alphaN
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCab
                        for (i in 1:nre){
                                guess.alphaCab <- alphaCab.i
                                guess.alphaCab[i] <- rnorm(1, alphaCab.i[i], JumpRSD * alphaCab.i[i])
                                guess.error.alpha <- lapply(1:nre,
                                                            function(i) {
                                                                    guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                           Cab.i + guess.alphaCab[i],
                                                                                           Cw.i + alphaCw.i[i],
                                                                                           Cm.i + alphaCm.i[i]
                                                                                           )
                                                                    guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                                    return(guess.error)
                                                            }
                                                            )
                                guess.error <- do.call(cbind, guess.error.alpha)
                                guess.posterior <- likelihood(guess.error, sd.i) + dnorm(guess.alphaCab[i], 0, sdreCab)
                                prev.posterior <- likelihood(prev.error, sd.i) + dnorm(alphaCab.i[i], 0, sdreCab)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCab.i <- guess.alphaCab
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCw
                        for (i in 1:nre){
                                guess.alphaCw <- alphaCw.i
                                guess.alphaCw[i] <- rnorm(1, alphaCw.i[i], JumpRSD * alphaCw.i[i])
                                guess.error.alpha <- lapply(1:nre,
                                                            function(i) {
                                                                    guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                           Cab.i + alphaCab.i[i],
                                                                                           Cw.i + guess.alphaCw[i],
                                                                                           Cm.i + alphaCm.i[i]
                                                                                           )
                                                                    guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                                    return(guess.error)
                                                            }
                                                            )
                                guess.error <- do.call(cbind, guess.error.alpha)
                                guess.posterior <- likelihood(guess.error, sd.i) + dnorm(guess.alphaCw[i], 0, sdreCw)
                                prev.posterior <- likelihood(guess.error, sd.i) + dnorm(alphaCw.i[i], 0, sdreCw)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCw.i <- guess.alphaCw
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCm
                        for (i in 1:nre){
                                guess.alphaCm <- alphaCm.i
                                guess.alphaCm[i] <- rnorm(1, alphaCm.i[i], JumpRSD * alphaCm.i[i])
                                guess.error.alpha <- lapply(1:nre,
                                                            function(i) {
                                                                    guess.spec <- prospect(N.i + alphaN.i[i],
                                                                                           Cab.i + alphaCab.i[i],
                                                                                           Cw.i + alphaCw.i[i],
                                                                                           Cm.i + guess.alphaCm[i]
                                                                                           )
                                                                    guess.error <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                                                    return(guess.error)
                                                            }
                                                            )
                                guess.error <- do.call(cbind, guess.error.alpha)
                                guess.posterior <- likelihood(guess.error, sd.i) + dnorm(guess.alphaCm[i], 0, sdreCm)
                                prev.posterior <- likelihood(guess.error, sd.i) + dnorm(alphaCm.i[i], 0, sdreCm)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCm.i <- guess.alphaCm
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ### Sample alphaN precision ###
                        v1 <- randeff.s[1] + nre/2

                        v2N <- randeff.s[2] + 0.5 * sum(alphaN.i^2)
                        preN <- rgamma(1, v1, v2N)
                        sdreN <- 1/sqrt(preN)

                        v2Cab <- randeff.s[2] + 0.5 * sum(alphaCab.i^2)
                        preCab <- rgamma(1, v1, v2Cab)
                        sdreCab <- 1/sqrt(preCab)

                        v2Cw <- randeff.s[2] + 0.5 * sum(alphaCw.i^2)
                        preCw <- rgamma(1, v1, v2Cw)
                        sdreCw <- 1/sqrt(preCw)

                        v2Cm <- randeff.s[2] + 0.5 * sum(alphaCm.i^2)
                        preCm <- rgamma(1, v1, v2Cm)
                        sdreCm <- 1/sqrt(preCm)
                }

                ### Sample error precision ### 
                if(single.precision){
                        nprec <- 1
                        u1p <- nspec*nwl/2
                        u2p <- (nspec*nwl - 1) * var(c(prev.error))
                } else {
                        nprec <- nwl
                        u1p <- nspec/2
                        u2p <- 0.5 * rowSums(prev.error^2)
                }
                u1 <- pwl.p[1] + u1p
                u2 <- pwl.p[2] + u2p
                pwl.i <- rgamma(nprec, u1, u2)
                sd.i <- 1/sqrt(pwl.i)

                # Store values 
                if (local.store){
                        N.store[g] <- N.i
                        Cab.store[g] <- Cab.i
                        Cw.store[g] <- Cw.i
                        Cm.store[g] <- Cm.i
                        if(single.precision){
                                sd.store[g] <- sd.i
                        } else{
                                sd.store[g,] <- sd.i
                        }
                        if(random.effects != "none"){
                                sdplotN.store[g] <- sdreN
                                sdplotCab.store[g] <- sdreCab
                                sdplotCw.store[g] <- sdreCw
                                sdplotCm.store[g] <- sdreCm
                                alphaN.store[g,] <- alphaN.i
                                alphaCab.store[g,] <- alphaCab.i
                                alphaCw.store[g,] <- alphaCw.i
                                alphaCm.store[g,] <- alphaCm.i
                        }
                } else {
                        if(random.effects != "none"){
                        write(c(N.i, Cab.i, Cw.i, Cm.i,
                                sdreN, sdreCab, sdreCw, sdreCm, 
                                sd.i), 
                              ncolumns=length(header),
                              sep=",",
                              file=fname,
                              append=TRUE)
                        } else {
                        write(c(N.i, Cab.i, Cw.i, Cm.i,
                                sd.i), 
                              ncolumns=length(header),
                              sep=",",
                              file=fname,
                              append=TRUE)
                        }

                }
        }

        if (local.store){
                if(random.effects != "none"){
                returnlist <- list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, sd=sd.store,
                            sdplotN=sdplotN.store,
                            sdplotCab=sdplotCab.store,
                            sdplotCw=sdplotCw.store,
                            sdplotCm=sdplotCm.store)
                } else {
                returnlist <- list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, sd=sd.store)
                }
                return(returnlist)
        }
}

