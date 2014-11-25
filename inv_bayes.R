#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.R")
source("truncnorm.R")

inits <- list(N=1.4, 
              Cab=30,
              Cw=0.017,
              Cm=0.006,
              pwl=rep(1, 2101)
              )

## NOTE: obs.spec must be a matrix as follows:
## Column 1 : Wavelengths (400:2500)
## Columns 2-n : Reflectance observations
## Use specdatproc script to generate correct matrices from data.
pinvbayes <- function(obs.spec,
                      ngibbs=100,
                      initc=inits,
                      JumpRSD=0.1,
                      local.store=FALSE,
                      single.precision=TRUE,
                      random.effects='none',
                      random.inits=FALSE,
                      ar.step=100,
                      ar.min=0.1,
                      ar.max=0.9,
                      ar.tweak=5,
                      fname = "runs/test_run.dat"
                      )
{
        wl <- min(obs.spec[,1]):max(obs.spec[,1])
        nwl <- length(wl)
        nspec <- ncol(obs.spec)
        JumpSD <- JumpRSD * unlist(initc)

        ### Priors ###
        N.s <- c(0, 1.5)                # Halfnormal (N = 1 + rlnorm)

        # Based on histograms in Feret et al. 2008
        Cab.s <- c(log(30), 0.9)          # Lognormal
        Cw.s <- c(log(0.017), 0.5)        # Lognormal
        Cm.s <- c(log(0.006), 0.9)        # Lognormal

        # Error
        pwl.s <- c(0.001, 0.001)          # Inverse gamma
        
        ### Initial conditions
        if(!random.inits){
                N.i <- initc[["N"]]
                Cab.i <- initc[["Cab"]]
                Cw.i <- initc[["Cw"]]
                Cm.i <- initc[["Cm"]]
        } else {
                N.i <- abs(rnorm(1, N.s[1], N.s[2])) + 1
                Cab.i <- rlnorm(1, Cab.s[1], Cab.s[2])
                Cw.i <- rlnorm(1, Cw.s[1], Cw.s[2])
                Cm.i <- rlnorm(1, Cm.s[1], Cm.s[2])
        }

        pwl.i <- rep(1, 2101)
        if(single.precision) pwl.i <- pwl.i[1]

        

        ### Extract indices for random effects ###
        if(random.effects != 'none'){
                regxp.list <- c(leaf = ".*_(L[1-9].*)_.*",
                                plot = ".*_Plot[1-9a-zA-Z]+_.*")
                randeff.regxp <- regxp.list[random.effects]
                randeffs <- unique(gsub(randeff.regxp, "\\1", colnames(obs.spec)[-1]))
                randeff.list <- lapply(randeffs, grep, colnames(obs.spec))
                print(randeff.list)
                nre <- length(randeff.list)

                ### Random effects initial conditions
                alphaN.i <- rep(0, nre)
                alphaCab.i <- rep(0, nre)
                alphaCw.i <- rep(0, nre)
                alphaCm.i <- rep(0, nre)

                pplotN <- 1
                pplotCab <- 1
                pplotCw <- 1
                pplotCm <- 1

                # Random effects
                randeff.s <- c(0.001, 0.001)              # Inverse gamma
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

        likelihood <- function(guess.error, pwl.i) sum(dgamma(colSums(guess.error^2),
                                                              (nwl-1)/2,
                                                              pwl.i,
                                                              log=TRUE))
        N.prior <- function(N) dnorm(N - 1, N.s[1], N.s[2], log=TRUE) + log(2)
        Cab.prior <- function(Cab) dlnorm(Cab, Cab.s[1], Cab.s[2], log=TRUE)
        Cw.prior <- function(Cw) dlnorm(Cw, Cw.s[1], Cw.s[2], log=TRUE)
        Cm.prior <- function(Cm) dlnorm(Cm, Cm.s[1], Cm.s[2], log=TRUE)

        # Precalculate first model and posterior
        prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
        prev.error <- spec.error(prev.spec, obs.spec)
        prev.posterior <- (likelihood(prev.error, pwl.i) + 
                           N.prior(N.i) +
                           Cab.prior(Cab.i) +
                           Cw.prior(Cw.i) +
                           Cm.prior (Cm.i)
                           )
        # Random effects
        re.leaf.s <- c(0.001, 0.001)              # Inverse gamma

        ### MCMC storage
        if (local.store){
                N.store <- numeric(ngibbs)
                Cab.store <- numeric(ngibbs)
                Cw.store <- numeric(ngibbs)
                Cm.store <- numeric(ngibbs)
                if(single.precision){
                        pwl.store <- numeric(ngibbs)
                } else {
                        pwl.store <- matrix(NA, nrow=ngibbs, ncol=nwl)
                }
                if(random.effects != 'none'){
                        pplotN.store <- numeric(ngibbs)
                        pplotCab.store <- numeric(ngibbs)
                        pplotCw.store <- numeric(ngibbs)
                        pplotCm.store <- numeric(ngibbs)
                        alphaN.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCab.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCw.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                        alphaCm.store <- matrix(NA, nrow=ngibbs, ncol=nre)
                }
                
        } else {
                if(single.precision) {
                        pvec <- "p"
                } else {
                        pvec <- paste("p", wl, sep='')
                }
                if(random.effects != "none"){
                        header <- c("N", "Cab", "Cw", "Cm", 
                                    "pleafN", "pleafCab", "pleafCw", "pleafCm",
                                    pvec)
                } else {
                        header <- c("N", "Cab", "Cw", "Cm", 
                                    pvec)
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
                                JumpSD <- JumpSD/ar.tweak
                                print(sprintf("   Iter %d, AR %.3f , JSD / %.1f", g, arate, ar.tweak))
                        }
                        if(arate > ar.max){
                                JumpSD <- JumpSD*ar.tweak
                                cat(sprintf("   Iter %d, AR %.3f , JSD x %.1f \n", g, arate, ar.tweak))
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
                        guess.error <- spec.error(guess.spec, obs.spec[, -1])
                }
                gpn1 <- likelihood(guess.error, pwl.i)
                gpn2 <- N.prior(guess.N)
                guess.posterior <-  gpn1 + gpn2       
                jnum <- dtnorm(guess.N, N.i, JumpSD["N"], Min=1)
                jden <- dtnorm(N.i, guess.N, JumpSD["N"], Min=1)
                a <- exp((guess.posterior ) - (prev.posterior ))
                print(sprintf("%g", c(guess.N, as.numeric(JumpSD["N"]), gpn1, gpn2, prev.posterior, a)))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        N.i <- guess.N
                        prev.error <- guess.error
                        prev.posterior <- guess.posterior
                        ar <- ar + 1
                        print("Accepted!")
                }
                print("----------")
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
                        guess.error <- spec.error(guess.spec, obs.spec[, -1])
                }
                guess.posterior <- likelihood(guess.error, pwl.i) + Cab.prior(guess.Cab)
                jnum <- dlnorm(guess.Cab, Cab.i, JumpSD["Cab"])
                jden <- dlnorm(Cab.i, guess.Cab, JumpSD["Cab"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cab.i <- guess.Cab
                        prev.error <- guess.error
                        prev.posterior <- guess.posterior
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
                        guess.error <- spec.error(guess.spec, obs.spec[, -1])
                }
                guess.posterior <- likelihood(guess.error, pwl.i) + Cw.prior(guess.Cw)
                jnum <- dlnorm(guess.Cw, Cw.i, JumpSD["Cw"])
                jden <- dlnorm(Cw.i, guess.Cw, JumpSD["Cw"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cw.i <- guess.Cw
                        prev.error <- guess.error
                        prev.posterior <- guess.posterior
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
                        guess.error <- spec.error(guess.spec, obs.spec[, -1])
                }
                guess.posterior <- likelihood(guess.error, pwl.i) + Cm.prior(guess.Cm)
                jnum <- dlnorm(guess.Cm, Cm.i, JumpSD["Cm"])
                jden <- dlnorm(Cm.i, guess.Cm, JumpSD["Cm"])
                a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
                if(is.na(a)) a <- -1
                if(a > runif(1)){
                        Cm.i <- guess.Cm
                        prev.error <- guess.error
                        prev.posterior <- guess.posterior
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
                                guess.posterior <- likelihood(guess.error, pwl.i) + dnorm(guess.alphaN[i], 1/sqrt(pplotN))
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaN.i <- guess.alphaN
                                        prev.posterior <- guess.posterior
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
                                guess.posterior <- likelihood(guess.error, pwl.i) + dnorm(guess.alphaCab[i], 1/sqrt(pplotCab))
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCab.i <- guess.alphaCab
                                        prev.posterior <- guess.posterior
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
                                guess.posterior <- likelihood(guess.error, pwl.i) + dnorm(guess.alphaCw[i], 1/sqrt(pplotCw))
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCw.i <- guess.alphaCw
                                        prev.posterior <- guess.posterior
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
                                guess.posterior <- likelihood(guess.error, pwl.i) + dnorm(guess.alphaCm[i], 1/sqrt(pplotCm))
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCm.i <- guess.alphaCm
                                        prev.posterior <- guess.posterior
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ### Sample alphaN precision ###
                        v1 <- randeff.s[1] + nre/2

                        v2N <- randeff.s[2] + 0.5 * sum(alphaN.i^2)
                        pplotN <- rgamma(1, v1, v2N)

                        v2Cab <- randeff.s[2] + 0.5 * sum(alphaCab.i^2)
                        pplotCab <- rgamma(1, v1, v2Cab)

                        v2Cw <- randeff.s[2] + 0.5 * sum(alphaCw.i^2)
                        pplotCw <- rgamma(1, v1, v2Cw)

                        v2Cm <- randeff.s[2] + 0.5 * sum(alphaCm.i^2)
                        pplotCm <- rgamma(1, v1, v2Cm)
                }

                ### Sample error precision ### 
                if(single.precision){
                        nprec <- 1
                        u1p <- nspec
                        u2p <- 0.5 * sum(colSums(prev.error^2))
                } else {
                        nprec <- nwl
                        u1p <- nspec/2
                        u2p <- 0.5 * rowSums(prev.error^2)
                }
                u1 <- pwl.s[1] + u1p
                u2 <- pwl.s[2] + u2p
                pwl.i <- rgamma(nprec, u1, u2)

                # Store values 
                if (local.store){
                        N.store[g] <- N.i
                        Cab.store[g] <- Cab.i
                        Cw.store[g] <- Cw.i
                        Cm.store[g] <- Cm.i
                        if(single.precision){
                                pwl.store[g] <- pwl.i  
                        } else{
                                pwl.store[g,] <- pwl.i
                        }
                        if(random.effects != "none"){
                                pplotN.store[g] <- pplotN
                                pplotCab.store[g] <- pplotCab
                                pplotCw.store[g] <- pplotCw
                                pplotCm.store[g] <- pplotCm
                                alphaN.store[g,] <- alphaN.i
                                alphaCab.store[g,] <- alphaCab.i
                                alphaCw.store[g,] <- alphaCw.i
                                alphaCm.store[g,] <- alphaCm.i
                        }
                } else {
                        if(random.effects != "none"){
                        write(c(N.i, Cab.i, Cw.i, Cm.i,
                                pplotN, pplotCab, pplotCw, pplotCm, 
                                pwl.i), 
                              ncolumns=length(header),
                              sep=",",
                              file=fname,
                              append=TRUE)
                        } else {
                        write(c(N.i, Cab.i, Cw.i, Cm.i,
                                pwl.i), 
                              ncolumns=length(header),
                              sep=",",
                              file=fname,
                              append=TRUE)
                        }

                }
        }

        if (local.store){
                if(random.effects != "none"){
                returnlist <- list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store,
                            pplotN=pplotN.store,
                            pplotCab=pplotCab.store,
                            pplotCw=pplotCw.store,
                            pplotCm=pplotCm.store)
                } else {
                returnlist <- list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store)
                }
                return(returnlist)
        }
}

