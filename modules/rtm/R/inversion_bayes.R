##' @name pinvbayes
##' @title Prospect Bayesian Inversion
##' 
##' @details {
##' Function to perform Bayesian inversion of PROSPECT4 model
##' using measured reflectance data.
##' }
##' 
##' @param obs.spec Matrix of observed reflectance 400-2500 nm. Each row is a wavelength and each column is one spectrum.
##' @param ngibbs Number of iterations (default = 100)
##' @param JumpRSD Initial relative standard deviation of Jump distribution (default = 0.1).
##' @param wl Wavelengths over which to perform inversion (must match data) (default = 400:2500)
##' @param single.precision If TRUE (default), a single residual SD is calculated for the entire spectrum. If FALSE, a value is calcluated for each wavelength.
##' @param random.effects What kind of random effects should be included. Options are 'none' (default), 'leaf', 'plot'
##' @param inits How initial conditions are generated. Options are maximum likelihood ('mle', default), 'random', or 'guess'.
##' @param ar.step Interval on which JumpRSD is adjusted to maintain a good acceptance rate (default = 10).
##' @param ar.target Target acceptance rate (default = 0.75)
##' @param fname Filename for storing results (default = 'runs/test_run.dat')
##' @export
##'
##' @author Alexey Shiklomanov

source("prospect.R")
source("timer.R")
source("truncnorm.R")
source("inversion_mle.R")

## Use specdatproc script to generate correct "obs.spec" matrix from data.
pinvbayes <- function(obs.spec,
                      ngibbs=100,
                      JumpRSD=0.1,
                      wl=400:2500,
                      single.precision=TRUE,
                      random.effects='none',
                      inits='mle',
                      ar.step=10,
                      ar.target=0.75,
                      fname = "TESTRUN.dat"
                      )
{
        nwl <- length(wl)
        if(!is.matrix(obs.spec)) obs.spec <- as.matrix(obs.spec)
        nspec <- ncol(obs.spec)
        JumpSD <- JumpRSD * unlist(guess.inits)
        JumpSD.alpha <- JumpSD


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
        nre <- 1
        if(random.effects != 'none'){
                regxp.list <- c(leaf_SE = "(^.*_)[0-9]{5}.csv",
                                leaf_FFT = "(.*)",
                                plot = "^[0-9]{4}[A-Za-z]+[0-9]{2}__([A-Za-z]+_[A-Za-z0-9]+_).*")
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
        if(single.precision) {
                sdvec <- "sd"
        } else {
                sdvec <- paste("sd", wl, sep='')
        }
        if(random.effects != "none"){
                aNvec <- sprintf("aN_%s", randeffs)
                aCabvec <- sprintf("aCab_%s", randeffs)
                aCwvec <- sprintf("aCw_%s", randeffs)
                aCmvec <- sprintf("aCm_%s", randeffs)
                header <- c("N", "Cab", "Cw", "Cm", 
                            aNvec, aCabvec, aCwvec, aCmvec,
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

        ## MCMC loop
        tstart <- proc.time()
        ar <- 0
        ar.alpha <- 0
        arp <- 0
        arp.alpha <- 0
        for(g in 1:ngibbs){
                arate <- ar/(4*g)
                if((g == 5) | (g %% (ngibbs/20) == 0)) laptime(tstart, g, ngibbs)

                if(g %% ar.step == 0){
                        ## Tweak JumpRSD based on acceptance rate
                        arate <- (ar - arp)/(4*ar.step)
                        arate.alpha <- (ar.alpha - arp.alpha)/(4*nre*ar.step)
                        JumpSD <- JumpSD * max(arate/ar.target, 0.001)
                        JumpSD.alpha <- JumpSD.alpha * max(arate.alpha/ar.target, 0.001)
                        arp <- ar
                        arp.alpha <- ar.alpha
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
                stopifnot(dim(guess.error) == dim(prev.error)) ## Throw error in case of regular expression mismatch
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
                        prev.like <- likelihood(prev.error, sd.i)
                        ## Sample alphaN
                        for (i in 1:nre){
                                guess.alphaN <- alphaN.i
                                guess.alphaN[i] <- rnorm(1, alphaN.i[i], JumpSD.alpha["N"])
                                guess.spec <- prospect(N.i + guess.alphaN[i],
                                                       Cab.i + alphaCab.i[i],
                                                       Cw.i + alphaCw.i[i],
                                                       Cm.i + alphaCm.i[i]
                                                       )
                                guess.error <- prev.error
                                guess.error[,randeff.list[[i]]] <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                guess.like <- likelihood(guess.error, sd.i)
                                guess.posterior <- guess.like + dnorm(guess.alphaN[i], 0, sdreN, log=TRUE)
                                prev.posterior <- prev.like + dnorm(alphaN.i[i], 0, sdreN, log=TRUE)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaN.i <- guess.alphaN
                                        prev.error <- guess.error
                                        prev.like <- guess.like
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCab
                        for (i in 1:nre){
                                guess.alphaCab <- alphaCab.i
                                guess.alphaCab[i] <- rnorm(1, alphaCab.i[i], JumpSD.alpha["Cab"])
                                guess.spec <- prospect(N.i + alphaN.i[i],
                                                       Cab.i + guess.alphaCab[i],
                                                       Cw.i + alphaCw.i[i],
                                                       Cm.i + alphaCm.i[i]
                                                       )
                                guess.error <- prev.error
                                guess.error[,randeff.list[[i]]] <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                guess.like <- likelihood(guess.error, sd.i)
                                guess.posterior <- guess.like + dnorm(guess.alphaCab[i], 0, sdreCab, log=TRUE)
                                prev.posterior <- prev.like + dnorm(alphaCab.i[i], 0, sdreCab, log=TRUE)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCab.i <- guess.alphaCab
                                        prev.error <- guess.error
                                        prev.like <- guess.like
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCw
                        for (i in 1:nre){
                                guess.alphaCw <- alphaCw.i
                                guess.alphaCw[i] <- rnorm(1, alphaCw.i[i], JumpSD.alpha["Cw"])
                                guess.spec <- prospect(N.i + alphaN.i[i],
                                                       Cab.i + alphaCab.i[i],
                                                       Cw.i + guess.alphaCw[i],
                                                       Cm.i + alphaCm.i[i]
                                                       )
                                guess.error <- prev.error
                                guess.error[,randeff.list[[i]]] <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                guess.like <- likelihood(guess.error, sd.i)
                                guess.posterior <- guess.like + dnorm(guess.alphaCw[i], 0, sdreCw, log=TRUE)
                                prev.posterior <- prev.like + dnorm(alphaCw.i[i], 0, sdreCw, log=TRUE)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCw.i <- guess.alphaCw
                                        prev.error <- guess.error
                                        prev.like <- guess.like
                                        ar.alpha <- ar.alpha + 1
                                }
                        }

                        ## Sample alphaCm
                        for (i in 1:nre){
                                guess.alphaCm <- alphaCm.i
                                guess.alphaCm[i] <- rnorm(1, alphaCm.i[i], JumpSD.alpha["Cm"])
                                guess.spec <- prospect(N.i + alphaN.i[i],
                                                       Cab.i + alphaCab.i[i],
                                                       Cw.i + alphaCw.i[i],
                                                       Cm.i + guess.alphaCm[i]
                                                       )
                                guess.error <- prev.error
                                guess.error[,randeff.list[[i]]] <- spec.error(guess.spec, obs.spec[, randeff.list[[i]]])
                                guess.like <- likelihood(guess.error, sd.i)
                                guess.posterior <- guess.like + dnorm(guess.alphaCm[i], 0, sdreCm, log=TRUE)
                                prev.posterior <- prev.like + dnorm(alphaCm.i[i], 0, sdreCm, log=TRUE)
                                a <- exp(guess.posterior - prev.posterior)
                                if(is.na(a)) a <- -1
                                if(a > runif(1)){
                                        alphaCm.i <- guess.alphaCm
                                        prev.error <- guess.error
                                        prev.like <- guess.like
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
                if(random.effects != "none"){
                        write(c(N.i, Cab.i, Cw.i, Cm.i,
                                alphaN.i, alphaCab.i, alphaCw.i, alphaCm.i,
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

