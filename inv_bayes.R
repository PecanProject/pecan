#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.R")
source("truncnorm.R")

samp.inits <- list(N=1, 
                   Cab=30,
                   Cw=0.01,
                   Cm=0.005,
                   )

## NOTE: obs.spec must be a matrix as follows:
## Column 1 : Wavelengths (400:2500)
## Columns 2-n : Reflectance observations
## Use specdatproc script to generate correct matrices from data.
pinvbayes <- function(obs.spec, prospect=prospect4, ngibbs=100,
                      initc=samp.inits,
                      JumpRSD=0.1,
                      local.store=FALSE,
                      sample.together=FALSE, 
                      single.precision=TRUE,
                      random.effects='none',
                      random.inits=FALSE
                      ar.step=100,
                      ar.min=0.1,
                      ar.max=0.9,
                      ar.tweak=5,
                      fname = sprintf("runs/%s_%g.dat", deparse(substitute(obs.spec)), JumpRSD))
{
        wl <- min(obs.spec[,1]):max(obs.spec[,1])
        nwl <- length(wl)
        nspec <- ncol(obs.spec)
        JumpSD <- JumpRSD * unlist(initc)

        ### Priors
        N.s <- c(0, 1.5)                # Halfnormal (N = 1 + rlnorm)

        # Based on histograms in Feret et al. 2008
        Cab.s <- c(log(30), 0.9)          # Lognormal
        Cw.s <- c(log(0.017), 0.5)        # Lognormal
        Cm.s <- c(log(0.006), 0.9)        # Lognormal

        pwl.s <- c(0.001, 0.001)          # Inverse gamma
        
        ### Initial conditions
        if(!random.inits){
        N.i <- initc[["N"]]
        Cab.i <- initc[["Cab"]]
        Cw.i <- initc[["Cw"]]
        Cm.i <- initc[["Cm"]]
        } else {
        N.i <- rnorm(1, N.s[1], N.s[2])
        Cab.i <- rnorm(1, Cab.s[1], Cab.s[2])
        Cw.i <- rnorm(1, Cw.s[1], Cw.s[2])
        Cm.i <- rnorm(1, Cm.s[1], Cm.s[2])
        }

        pwl.i <- rep(1, 2101)
        if(single.precision) pwl.i <- pwl.i[1]

        ### Define sampler functions
        try.error <- function(N, Cab, Cw, Cm){

                ## Calculate modeled spectra and residuals
                guess.spec <- prospect(N, Cab, Cw, Cm, n.a, cab.a, w.a, m.a)
                guess.error <- -apply(obs.spec[,-1], 2, "-", guess.spec)
                return(guess.error)
        }

        try.posterior <- function(N, Cab, Cw, Cm, pwl.i, guess.error){

                ## Evaluate posterior | PROSPECT
                gp1 <- sum(dnorm(guess.error, 0, 1/sqrt(pwl.i), log=TRUE))  # Likelihood
                gp2 <- dlnorm(N - 1, N.s[1], N.s[2], log=TRUE) + log(2)    # N prior
                gp3 <- dlnorm(Cab, Cab.s[1], Cab.s[2], log=TRUE)    # Cab prior
                gp4 <- dlnorm(Cw, Cw.s[1], Cw.s[2], log=TRUE)     # Cw prior
                gp5 <- dlnorm(Cm, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior
                guess.posterior <- gp1 + gp2 + gp3 + gp4 + gp5
                return(guess.posterior)
        }

        # Precalculate first model and posterior
        prev.error <- try.error(N.i, Cab.i, Cw.i, Cm.i)
        prev.posterior <- try.posterior(N.i, Cab.i, Cw.i, Cm.i, pwl.i, prev.error)

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
        } else {
                if(single.precision) {
                  pvec <- "p"
                } else {
                  pvec <- paste("p", wl, sep='')
                }
                header <- c("N", "Cab", "Cw", "Cm", pvec)
                write(header,
                      ncolumns=length(header),
                      file=fname, 
                      sep=",")
        }

        ## MCMC loop
        tstart <- proc.time()
        ar <- 0
        arp <- 0
        for(g in 1:ngibbs){
                arate <- (ar/(4 - 3*sample.together))/g
                if((g == 5) | (g %% (ngibbs/20) == 0)) laptime(tstart, g, ngibbs)

                if(g %% ar.step == 0){
                        ## Tweak JumpRSD based on acceptance rate
                        arate <- (ar - arp)/100
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

                if(sample.together){
                        ## Draw PROSPECT parameters
                        guess.N <- rtnorm(1, N.i, JumpSD[1], Min=1)
                        guess.Cab <- rtnorm(1, Cab.i, JumpSD[2])
                        guess.Cw <- rtnorm(1, Cw.i, JumpSD[3])
                        guess.Cm <- rtnorm(1, Cm.i, JumpSD[4])

                        guess.error <- try.error(guess.N, guess.Cab, guess.Cw, guess.Cm)
                        guess.posterior <- try.posterior(guess.N, guess.Cab, guess.Cw, guess.Cm,
                                                         pwl.i, guess.error)

                        ## Test acceptance w/ Jump Distribution
                        jn1 <- dtnorm(guess.N, N.i, JumpSD[1], Min=1)
                        jn2 <- dtnorm(guess.Cab, Cab.i, JumpSD[2])
                        jn3 <- dtnorm(guess.Cw, Cw.i, JumpSD[3])
                        jn4 <- dtnorm(guess.Cm, Cm.i, JumpSD[4])
                        jnum <-jn1 + jn2 + jn3 + jn4

                        jd1 <- dtnorm(N.i, guess.N, JumpSD[1], Min=1)
                        jd2 <- dtnorm(Cab.i, guess.Cab, JumpSD[2])
                        jd3 <- dtnorm(Cw.i, guess.Cw, JumpSD[3])
                        jd4 <- dtnorm(Cm.i, guess.Cm, JumpSD[4])
                        jden <- jd1 + jd2 + jd3 + jd4

                        a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

                        if(is.na(a)) a <- -1
                        if(a > runif(1)){
                                N.i <- guess.N
                                Cab.i <- guess.Cab
                                Cw.i <- guess.Cw
                                Cm.i <- guess.Cm
                                prev.error <- guess.error
                                prev.posterior <- guess.posterior
                                ar <- ar + 1 
                        }
                } else {
                        ## Sample N
                        guess.N <- rtnorm(1, N.i, JumpSD["N"], Min=1)
                        guess.error <- try.error(guess.N, Cab.i, Cw.i, Cm.i)
                        guess.posterior <- try.posterior(guess.N, Cab.i, Cw.i, Cm.i, pwl.i, guess.error)

                        jnum <- dtnorm(guess.N, N.i, JumpSD["N"], Min=1)
                        jden <- dtnorm(N.i, guess.N, JumpSD["N"], Min=1)
                        a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

                        if(is.na(a)) a <- -1
                        if(a > runif(1)){
                                N.i <- guess.N
                                prev.error <- guess.error
                                prev.posterior <- guess.posterior
                                ar <- ar + 1
                        }

                        ## Sample Cab
                        guess.Cab <- rtnorm(1, Cab.i, JumpSD["Cab"])
                        guess.error <- try.error(N.i, guess.Cab, Cw.i, Cm.i)
                        guess.posterior <- try.posterior(N.i, guess.Cab, Cw.i, Cm.i, pwl.i, guess.error)

                        jnum <- dtnorm(guess.Cab, Cab.i, JumpSD["Cab"])
                        jden <- dtnorm(Cab.i, guess.Cab, JumpSD["Cab"])
                        a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

                        if(is.na(a)) a <- -1
                        if(a > runif(1)){
                                Cab.i <- guess.Cab
                                prev.error <- guess.error
                                prev.posterior <- guess.posterior
                                ar <- ar + 1
                        }

                        ## Sample Cw
                        guess.Cw <- rtnorm(1, Cw.i, JumpSD["Cw"])
                        guess.error <- try.error(N.i, Cab.i, guess.Cw, Cm.i)
                        guess.posterior <- try.posterior(N.i, Cab.i, guess.Cw, Cm.i, pwl.i, guess.error)

                        jnum <- dtnorm(guess.Cw, Cw.i, JumpSD["Cw"])
                        jden <- dtnorm(Cw.i, guess.Cw, JumpSD["Cw"])
                        a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

                        if(is.na(a)) a <- -1
                        if(a > runif(1)){
                                Cw.i <- guess.Cw
                                prev.error <- guess.error
                                prev.posterior <- guess.posterior
                                ar <- ar + 1
                        }

                        ## Sample Cm
                        guess.Cm <- rtnorm(1, Cm.i, JumpSD["Cm"])
                        guess.error <- try.error(N.i, Cab.i, Cw.i, guess.Cm)
                        guess.posterior <- try.posterior(N.i, Cab.i, Cw.i, guess.Cm, pwl.i, guess.error)

                        jnum <- dtnorm(guess.Cm, Cm.i, JumpSD["Cm"])
                        jden <- dtnorm(Cm.i, guess.Cm, JumpSD["Cm"])
                        a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

                        if(is.na(a)) a <- -1
                        if(a > runif(1)){
                                Cm.i <- guess.Cm
                                prev.error <- guess.error
                                prev.posterior <- guess.posterior
                                ar <- ar + 1
                        }
                }

                ### Sample error precision ### 
                if(single.precision){
                        nprec <- 1
                        u1p <- nspec*nwl/2
                        u2p <- 0.5 * sum(prev.error^2)
                } else {
                        nprec <- nwl
                        u1p <- nspec/2
                        u2p <- 0.5 * apply(prev.error^2, 1, sum)
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
                } else {
                        write(c(N.i, Cab.i, Cw.i, Cm.i, pwl.i), 
                              ncolumns=length(header),
                              sep=",",
                              file=fname,
                              append=TRUE)
                }
        }

        if (local.store){
                return(list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store, arate=ar/ngibbs))
        }
}

