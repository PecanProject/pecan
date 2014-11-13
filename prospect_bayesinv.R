#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.r")
source("truncnorm.R")

samp.inits <- list(N=1, 
                   Cab=30,
                   Cw=0.01,
                   Cm=0.005,
                   pwl=rep(0.01, 2101))

## NOTE: obs.spec must be a matrix as follows:
## Column 1 : Wavelengths (400:2500)
## Columns 2-n : Reflectance observations
pinvbayes <- function(obs.spec, prospect=prospect4, ngibbs=100,
                      initc=samp.inits,
                      JumpRSD=5e-4) {
  wl <- min(obs.spec[,1]):max(obs.spec[,1])
  nwl <- length(wl)
  nspec <- ncol(obs.spec)
  JumpSD <- JumpRSD * unlist(initc)[-5]
  
  ### Initial values unpacked
  N.i <- initc[["N"]]
  Cab.i <- initc[["Cab"]]
  Cw.i <- initc[["Cw"]]
  Cm.i <- initc[["Cm"]]
  pwl.i <- initc[["pwl"]]
  
  
  ### Priors
  N.s <- c(0, 1.5)                # Halfnormal (N = 1 + rlnorm)
  
  # Based on histograms in Feret et al. 2008
  Cab.s <- c(log(30), 0.9)          # Lognormal
  Cw.s <- c(log(0.017), 0.5)        # Lognormal
  Cm.s <- c(log(0.006), 0.9)        # Lognormal
  
  pwl.s <- c(0.001, 0.001)          # Inverse gamma
  
  # Precalculate first model and posterior
  prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
  prev.error <- -apply(obs.spec[,-1], 2, "-", prev.spec$Reflectance)
  
  pp1 <- sum(dnorm(prev.error, 0, 1/sqrt(pwl.i), log=TRUE))  # Likelihood
  pp2 <- dnorm(N.i - 1, N.s[1], N.s[2], log=TRUE) + log(2)    # N prior
  pp3 <- dlnorm(Cab.i, Cab.s[1], Cab.s[2], log=TRUE)    # Cab prior
  pp4 <- dlnorm(Cw.i, Cw.s[1], Cw.s[2], log=TRUE)     # Cw prior
  pp5 <- dlnorm(Cm.i, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior
  prev.posterior <- pp1 + pp2 + pp3 + pp4 + pp5
  
  ### MCMC storage
  N.store <- numeric(ngibbs)
  Cab.store <- numeric(ngibbs)
  Cw.store <- numeric(ngibbs)
  Cm.store <- numeric(ngibbs)
  pwl.store <- matrix(NA, nrow=ngibbs, ncol=nwl)
  
  ## MCMC loop
  tstart <- proc.time()
  ar <- 0
  for(g in 1:ngibbs){
    if((g == 5) | (g %% (ngibbs/20) == 0)){
      laptime(tstart, g, ngibbs)
      cat(sprintf("  AN: %d, AR: %.1f percent      ",
                  ar, ar/g * 100))
    }  

    ### Sample PROSPECT parameters ###
    
    ## Draw PROSPECT parameters
    guess.N <- rtnorm(1, N.i, JumpSD[1], Min=1)
    guess.Cab <- rtnorm(1, Cab.i, JumpSD[2])
    guess.Cw <- rtnorm(1, Cw.i, JumpSD[3])
    guess.Cm <- rtnorm(1, Cm.i, JumpSD[4])
    
    ## Calculate modeled spectra and residuals
    guess.spec <- prospect(guess.N, guess.Cab, guess.Cw, guess.Cm)
    guess.error <- -apply(obs.spec[,-1], 2, "-", guess.spec$Reflectance)

    ## Evaluate posterior | PROSPECT
    gp1 <- sum(dnorm(guess.error, 0, 1/sqrt(pwl.i), log=TRUE))  # Likelihood
    gp2 <- dlnorm(guess.N - 1, N.s[1], N.s[2], log=TRUE)    # N prior
    gp3 <- dlnorm(guess.Cab, Cab.s[1], Cab.s[2], log=TRUE)    # Cab prior
    gp4 <- dlnorm(guess.Cw, Cw.s[1], Cw.s[2], log=TRUE)     # Cw prior
    gp5 <- dlnorm(guess.Cm, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior
    guess.posterior <- gp1 + gp2 + gp3 + gp4 + gp5

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

    ## Optional value tests
    #print(c(guess.posterior, prev.posterior, jnum, jden, a))
    if(is.na(a)) a <- -1
    if(a > runif(1)){
      N.i <- guess.N
      Cab.i <- guess.Cab
      Cw.i <- guess.Cw
      Cm.i <- guess.Cm
      prev.posterior <- guess.posterior
      ar <- ar + 1
    }

    # Store PROSPECT parameters
    N.store[g] <- N.i
    Cab.store[g] <- Cab.i
    Cw.store[g] <- Cw.i
    Cm.store[g] <- Cm.i
    

    ### Sample error precision ### 
    u1 <- pwl.s[1] + nspec/2
    u2 <- pwl.s[2] + 0.5 * apply(prev.error^2, 1, sum)
    pwl.i <- rgamma(nwl, u1, u2)
     
    # Store error value
    pwl.store[g,] <- pwl.i  
  }
  
  return(list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store, arate=ar/ngibbs))
}
