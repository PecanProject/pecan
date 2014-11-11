#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.r")
source("truncnorm.R")

library(coda)
library(mvtnorm)

samp.inits <- list(N=1, 
                   Cab=30,
                   Cw=0.0001,
                   Cm=0.001,
                   pwl=0.01)
pinvbayes <- function(obs.spec, prospect=prospect4, ngibbs=100,
                      initc=samp.inits,
                      JumpSD=c(0.3, 5, 0.001, 0.001)) {
  wl <- min(obs.spec$Wavelength):max(obs.spec$Wavelength)
  
  ### Initial values unpacked
  N.i <- initc[["N"]]
  Cab.i <- initc[["Cab"]]
  Cw.i <- initc[["Cw"]]
  Cm.i <- initc[["Cm"]]
  pwl.i <- initc[["pwl"]]
  
  ### Priors
  N.s <- c(0.7, 1.5)                # Lognormal (N = 1 + rlnorm)
  
  # Based on histograms in Feret et al. 2008
  Cab.s <- c(log(30), 0.9)          # Lognormal
  Cw.s <- c(log(0.017), 0.5)        # Lognormal
  Cm.s <- c(log(0.006), 0.9)        # Lognormal
  
  pwl.s <- c(0.001, 0.001)          # Inverse gamma
  
  # Precalculate first model and posterior
  prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
  prev.error <- prev.spec$Reflectance - obs.spec$Reflectance
  prev.posterior <- sum(dnorm(prev.error, 0, 1/sqrt(pwl.i), log=TRUE)) +
    dlnorm(1 - N.i, N.s[1], N.s[2], log=TRUE)+
    dlnorm(Cab.i, Cab.s[1], Cab.s[2], log=TRUE) +
    dlnorm(Cw.i, Cw.s[1], Cw.s[2], log=TRUE) +
    dlnorm(Cm.i, Cm.s[1], Cm.s[2], log=TRUE)
  
  ### MCMC.storage
  N.store <- numeric(ngibbs)
  Cab.store <- numeric(ngibbs)
  Cw.store <- numeric(ngibbs)
  Cm.store <- numeric(ngibbs)
  pwl.store <- numeric(ngibbs)
  
  ## MCMC.guess
  tstart <- proc.time()
  for(g in 1:ngibbs){
    if((g == 5) | (g %% (ngibbs/20) == 0)) laptime(tstart, g, ngibbs)   

    ### Sample PROSPECT parameters ###
    
    ## Draw PROSPECT parameters
    guess.N <- rtnorm(1, N.i, JumpSD[1], Min=1)
    guess.Cab <- rtnorm(1, Cab.i, JumpSD[2])
    guess.Cw <- rtnorm(1, Cw.i, JumpSD[3])
    guess.Cm <- rtnorm(1, Cm.i, JumpSD[4])
    
    ## Calculate modeled spectra and residuals
    guess.spec <- prospect(guess.N, guess.Cab, guess.Cw, guess.Cm)
    guess.error <- guess.spec$R - obs.spec$Reflectance

    ## Evaluate posterior | PROSPECT
    guess.posterior <- sum(dnorm(guess.error, 0, 1/sqrt(pwl.i), log=TRUE)) +     # Likelihood
      dlnorm(1 - guess.N, N.s[1], N.s[2], log=TRUE)+     # N prior
      dlnorm(guess.Cab, Cab.s[1], Cab.s[2], log=TRUE) +  # Cab prior
      dlnorm(guess.Cw, Cw.s[1], Cw.s[2], log=TRUE) +     # Cw prior
      dlnorm(guess.Cm, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior

    ## Test acceptance w/ Jump Distribution
    jnum <- dtnorm(guess.N, N.i, JumpSD[1], Min=1) +
      dtnorm(guess.Cab, Cab.i, JumpSD[2]) +
      dtnorm(guess.Cw, Cw.i, JumpSD[3]) +
      dtnorm(guess.Cm, Cm.i, JumpSD[4])
    
    jden <- dtnorm(N.i, guess.N, JumpSD[1], Min=1) +
      dtnorm(Cab.i, guess.Cab, JumpSD[2]) +
      dtnorm(Cw.i, guess.Cw, JumpSD[3]) +
      dtnorm(Cm.i, guess.Cm, JumpSD[4])
    
    a <- exp((guess.posterior - jnum) - (prev.posterior - jden))
    if(is.na(a)) a <- -1
    if(a < runif(1)){
      N.i <- guess.N
      Cab.i <- guess.Cab
      Cw.i <- guess.Cw
      Cm.i <- guess.Cm
      prev.error <- guess.error
    }

    # Store PROSPECT parameters
    N.store[g] <- N.i
    Cab.store[g] <- Cab.i
    Cw.store[g] <- Cw.i
    Cm.store[g] <- Cm.i
    

    ### Sample error precision ### 
    u1 <- pwl.s[1] + length(prev.error)/2
    u2 <- pwl.s[2] + 0.5 * sum(prev.error^2)
    pwl.i <- rgamma(1, u1, u2)
     
    # Store error value
    pwl.store[g] <- pwl.i  
  }
  
  return(list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store))
}