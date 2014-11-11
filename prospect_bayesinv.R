#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.r")
source("truncnorm.R")

library(coda)
library(mvtnorm)

pinvbayes <- function(obs.spec, prospect=prospect4,
                      ngibbs=100,
                      initc=list(N=1,
                                 Cab=30,
                                 Cw=0.0001,
                                 Cm=0.001,
                                 pwl = rep(0.01, 2101)),
                                 JumpSD=c(1, 15, 0.001, 0.001)
                                 ) {
  wl <- min(obs.spec$Wavelength):max(obs.spec$Wavelength)
  
  ### Initial values unpacked
  N.i <- initc[["N"]]
  Cab.i <- initc[["Cab"]]
  Cw.i <- initc[["Cw"]]
  Cm.i <- initc[["Cm"]]
  pwl.i <- initc[["pwl"]]
  
  ### Priors
  # Based on LOPEX93 data in Shawn's code
  N.s <- c(1, 4)        # Weibull (N = 1 +rweibull)
  Cab.s <- c(1.8, 70)      # Weibull
  Cw.s <- c(2, 0.02)       # Weibull
  Cm.s <- c(2, 0.012)     # Weibull
  
  pwl.s <- c(0.001, 0.001)  # Inverse gamma
  
  
  ### MCMC.storage
  N.store <- numeric(ngibbs)
  Cab.store <- numeric(ngibbs)
  Cw.store <- numeric(ngibbs)
  Cm.store <- numeric(ngibbs)
  pwl.store <- matrix(NA, nrow=ngibbs, ncol=2101)
  
  ## MCMC.guess
  tstart <- proc.time()
  for(g in 1:ngibbs){
    if((g == 5) | (g %% (ngibbs/20) == 0)) laptime(tstart, g, ngibbs)   

    ## Sample PROSPECT parameters
    guess.N <- rtnorm(1, N.i, JumpSD[1], Min=1)
    guess.Cab <- rtnorm(1, Cab.i, JumpSD[2])
    guess.Cw <- rtnorm(1, Cw.i, JumpSD[3])
    guess.Cm <- rtnorm(1, Cm.i, JumpSD[4])
    
    
    guess.spec <- prospect(guess.N, guess.Cab, guess.Cw, guess.Cm)
    guess.error <- guess.spec$R - obs.spec$Reflectance
    
    guess.pwl <- rnorm(2101, pwl.s[1], pwl.s[2])
    guess.posterior <- dmvnorm(guess.error, rep(0, 2101), diag(guess.pwl), log=TRUE) +
      dweibull(1 - guess.N, N.s[1], N.s[2], log=TRUE)+
      dweibull(guess.Cab, Cab.s[1], Cab.s[2], log=TRUE) +
      dweibull(guess.Cw, Cw.s[1], Cw.s[2], log=TRUE) +
      dweibull(guess.Cm, Cm.s[1], Cm.s[2], log=TRUE) +
      sum(mapply(dgamma, guess.pwl, rep(pwl.s[1], 2101), rep(pwl.s[2], 2101), MoreArgs = list(log=TRUE)))
    
    jnum <- dtnorm(guess.N, N.i, JumpSD[1], Min=1) +
      dtnorm(guess.Cab, Cab.i, JumpSD[2]) +
      dtnorm(guess.Cw, Cw.i, JumpSD[3]) +
      dtnorm(guess.Cm, Cm.i, JumpSD[4])
    
    prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
    prev.error <- guess.spec$R - obs.spec$Reflectance
    prev.posterior <- dmvnorm(prev.error, rep(0, 2101), diag(pwl.i), log=TRUE) +
      dweibull(1 - N.i, N.s[1], N.s[2], log=TRUE)+
      dweibull(Cab.i, Cab.s[1], Cab.s[2], log=TRUE) +
      dweibull(Cw.i, Cw.s[1], Cw.s[2], log=TRUE) +
      dweibull(Cm.i, Cm.s[1], Cm.s[2], log=TRUE) +
      sum(mapply(dgamma, pwl.i, rep(pwl.s[1], 2101), rep(pwl.s[2], 2101), MoreArgs = list(log=TRUE)))
    
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
      pwl.i <- guess.pwl
    }
  N.store[g] <- N.i
  Cab.store[g] <- Cab.i
  Cw.store[g] <- Cw.i
  Cm.store[g] <- Cm.i
  pwl.store[g,] <- pwl.i
  
  }
  
  return(list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store))
}