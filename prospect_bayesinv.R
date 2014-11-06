#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")

library(coda)

pinvbayes <- function(obs.spec, prospect=prospect4,
                      ngibbs=100,
                      initc=c(1, 0.01, 0.01, 0.01)
                      )) {
  wl <- min(obs.spec$Wavelength):max(obs.spec$Wavelength)
  
  ### Priors
  N.s <- c(1.5, 0.5)    # Gamma
  Cab.s <- c()
  
  ### MCMC.storage
  pp.store <- matrix(NA, nrow=ngibbs, ncol=4)
  tstart <- proc.time()
  for(g in 1:ngibbs){
    ## Loop timer
    if((g == 5) | (g %% (ngibbs/20) == 0)){
      tlap <- proc.time()
      tdif <- (tlap - tstart)[[3]]
      tleft <- tdif * (ngibbs/g - 1)
      cat(sprintf("\r%d of %d. Time elapsed: %.1f. Time remaining: %.1f", g, ngibbs, tdif, tleft))
    }
    
    ## Try parameters
    guess.N <- rgamma()
    
  }
}