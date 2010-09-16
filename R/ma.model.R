## Bayesian Meta-analysis of plant traits
ma.model <- function(){
  for (k in 1:LENGTHK){
    Y[k] ~ dnorm( Z[k] , tau.y[k])              # observed site x trt means and uncertainties
    Z[k] <- beta.o + b.trt[trt[k]] + b.site[site[k]]     # linear model with random treatment and site effects
    tau.y[k] <- prec.y*n[k]                     # precision from obs.prec
    u1[k] <- n[k]/2                             
    u2[k] <- n[k]/(2*prec.y)
    obs.prec[k] ~ dgamma(u1[k], u2[k])          # observed precision
  }
  b.trt[1] <- 0                                 # control treatment effect = 0
  for ( j in 2:LENGTHJ){
    b.trt[j] ~ dnorm(0, tau.trt)                # treatment effects, random
  }
  for (g in 1:LENGTHG){
    b.site[g] ~ dnorm(0, tau.site)         # site effects, random
  }
  beta.o    ~ PRIORDIST (PRIORPARAMA, PRIORPARAMB) # informed prior on global mean
  tau.site  ~ dgamma(0.1, 0.1)                  # global precision
  tau.trt   ~ dgamma(0.1, 0.1)                  # treatment effect precision
  prec.y    ~ dgamma(0.1, 0.1)                  # observation Y precision
  thetaSD  <- 1 / sqrt(tau.site)                # across site variance
  ySD      <- 1 / sqrt(prec.y)                  # within site variance
  trtSD    <- 1 / sqrt(tau.trt)                 # within trt variance
}
 
