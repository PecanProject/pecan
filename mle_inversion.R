###'@title Prospect Model Inversion
###'@author Alexey Shiklomanov
###'@description PROSPECT model inversion

source("prospect.R")

# Merit function based on sum-of-squares difference
ssd <- function(P, ref.obs, func=prospect4){
  P <- as.list(P)
  prospect <- func(P$N, P$Cab, P$Cw, P$Cm, n.a, cab.a, w.a, m.a)
  ssd <- log(sum((prospect - ref.obs)^2))
  return(ssd)
}

p.invert <- function(observed, func=prospect4, inits=initv){
  observed <- rowMeans(observed)
  a1 <- optim(inits, ssd, ref.obs=observed, func=func)
return(a1$par)
}

initv <- unlist(guess.inits)[1:4]

