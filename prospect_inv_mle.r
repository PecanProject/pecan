###'@title Prospect Model Inversion
###'@author Alexey Shiklomanov
###'@description PROSPECT model inversion

source("prospect.R")

# Merit function based on sum-of-squares difference
ssd <- function(P, ref.obs){
  prospect <- prospect4(P[1], P[2], P[3], P[4])
  ref.mod <- prospect$R
  ssd <- -sum((ref.mod - ref.obs)^2)
  return(ssd)
}