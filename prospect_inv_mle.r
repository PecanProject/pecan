###'@title Prospect Model Inversion
###'@author Alexey Shiklomanov
###'@description PROSPECT model inversion

source("prospect.R")

# Some test data
beech <- read.table("data/beech.txt", header=TRUE, quote="\"")
clover <- read.table("data/clover.txt", header=TRUE, quote="\"")
corn <- read.table("data/corn.txt", header=TRUE, quote="\"")
poplar <- read.table("data/poplar.txt", header=TRUE, quote="\"")

# Vector of initial conditions based on min/max Shawn's LOPEX 93 data

ic1 <- unlist(testdata[1, 2:5])   # min f
ic2 <- unlist(testdata[2, 2:5])   # max f
ic3 <- unlist(testdata[7, 2:5])   # min d
ic4 <- unlist(testdata[8, 2:5])   # max d

# Minimum and maximum values from Shawn's LOPEX 93 data (for DEoptim)
mins <- as.numeric(unlist(apply(testdata, 2, min)[2:5]))
maxs <- as.numeric(unlist(apply(testdata, 2, max)[2:5]))

# Merit function based on sum-of-squares difference
ssd <- function(P, ref.obs, func=prospect4){
  prospect <- do.call(func, as.list(P))
  ref.mod <- prospect$R
  ssd <- min(log(sum((ref.mod - ref.obs)^2)), 1e10)
  return(ssd)
}

p.invert <- function(observed, func=prospect4, inits=ic1){
  a1 <- optim(inits, ssd, ref.obs=observed$Refl, func=func)
  p <- do.call(func, as.list(a1$par))
  d <- p$R - observed$R
  plot(p$wavelength, p$R, type='l')
  lines(observed$Wavelength, observed$R, col=2)
  plot(p$wavelength, d, type='l')
  abline(h=0)
  return(a1)
}

