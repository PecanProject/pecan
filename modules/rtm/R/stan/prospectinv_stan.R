### RStan implementation of PROSPECT
library(rstan)
load("stan/p4dat.Rdata")

p4.dat$observed <- obs.spec

p4.c <- stanc(file="stan/prospect.stan", model_name = "prospect")

# data= p4.dat, iter=100, chains=1
