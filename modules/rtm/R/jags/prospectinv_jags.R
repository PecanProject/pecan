### PROSPECT code and inversion, implemented in Nimble
library(rjags)
load("../data/prospect4.Rdata")
load("pinv_nimble_testdat.Rdata")

### Initial setup
n.iter <- 10

prospectData <- list(Cab_abs = prospect4.dat$Cab_abs,
                     Cw_abs = prospect4.dat$Cw_abs,
                     Cm_abs = prospect4.dat$Cm_abs,
                     tao1 = prospect4.dat$tao1,
                     tao2 = prospect4.dat$tao2,
                     rho1 = prospect4.dat$rho1,
                     rho2 = prospect4.dat$rho2,
                     x = prospect4.dat$x,
                     y = prospect4.dat$y,
                     e1 = expint_coefs$e1,
                     e2 = expint_coefs$e2,
                     observed = obs.spec,
                     nspec = ncol(obs.spec),
                     wl = nrow(obs.spec))

prospectInits <- list(Ni = 1.4-1,
                      Cab = 30,
                      Cw = 0.01,
                      Cm = 0.006,
                      resp = 0.5)

prospect <- jags.model(file = "jags/prospect.bug",
                       data = prospectData)

prospect.samples <- coda.samples(prospect,
                                 variable.names = c("N", "Cab", "Cw", "Cm", "resp"),
                                 n.iter = n.iter)

### Check output
plot(prospect.samples)