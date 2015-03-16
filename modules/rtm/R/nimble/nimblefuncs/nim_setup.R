### Prospect inversion - NIMBLE setup
library(nimble)
load("../data/prospect4.Rdata")

prospectCode <- nimbleCode({
        ### Priors
        Ni ~ dlnorm(-0.916, 2.2)
        N <- Ni + 1
        Cab ~ dlnorm(3.4, 0.9)
        Cw ~ dlnorm(-6.377, 0.5)
        Cm ~ dlnorm(-5.116, 0.9)
        resp ~ dgamma(0.001, 0.001)
})

prospectConstants <- list(Cab_abs = prospect4.dat$Cab_abs,
                          Cw_abs = prospect4.dat$Cw_abs,
                          Cm_abs = prospect4.dat$Cm_abs,
                          tao1 = prospect4.dat$tao1,
                          tao2 = prospect4.dat$tao2,
                          rho1 = prospect4.dat$rho1,
                          rho2 = prospect4.dat$rho2,
                          x = prospect4.dat$x,
                          y = prospect4.dat$y,
                          e1 = expint_coefs$e1,
                          e2 = expint_coefs$e2)

prospectInits <- list(N = 1.4,
                      Cab = 30,
                      Cw = 0.01,
                      Cm = 0.006,
                      resp = 0.5)
