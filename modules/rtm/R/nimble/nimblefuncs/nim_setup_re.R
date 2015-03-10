### Prospect inversion - NIMBLE setup
library(nimble)
load("../data/prospect4.Rdata")

prospectCode <- nimbleCode({
        mu.N ~ dnorm(-0.916, 2.2)
        mu.Cab ~ dnorm(3.4, 0.9)
        mu.Cw ~ dnorm(-6.377, 0.5)
        mu.Cm ~ dnorm(-5.116, 0.9)

        resp ~ dgamma(0.001, 0.001)
        ressd <- 1/sqrt(resp)

        tinv.N ~ dgamma(0.001, 0.001)
        tau.N <- 1/sqrt(tinv.N)
        tinv.Cab ~ dgamma(0.001, 0.001)
        tau.Cab <- 1/sqrt(tinv.Cab)
        tinv.Cw ~ dgamma(0.001, 0.001)
        tau.Cw <- 1/sqrt(tinv.Cw)
        tinv.Cm ~ dgamma(0.001, 0.001)
        tau.Cm <- 1/sqrt(tinv.Cm)
        
        for(i in 1:nspec) {
                alpha.N[i] ~ dnorm(0, tinv.N)
                alpha.Cab[i] ~ dnorm(0, tinv.Cab)
                alpha.Cw[i] ~ dnorm(0, tinv.Cw)
                alpha.Cm[i] ~ dnorm(0, tinv.Cm)

                N[i] <- 1 + exp(mu.N + alpha.N[i])
                Cab[i] <- exp(mu.Cab + alpha.Cab[i])
                Cw[i] <- exp(mu.Cw + alpha.Cw[i])
                Cm[i] <- exp(mu.Cm + alpha.Cm[i])
        }
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
                      resp = 0.5,
                      tinv.N = 1,
                      tinv.Cab = 1,
                      tinv.Cw = 1,
                      tinv.Cm = 1)
