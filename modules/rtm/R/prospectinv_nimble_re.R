### PROSPECT code and inversion, implemented in Nimble
library(nimble)
load("../data/prospect4.Rdata")

### Initial setup
n.iter <- 1
AI <- 3

### Bayesian inversion
prospectCode <- nimbleCode({
        ### Priors
        # Global mean PROSPECT parameters
        Ni ~ dlnorm(-0.916, 2.2)
        N.m <- Ni + 1
        Cab.m ~ dlnorm(3.4, 0.9)
        Cw.m ~ dlnorm(-6.377, 0.5)
        Cm.m ~ dlnorm(-5.116, 0.9)

        # Random effects
        for(i in 1:nspec){
                sp.N[i] ~ dnorm(0, sp.N.t)
                sp.Cab[i] ~ dnorm(0, sp.Cab.t)
                sp.Cw[i] ~ dnorm(0, sp.Cw.t)
                sp.Cm[i] ~ dnorm(0, sp.Cm.t)
        }
        sp.N.t ~ dgamma(0.1, 0.1)
        sp.Cab.t ~ dgamma(0.1, 0.1)
        sp.Cw.t ~ dgamma(0.1, 0.1)
        sp.Cm.t ~ dgamma(0.1, 0.1)

        # Residual
        resp ~ dgamma(0.001, 0.001)

        ### Model variables
        for (sp in 1:nspec){
                N[sp] <- N.m + sp.N[sp]
                if(N[sp] < 1.001){
                        N[sp] <- 1.001
                }
                Cab[sp] <- Cab.m + sp.Cab[sp]
                Cw[sp] <- Cw.m + sp.Cw[sp]
                Cm[sp] <- Cm.m + sp.Cm[sp]
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
                          e2 = expint_coefs$e2,
                          observed = obs.spec,
                          nspec = ncol(obs.spec),
                          wl = nrow(obs.spec))

prospectInits <- list(N.m = 1.4,
                      Cab.m = 30,
                      Cw.m = 0.01,
                      Cm.m = 0.006,
                      resp = 0.5,
                      sp.N.t = 0.1,
                      sp.Cab.t = 3,
                      sp.Cw.t = 0.001,
                      sp.Cm.t = 0.0006)

prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants,
                        inits = prospectInits)
prospectNodes <- prospect$getNodeNames()

prospectSpec <- configureMCMC(prospect,
                              nodes = prospectNodes[grep("*\\.t", prospectNodes)],
                              print=TRUE)

samplers.to.add <- prospectNodes[grep("(Ni)|(Cab\\.m)|(Cw\\.m)|(Cm\\.m)|\
                                 (sp\\.N\\[)|(sp\\.Cab\\[)|(sp\\.Cw\\[)|(sp\\.Cm\\[)",
                                      prospectNodes)]

### Load custom samplers
source("nimblefuncs/LL_mod_re.R")
source("nimblefuncs/resp_long_re.R")

specialized_prospect_LL <- prospect_LL(prospect, prospectConstants)

for (s in samplers.to.add){
        prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = s,
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))
}

prospectSpec$addSampler(type = "resp", control = list(targetNode = "resp"))

prospectSpec$addMonitors(c("N.m", "Cab.m", "Cw.m", "Cm.m", "resp", "sp.N[1]", "sp.N[2]"))
prospectMCMC <- buildMCMC(prospectSpec, project = prospect)

### Compilation
print("Begin compilation")
prosProj <- compileNimble(prospect, prospectMCMC)
print("Compilation complete!")

### Run MCMC
print("Begin MCMC...")
sw <- system.time(prosProj$prospectMCMC$run(n.iter))
print("MCMC complete!")
print(sw)

### Check output
samples1 <- as.matrix(prosProj$prospectMCMC$mvSamples)
print(samples1)
par(mfrow=c(3,2))
plot(samples1[,"N.m"], type='l')
plot(samples1[,"Cab.m"], type='l')
plot(samples1[,"Cw.m"], type='l')
plot(samples1[,"Cm.m"], type='l')
plot(samples1[,"resp"], type='l')
