### PROSPECT code and inversion, implemented in Nimble
library(nimble)

### Initial setup
n.iter <- 100
AI <- 3

### Bayesian inversion
prospectCode <- nimbleCode({
        ### Priors
        Ni ~ dlnorm(-0.916, 2.2)
        N <- Ni + 1
        Cab ~ dlnorm(3.4, 0.9)
        Cw ~ dlnorm(-6.377, 0.5)
        Cm ~ dlnorm(-5.116, 0.9)
        resp ~ dgamma(0.001, 0.001)
})

prospectConstants <- list(nr = dat.p[,2],
                          Cab_abs = dat.p[,3],
                          Cw_abs = dat.p[,5],
                          Cm_abs = dat.p[,6],
                          observed = obs.spec,
                          nspec = ncol(obs.spec),
                          wl = nrow(obs.spec))

prospectInits <- list(N = 1.4,
                      Cab = 30,
                      Cw = 0.01,
                      Cm = 0.006,
                      resp = 0.5)

prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants,
                        inits = prospectInits)

### Load custom samplers
# source("nimblefuncs/reflectance.R")
# source("nimblefuncs/LL.R")
#source("nimblefuncs/resp_sampler.R")
source("nimblefuncs/LL_perry.R")

# tp <- prospect_refl(prospect, prospectConstants)
# plot(tp$run(), type='l')


prospectSpec <- configureMCMC(prospect, print=TRUE)
prospectSpec$removeSamplers(1:5)

specialized_prospect_LL <- prospect_LL(prospect, prospectConstants)

prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Ni",
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cab",
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cw",
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))
prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "Cm",
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))
#prospectSpec$addSampler(type = "resp", control = list(targetNode = "resp"))

prospectSpec$addSampler(type = "RW_llFunction",
                        control = list(targetNode = "resp",
                                       llFunction = specialized_prospect_LL,
                                       includesTarget = FALSE,
                                       adaptInterval = AI))

prospectSpec$addMonitors(c("N", "Cab", "Cw", "Cm", "resp"))
prospectMCMC <- buildMCMC(prospectSpec, project = prospect)

### Compilation
print("Begin compilation")
prosProj <- compileNimble(prospect, prospectMCMC)
print("Compilation complete!")

### Run MCMC
print("Begin MCMC...")
prosProj$prospectMCMC$run(n.iter)
print("MCMC complete!")

### Check output
samples1 <- as.matrix(prosProj$prospectMCMC$mvSamples)
print(samples1)
par(mfrow=c(3,2))
plot(samples1[,"N"], type='l')
plot(samples1[,"Cab"], type='l')
plot(samples1[,"Cw"], type='l')
plot(samples1[,"Cm"], type='l')
plot(samples1[,"resp"], type='l')
