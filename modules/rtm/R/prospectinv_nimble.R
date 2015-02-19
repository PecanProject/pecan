### PROSPECT code and inversion, implemented in Nimble
library(nimble)

### Initial setup
n.iter <- 10
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
        resv <- 1/resp
})

prospectConstants <- list(nr = dat.p[,2],
                          Cab_abs = dat.p[,3],
                          Cw_abs = dat.p[,5],
                          Cm_abs = dat.p[,6],
                          observed = obs.spec,
                          nspec = ncol(obs.spec))

prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants)

prospectSpec <- configureMCMC(prospect, print=TRUE)
prospectSpec$removeSamplers(1:5)

### Load custom samplers
source("nimblefuncs/specerror.R")
source("nimblefuncs/LL.R")
source("nimblefuncs/resp_sampler.R")

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
prospectSpec$addSampler(type = "resp", control = list(targetNode = "resp"))

# prospectSpec$addSampler(type = "RW_llFunction",
#                         control = list(targetNode = "resp",
#                                        llFunction = specialized_prospect_LL,
#                                        includesTarget = FALSE))

prospectSpec$addMonitors(c("N", "Cab", "Cw", "Cm", "resv"))
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
samples1 <- as.matrix(prospectMCMC$mvSamples)
par(mfrow=c(3,2))
plot(samples1[,"N"], type='l')
plot(samples1[,"Cab"], type='l')
plot(samples1[,"Cw"], type='l')
plot(samples1[,"Cm"], type='l')
plot(samples1[,"resp"], type='l')
