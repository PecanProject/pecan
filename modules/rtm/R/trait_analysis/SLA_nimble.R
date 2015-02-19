### Analysis of SLA using Nimble
library(nimble)
library(igraph)

dat <- read.csv("N_SLA.csv")

hpCode <- nimbleCode({
        ## Priors
        a ~ dnorm(0.1, 0.01)
        b ~ dnorm(0.025, 0.01)
        c ~ dnorm(1, 0.01)
        d ~ dnorm(0.9, 0.01)
        resp ~ dgamma(0.01, 0.01)
        resv <- 1/resp

        ## Model
        for(i in 1:n){
                mu[i] <- (a * N[i] + b) / (c * N[i] + d)
                SLA[i] ~ dnorm(mu[i], resp)
                pi[i] ~ dnorm(mu[i], resp)
        }
})

hpConsts <- list(n = length(dat$N), N = dat$N)

hpData <- list(SLA = dat$SLA)

hpInits <- list(a = 0.1,
                b = 0.025,
                c = 1,
                d = 0.9)

hp <- nimbleModel(code = hpCode,
                  name = 'hp',
                  constants = hpConsts,
                  data = hpData,
                  inits = hpInits)

Chp <- compileNimble(hp)
hpSpec <- configureMCMC(hp, print=TRUE)
hpSpec$addMonitors(c("a", "b", "c", "d", "mu", "pi", "resv"))

hpMCMC <- buildMCMC(hpSpec, project = hp)
ChpMCMC <- compileNimble(hpMCMC, project = hp)

n.iter <- 10000
print("Starting run...")
ChpMCMC$run(n.iter)
print("Finished run! Post processing.")

samples <- as.matrix(ChpMCMC$mvSamples)

plot(samples[,'a'], type='l')
