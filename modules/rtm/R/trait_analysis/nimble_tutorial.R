## Nimble tutorial - verbatim from user guide
library(nimble)
library(igraph)

pumpCode <- nimbleCode({
        for (i in 1:N){
                theta[i] ~ dgamma(alpha, beta)
                lambda[i] <- theta[i] * t[i]
                x[i] ~ dpois(lambda[i])
        }
        alpha ~ dexp(1.0)
        beta ~ dgamma(0.1, 1.0)
})

pumpConsts <- list(N = 10,
                   t = c(94.3, 15.7, 62.9, 126, 5.24,
                         31.4, 1.05, 1.05, 2.1, 10.5))
pumpData <- list(x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22))
pumpInits <- list(alpha = 1,
                  beta = 1,
                  theta = rep(0.1, pumpConsts$N))

pump <- nimbleModel(code = pumpCode,
                    name = 'pump',
                    constants = pumpConsts,
                    data = pumpData)

Cpump <- compileNimble(pump)
pumpSpec <- configureMCMC(pump, print=TRUE)
pumpSpec$addMonitors(c("alpha", "beta", "theta"))

pumpMCMC <- buildMCMC(pumpSpec)
CpumpMCMC <- compileNimble(pumpMCMC, project = pump)

n.iter <- 1000
CpumpMCMC$run(n.iter)

samples <- as.matrix(CpumpMCMC$mvSamples)

plot(samples[ , 'alpha'], type='l', xlab='', ylab=expression(alpha))


