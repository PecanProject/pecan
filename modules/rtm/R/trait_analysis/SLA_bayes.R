##' Bayesian analysis of SLA relationship
library(nimble)
library(ggplot2)
source("trait_load.R")

dat <- load.data()[,c("N","SLA_cm2_g_DW")]
dat <- dat[complete.cases(dat),]
dat$SLA <- dat$SLA_cm2_g_DW / 1000
dat$SLA_cm2_g_DW <- NULL

### Hyperbolic
hpCode <- " model{
        ## Model
        for(i in 1:n){
                mu[i] <- (a*N[i] + b) / (c*N[i] + d)
                SLA[i] ~ dnorm(mu[i], resp)
                pi[i] ~ dnorm(mu[i], resp)
        }

        ## Priors
        a ~ dnorm(0.1, 0.01)
        b ~ dnorm(0.025, 0.01)
        c ~ dnorm(1, 0.01)
        d ~ dnorm(0.9, 0.01)
        resp ~ dgamma(0.01, 0.01)
        resv <- 1/resp
}
"
hpData <- list(N = dat$N, SLA = dat$SLA, n = length(dat$N))
hpInits <- list(a = 0.1, b = 0.025, c=1, d = 0.9)
hpModel <- jags.model(file=textConnection(hpCode), data=hpData,
                      inits=hpInits, n.chains=6)
update(hpModel, 1000)
hpSamples <- coda.samples(hpModel,
                          c("a", "b", "c", "d", "resv", "mu", "pi"),
                          100000,
                          thin = 100)
hp.summary <- summary(hpSamples)
ci.ind <- grep("mu", rownames(hp.summary$quantiles))
pi.ind <- grep("pi", rownames(hp.summary$quantiles))
hp.ci <- hp.summary$quantiles[ci.ind, c(1,3,5)]
hp.pi <- hp.summary$quantiles[pi.ind, c(1,5)]
hp.plot <- ggplot() +
        aes(x=dat$N) +
        geom_point(aes(y=dat$SLA)) +
        geom_line(aes(y=hp.ci[,1]), color="grey") +
        geom_line(aes(y=hp.ci[,2]), color="black") +
        geom_line(aes(y=hp.ci[,3]), color="grey") +
        geom_line(aes(y=hp.pi[,1]), color="red") +
        geom_line(aes(y=hp.pi[,2]), color="red") +
        xlab("N") + ylab("SLA (cm2/g)")

plot(hp.plot)
print(hp.summary$statistics[-c(ci.ind, pi.ind),])


### nth root inverse
niCode <- " model{
## Model
for(i in 1:n){
        mu[i] <- N[i]^b + a
        SLA[i] ~ dnorm(mu[i], resp)
        pi[i] ~ dnorm(mu[i], resp)
}

## Priors
a ~ dnorm(0.1, 0.01)
b ~ dnorm(4, 4)
resp ~ dgamma(0.01, 0.01)

}"
niData <- list(N = dat$N, SLA = dat$SLA, n = length(dat$N))
niInits <- list(a = 0.1, b = 4)
niModel <- jags.model(file = textConnection(niCode), data = niData,
                      inits = niInits, n.chains = 6)
update(niModel, 3000)
niSamples <- coda.samples(niModel, c("a", "b", "resp", "mu", "pi"),
                          100000, thin=100)

ni.summary <- summary(niSamples)
ci.ind <- grep("mu", rownames(ni.summary$quantiles))
pi.ind <- grep("pi", rownames(ni.summary$quantiles))
ni.ci <- ni.summary$quantiles[ci.ind, c(1,3,5)]
ni.pi <- ni.summary$quantiles[pi.ind, c(1,5)]

hpni.plot <- ggplot() +
        aes(x=dat$N) +
        geom_point(aes(y=dat$SLA)) +
        geom_line(aes(y=hp.ci[,1]), color="grey") +
        geom_line(aes(y=hp.ci[,2]), color="black") +
        geom_line(aes(y=hp.ci[,3]), color="grey") +
        geom_line(aes(y=hp.pi[,1]), color="red") +
        geom_line(aes(y=hp.pi[,2]), color="red") +
        geom_line(aes(y=ni.ci[,1]), color="lightblue") +
        geom_line(aes(y=ni.ci[,2]), color="blue") +
        geom_line(aes(y=ni.ci[,3]), color="lightblue") +
        geom_line(aes(y=ni.pi[,1]), color="purple") +
        geom_line(aes(y=ni.pi[,2]), color="purple") +
        xlab("N") + ylab("SLA (cm2/g)")

plot(hpni.plot)
print(ni.summary$statistics[-c(ci.ind, pi.ind),])

