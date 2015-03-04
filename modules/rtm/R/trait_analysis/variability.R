### Variability
library(rjags)
library(data.table)
load("../data/FFT_full.Rdata")

fft.spec <- fftdat[!is.na(Spectra)]

niter <- 5000
nchains <- 5
burnin <- 5000
thin <- 5

vcode <- "
model{
        ### Hierarchical random-effect, global mean
        for(i in 1:n.all){
                Ex[i] <- mu +
                        alpha.site[site[i]] +
                        alpha.plot[plot[i]] +
                        alpha.species[species[i]] +
                        alpha.height[height[i]]
                Y[i] ~ dnorm(Ex[i], y.s[i])
        }

#         ### Observation error
#         for(i in 1:n.all){y.m[i] ~ dlnorm(Y[i], tau)}

        ### Priors
        mu ~ dnorm(0, 0.01)
        tau ~ dgamma(0.01, 0.01)
        tau.site ~ dgamma(0.01, 0.01)
        tau.plot ~ dgamma(0.01, 0.01)
        tau.species ~ dgamma(0.01, 0.01)
        tau.height ~ dgamma(0.01, 0.01)
        for(i in 1:n.site){alpha.site[i] ~ dnorm(0, tau.site)}
        for(i in 1:n.plot){alpha.plot[i] ~ dnorm(0, tau.plot)}
        for(i in 1:n.species){alpha.species[i] ~ dnorm(0, tau.species)}
        for(i in 1:n.height){alpha.height[i] ~ dnorm(0, tau.height)}
}
"

vdata <- list(y.m = fft.spec[,Cm.m],
              y.s = fft.spec[,1/(Cm.sd)^2],
              n.all = nrow(fft.spec),
              site = as.numeric(as.factor(fft.spec[,Site])),
              plot = as.numeric(as.factor(fft.spec[,Plot])),
              species = as.numeric(as.factor(fft.spec[,Species])),
              height = as.numeric(as.factor(fft.spec[,Height])),
              n.site = nrow(fft.spec[,.N, by=Site]),
              n.plot = nrow(fft.spec[,.N, by=Plot]),
              n.species = nrow(fft.spec[,.N, by=Species]),
              n.height = nrow(fft.spec[,.N, by=Height])
)

vmodel <- jags.model(file = textConnection(vcode),
                     data = vdata,
                     n.chains = nchains)

update(vmodel, burnin)
monitors <- c("mu", "tau", "tau.site", "tau.plot", "tau.species", "tau.height")
vsamples <- coda.samples(vmodel, monitors, niter, thin)

plot(vsamples)
