### Variability
library(rjags)
library(data.table)
load("../data/FFT_full.Rdata")

fft.spec <- fftdat[!is.na(Spectra) & !is.na(PFT)]
design.reg <- model.matrix(N.mul ~ Height + PFT, data=fft.spec)

burnin <- 5000
nchains <- 1
thin <- 1
niter <- 5000 * thin / nchains

vcode <- "
model{
        ### Hierarchical random-effect, global mean
        for(i in 1:n.all){
                Ex[i] <- X[i,] %*% Beta
                Y[i] ~ dnorm(Ex[i], 1/y.s[i]^2)
                y.m[i] ~ dnorm(Y[i], tau)
        }

        ### Priors
        mu ~ dnorm(0, 0.01)
        for(i in 1:nfe){Beta[i] ~ dnorm(0, 0.01)}
        tau ~ dgamma(0.01, 0.01)
}
"

vdata <- list(X = design.reg,
               nfe = ncol(design.reg),
               n.all = nrow(fft.spec))
vdata$y.m <- fft.spec[,N.mul]
vdata$y.s <- fft.spec[,N.sdl]

vmodel <- jags.model(file = textConnection(vcode),
                     data = vdata,
                     n.chains = nchains)
update(vmodel, burnin)
monitors <- c("mu", "Beta",
              "tau", "tau.site", "tau.plot", "tau.species")
z <- system.time(vsamples <- coda.samples(vmodel, monitors, niter, thin))
