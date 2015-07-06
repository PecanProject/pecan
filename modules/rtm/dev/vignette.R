## PEcAN RTM Package Vignette
## Author: Alexey Shiklomanov

library(PEcAnRTM)
wl <- 400:2500
params <- c("N"=1.4, "Cab"=40, "Car"=15,
            "Cbrown"=0.5, "Cw"=0.002, "Cm"=0.004)

p4 <- prospect(params[c(-3,-4)], version=4)
p5 <- prospect(params[-4], version=5)
p5b <- prospect(params, version="5B")

plot(wl, p4[,1], type='l', xlab="Wavelength (nm)", ylab="Value", ylim=c(0,1))
lines(wl, 1-p4[,2], col=2)
lines(wl, p5[,1], lty=2, col=1)
lines(wl, 1-p5[,2], lty=2, col=2)
lines(wl, p5b[,1], lty=3, col=1)
lines(wl, 1-p5b[,2], lty=3, col=2)
legend("topright", c("Reflectance", "Transmittance"), col=c(1,2), lty=1)
legend("top", c("4", "5", "5B"), lty = c(1,2,3))

sail.params <- defparam("pro4sail")
print(sail.params)
p4s <- pro4sail(sail.params)
matplot(x = wl, y = p4s, type='l', xlab="Wavelength (nm)", ylab="Reflectance")
legend("topright", as.character(1:4), col=1:4, lty=1:4)

data(testspec)
matplot(wl, testspec_ACRU[,1:3], 
        xlab="Wavelength", ylab="Reflectance", type='l')

# Example priors
p5.means <- defparam("prospect_5")
p5.sd <- p5.means * 4
print(p5.means)
print(p5.sd)
p5mu <- log(p5.means / sqrt(1 + p5.sd^2 / p5.means^2))
p5sigma <- sqrt(log(1 + p5.sd^2 / p5.means^2))
print(p5mu)
print(p5sigma)

# Use lognormal prior for all parameters
p5log <- rep(TRUE, 5)

# Minimum values
p5min <- c("N"=1, "Cab"=0, "Car"=0, "Car"=0, "Cw"=0, "Cm"=0)

p5inv <- invert.fast(modname = "prospect_5",
                     observed = testspec_ACRU[,1:3],
                     inits = p5.means,
                     cons = NULL,
                     pmu = p5mu,
                     psd = p5sigma,
                     plog = p5log,
                     minp = p5min,
                     ngibbs = 1000)

par(mfrow=c(3,2))
for(i in 1:6){
    param <- colnames(p5inv)[i]
    plot(p5inv[,i], type='l', main=param)
    p5.burnin <- p5inv[-500:0,i]
    plot(density(p5.burnin), main=param)
    m <- mean(p5.burnin)
    s <- sd(p5.burnin)
    abline(v=m)
    abline(v=m-s, col=2, lty=2)
    abline(v=m+s, col=2, lty=2)
}

print(colMeans(p5inv[-500:0,]))
print(apply(p5inv[-500:0,], 2, sd))

### Random effects
p5rand <- matrix(0, 5, 3)
rownames(p5rand) <- names(p5.means)
p5re <- invert.fast.re(modname = "prospect_5b",
                       observed = testspec_ACRU[,1:3],
                       inits = p5.means,
                       cons = c("Cbrown"=0),
                       rand = p5rand,
                       pmu = p5mu,
                       psd = p5sigma,
                       plog = p5log,
                       minp = p5min,
                       ngibbs = 100)

print(colMeans(p5re[-50:0,]))
print(apply(p5re[-50:0,], 2, sd))

# Reinstall package
#detach("package:PEcAnRTM", unload=TRUE)
#install.packages("..", type='source', repos=NULL)
#library(PEcAnRTM)
