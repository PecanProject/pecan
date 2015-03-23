## Quick test
library(PEcAnRTM)
data(testspec)
ngibbs = 1000
tt <- system.time(z <- invert_prospect(testspec_ACRU[,1], ngibbs))
print(tt)

## Burnin and thin
Thin = 2
bt <- seq(ngibbs/2, ngibbs, by=Thin)
z <- z2[bt,]

par(mfrow=c(3,2))
plot(z[,1], type='l')
plot(z[,2], type='l')
plot(z[,3], type='l')
plot(z[,4], type='l')
plot(z[,5], type='l')
plot(prospect4(mean(z[,1]), mean(z[,2]), mean(z[,3]), mean(z[,4])), type='l')
lines(testspec_ACRU[,1], col=2)
