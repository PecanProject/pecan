## Quick test
library(PEcAnRTM)
data(testspec)
ngibbs = 1000
tt <- system.time(z2 <- invert_prospect(testspec_ACRU[,1], ngibbs))
print(tt)

## Burnin and thin
Thin = 2
bt <- seq(ngibbs/2, ngibbs, by=Thin)
z <- z2[bt,]

par(mfrow=c(3,2))
plot(z2[,1], type='l')
plot(z2[,2], type='l')
plot(z2[,3], type='l')
plot(z2[,4], type='l')
plot(z2[,5], type='l')
plot(prospect(4, colMeans(z[,1:4])), type='l')
lines(testspec_ACRU[,1], col=2)
