## Quick test
library(PEcAnRTM)
data(testspec)
ngibbs = 100
tt <- system.time(z2 <- invert_prospect_re(testspec_ACRU[,1:5], ngibbs, 10))
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
plot(prospect4(mean(z[,1]), mean(z[,2]), mean(z[,3]), mean(z[,4])), type='l')
lines(testspec_ACRU[,1], col=2)
