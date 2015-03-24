## Quick test
library(PEcAnRTM)
data(testspec)
ngibbs = 50
tt <- system.time(z2 <- invert_prospect_re(testspec_ACRU[,1:5], ngibbs, 10))
print(tt)

## Burnin and thin
Thin = 2
bt <- seq(ngibbs/2, ngibbs, by=Thin)
z <- z2[bt,]

par(mfrow=c(3,2))
plot(z2[,1], type='l')
plot(z2[,10], type='l')
plot(z2[,14], type='l')
plot(z2[,18], type='l')
plot(z2[,22], type='l')
lines(testspec_ACRU[,1], col=2)
