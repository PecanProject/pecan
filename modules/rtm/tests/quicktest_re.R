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
plot(z2[,11], type='l')
plot(z2[,15], type='l')
plot(z2[,19], type='l')
plot(z2[,23], type='l')
