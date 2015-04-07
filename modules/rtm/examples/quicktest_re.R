## Quick test
library(PEcAnRTM)
data(testspec)
ngibbs = 500
tt <- system.time(z2 <- invert_prospect_re(testspec_ACRU[,1:5], ngibbs, 10))
print(tt)

## Burnin and thin
Thin = 2
bt <- seq(ngibbs/2, ngibbs, by=Thin)
z <- z2[bt,]

plt <- c(1:6,8:13, 15:20, 22:27)
par(mfrow=c(3,2))
for(p in plt){plot(z2[,p], type='l')}
