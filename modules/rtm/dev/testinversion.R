#try({detach(package:PEcAnRTM, unload=TRUE)}, silent=TRUE)
#install.packages("..", repos=NULL, type="source")
library(PEcAnRTM)

r1 <- prospect(c(1.8, 45, 0.0017, 0.004), 4)
r2 <- prospect(c(1.7, 40, 0.002, 0.005), 4)
observed <- cbind(r1[,1], r2[,1])

modname <- "prospect_4"
inits <- c(1.4, 30, 0.001, 0.001)
names(inits) <- c("N", "Cab", "Cw", "Cm")
rand <- matrix(0, 4, 2)
rownames(rand) <- names(inits)
cons <- numeric(0)
pmu <- log(inits)
psd <- rep(1,4)
plog <- rep(TRUE,4)
minp <- c(1,0,0,0)
ngibbs <- 1000

results <- invert.fast.re(modname, observed, inits, rand, cons, 
                       pmu, psd, plog, minp, ngibbs)

s <- floor(ngibbs/2)
par(mfrow=c(4,2))
for(i in 5:12) plot(results[s:ngibbs,i], type='l')

print(colMeans(results[s:ngibbs,]))
