library(devtools)
library(FitAR)

load_all('~/Projects/pecan/pecan/modules/rtm')

data(testspec)
p <- prospect(defparam("prospect_5"), 5)[,1]
obs <- testspec_ACRU[,1]
png("~/Pictures/pacf.obs.png")
pacf(obs)
dev.off()

p1 <- as.matrix(p[-length(p)])
p2 <- as.matrix(p[-1])


neff_prof <- function(n = 10000) replicate(n, neff(p))

#Rprof()
#neff_orig <- replicate(n = 10000, neff(p))
#Rprof(NULL)

#sp <- summaryRprof()
#head(sp$by.self, 10)

# Steps to my neff
base <- function() ar.yw(p, order.max = 1)
fitar <- function() FitAR::FitAR(p, 1)
linmod <- function() lm.fit(x = p1, y = p2)
mb <- microbenchmark(base, fitar, linmod)
ggplot2::autoplot(mb)

arout <- ar.yw(p, order.max = 1)
ar2 <- FitAR::FitAR(p, 1)
vp <- arout$var.pred  # Prediction variance
arar <- arout$ar      # Autoregression coefficients
spec <- vp / (1 - sum(arar))^2
varp <- var(p)
out <- length(p) * var(p) / spec
print(out)

#myneff <- function(x) {

