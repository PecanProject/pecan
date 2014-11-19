## Test local run
source("specdataproc.R")

specdat <- load.all.spec()
grapedat <- specmatrix(specdat)

source("inv_bayes.R")
g1 <- pinvbayes(grapedat, local.store=TRUE, ngibbs=1000)