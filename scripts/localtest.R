## Test local run
source("specdataproc.R")

specdat <- load.all.spec()
grapedat <- specmatrix(specdat)

source("inv_bayes.R")
g2 <- pinvbayes(grapedat, local.store=TRUE, ngibbs=50,random.effects='none', random.inits=1 )