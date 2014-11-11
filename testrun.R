## Test run of Bayesian inversion of PROSPECT

source("prospect_bayesinv.R")
beech <- read.table("data/beech.txt", header=TRUE, quote="\"")

beech.inv <- pinvbayes(beech)
bi <- data.frame(beech.inv)
write.csv(bi, file="test_beech.csv")