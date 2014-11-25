## Test local run
#source("specdataproc.R")
source("inv_bayes.R")
g2 <- pinvbayes(grapedat,
                local.store=TRUE,
                ngibbs=200,
                random.effects='none',
                random.inits=1,
                ar.step=10,
                ar.min=0.4)

plot(g2$N, type='l')
length(unique(g2$N))/length(g2$N)
