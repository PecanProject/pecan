## Test local run
#source("specdataproc.R")
source("inv_bayes.R")
g2 <- pinvbayes(grapedat,
                local.store=TRUE,
                ngibbs=20,
                random.effects='none',
                random.inits=1,
                ar.step=100,
                ar.min=0.4,
                JumpRSD=1)

plot(g2$N/max(g2$N), type='l')
lines(g2$Cab/max(g2$Cab), col=2)
lines(g2$Cw/max(g2$Cw), col=3)
lines(g2$Cm/max(g2$Cm), col=4)
for(i in 1:4){
  print(length(unique(g2[[i]]))/length(g2[[i]]))
}
