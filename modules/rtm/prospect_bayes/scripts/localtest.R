## Test local run
if (!exists("ldd")) ldd <- 0
ld <- function(ldd) {
  if (ldd == 0) source("specdataproc.R")
  ldd <- ldd + 1
}
ld(ldd)

source("inv_bayes.R")
g2 <- pinvbayes(grapedat,
                local.store=TRUE,
                ngibbs=1000,
                random.effects='leaf',
                inits='mle',
                ar.step=100,
                ar.min=0.4,
                JumpRSD=0.01)

par(mfrow = c(2,2))
plot(g2$N, type='l', main='N', ylab="")
plot(g2$Cab, type='l', main='Cab', ylab="")
plot(g2$Cw, type='l', main='Cw', ylab="")
plot(g2$Cm, type='l', main='Cm', ylab="")
for(i in 1:4){
  print(length(unique(g2[[i]]))/length(g2[[i]]))
}
