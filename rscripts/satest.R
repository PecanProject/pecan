
load('out/pecan.samps.Rdata')
mean.quantile <-  function(x) mean(mean(x)-x>0)
prior.mean.quantile <- apply(prior.samps, 2, mean.quantile)
post.mean.quantile  <- apply(post.samps, 2, mean.quantile)

prior.quantiles     <- sapply(prior.mean.quantile, function(x) x + c(-0.3, -0.15, 0, 0.15, 0.3))
post.quantiles      <- sapply(post.mean.quantile, function(x) x + c(-0.3, -0.15, 0, 0.15, 0.3))

calculate.quantiles <- function(x,samps,quantiles) quantile(samps[,x], quantiles[,x])

traits<-colnames(post.samps)
qpost <- lapply(traits, calculate.quantiles, post.samps, post.quantiles)
qprior <- lapply(traits, calculate.quantiles, prior.samps, prior.quantiles)
names(qpost) <- traits
names(qprior) <- traits

save(qpost,qprior,file='quantiles1530.Rdata')
rm(list=ls())

load('quantiles1530.Rdata')

load('out/dat30.Rdata')
load('out/edout.Rdata')
load('out20101109/satables.Rdata')

sapost <- satables[['post']][['agb']]
saprior <- satables[['prior']][['agb']]

saprior$lcl30.f <- saprior$null
saprior$ucl30.f <- saprior$null
sapost$lcl30.f <- saprior$null
sapost$ucl30.f <- saprior$null

for(x in names(qpost)){
  saprior[x, c('lcl30.f','ucl30.f')] <- qprior[[x]][c(1,5)]
  sapost[x, c('lcl30.f','ucl30.f')] <- qpost[[x]][c(1,5)]
}

prior.dtheta <- saprior[,c('lcl30.f', 'lcl.f', 'mean.f', 'ucl.f', 'ucl30.f')]
post.dtheta <- sapost[,c('lcl30.f', 'lcl.f', 'mean.f', 'ucl.f', 'ucl30.f')]

ls()
dat30 <- dat[['agb']][['output']]

## next, need to extract the ucl/lcls from dat30 to go along with those already in the satables
