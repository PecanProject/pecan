library(PECAn, lib.loc = '~/lib/R')
load('out/pecan.MA.Rdata')
load('outdir.Rdata')

## sample values for ensemble
priors$distn[priors$distn=='weib']<-'weibull'
trait.samps <- pecan.samps(trait.mcmc, priors)
prior.dists <- trait.samps[['priors']]
post.samps <- trait.samps[['post.samps']]     
prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors

post.dtheta.q <- pecan.dtheta(post.samps, 0.30)
prior.dtheta.q <- pecan.dtheta(prior.samps, 0.30)

## generate config files

write.configs(M, SA=TRUE, pft, prior.samps, post.samps, outdir)
setwd(outdir)
save(M, file='M.Rdata')
rm(outdir)
save(post.samps, prior.samps,file='pecan.samps.Rdata')

## print out some statistical summaries and figures from meta-analysis
