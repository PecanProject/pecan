library(PECAn, lib.loc = '~/lib/R')
load('pecan.MA.Rdata')
## sample values for ensemble
trait.samps <- pecan.samps(trait.mcmc, priors)
prior.dists <- trait.samps[['priors']]
post.samps <- trait.samps[['post.samps']]     
prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors

post.dtheta.q <- pecan.dtheta(samps = post.samps)
prior.dtheta.q <- pecan.dtheta(samps = prior.samps)

## generate config files
write.configs(M, pft, prior.samps, post.samps)
save.image(file='pecan.samps.Rdata')
## print out some statistical summaries and figures from meta-analysis
