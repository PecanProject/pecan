library(PECAn, lib.loc = '~/lib/R')
load('out/pecan.MA.Rdata')

## sample values for ensemble
trait.samps <- pecan.samps(trait.mcmc, priors)
prior.dists <- trait.samps[['priors']]
post.samps <- trait.samps[['post.samps']]     
prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors

post.dtheta.q <- pecan.dtheta(post.samps)
prior.dtheta.q <- pecan.dtheta(prior.samps)

## generate config files

write.configs(M, pft, prior.samps, post.samps)
setwd(outdir)
rm(outdir)
save.image(pecan.samps.Rdata)
save(M, file='M.Rdata')
## print out some statistical summaries and figures from meta-analysis
