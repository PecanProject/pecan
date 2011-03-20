library(PECAn, lib.loc = '~/lib/R')
load('out/pecan.MA.Rdata')


pft.xml <- pecan.config.constants(settings$pft)
  
prior.samps <- lapply(traits, 
  function(trait) get.sample(prior.data[trait,], nrow(as.matrix(trait.mcmc[[trait]]))))
names(prior.samps) <- traits
prior.ens.samps <- get.ensemble.samples(ensemble_size, prior.samps)
prior.run.ids <- write.ensemble.configs(pft.xml, prior.ens.samps, 'prior', outdir)

post.samps <- lapply(traits, function(trait) as.matrix(trait.mcmc[[trait]][,'beta.o']))
names(post.samps) <- traits
post.ens.samps <- get.ensemble.samples(ensemble_size, post.samps)
post.run.ids <- write.ensemble.configs(pft.xml, post.ens.samps, 'post', outdir)

#This section is preserved only to maintain code in sensitivity analysis
samps <- list(prior=prior.samps, 
              post=post.samps, 
              priors=prior.data)
sample.ensemble <- list(prior=prior.ens.samps, post.ens.samps)
save(sample.ensemble, file = paste(outdir, 'sample.ensemble.RData', sep=''))
save(samps, file='samps.Rdata')

if(sensitivity_analysis) {
  quantiles <- get.quantiles(settings$quantiles)
  
  quantile.samples <- list(post  = get.sa.samples(post.samps, quantiles),
                           prior = get.sa.samples(prior.samps, quantiles))
  for(run.name in names(quantile.samples)) {
    names(quantile.samples[[run.name]]) <- traits
    run.ids <- write.sa.configs(pft.xml, quantile.samples[[run.name]], run.name, outdir)
  }

  save(quantile.samples, file = "out/quantile.samples.Rdata")
}
