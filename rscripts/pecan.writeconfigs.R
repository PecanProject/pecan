### LOAD SETTINGS ###
settings <- settings()
outdir   <- settings$outdir
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)
quantiles <- settings$sensitivity.analysis$quantiles

for (pft in settings$pfts){
  load(paste(pft$outdir, '/pecan.MA.Rdata', sep = ''))


  browser()
  pft.xml <- pecan.config.constants(pft$name)
  
  prior.samps <- lapply(traits, 
  function(trait) get.sample(prior.distns[trait,], nrow(as.matrix(trait.mcmc[[trait]]))))
  names(prior.samps) <- traits
  prior.ens.samps <- get.ensemble.samples(ensemble.size, prior.samps)
  prior.run.ids <- write.ensemble.configs(pft.xml, prior.ens.samps, 'prior', pft$outdir)

  post.samps <- lapply(traits, function(trait) as.matrix(trait.mcmc[[trait]][,'beta.o']))
  names(post.samps) <- traits
  post.ens.samps <- get.ensemble.samples(ensemble.size, post.samps)
  post.run.ids <- write.ensemble.configs(pft.xml, post.ens.samps, 'post', pft$outdir)

  #This section is preserved only to maintain code in sensitivity analysis
  samps <- list(prior=prior.samps, 
                post=post.samps, 
                priors=prior.distns)
  sample.ensemble <- list(prior=prior.ens.samps, post.ens.samps)
  save(sample.ensemble, file = paste(pft$outdir, 'sample.ensemble.RData', sep=''))
  save(samps, file='samps.Rdata')

  if('sensitivity.analysis' %in% settings) {
    quantiles <- get.quantiles(settings$quantiles)
  
    quantile.samples <- list(post  = get.sa.samples(post.samps, quantiles),
                             prior = get.sa.samples(prior.samps, quantiles))
    for(run.name in names(quantile.samples)) {
      names(quantile.samples[[run.name]]) <- traits
      run.ids <- write.sa.configs(pft.xml, quantile.samples[[run.name]], run.name, pft$outdir)
    }
  
    save(quantile.samples, file = paste(pft$outdir, "quantile.samples.Rdata", sep=''))
  }
}

