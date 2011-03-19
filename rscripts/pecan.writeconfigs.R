library(PECAn, lib.loc = '~/lib/R')
load('out/pecan.MA.Rdata')


browser()

pftXml <- pecan.config.constants(settings$pft)
  
prior.samps <- lapply(traits, function(trait) get.sample(prior.data[trait,], nrow(as.matrix(trait.mcmc[[trait]]))))
names(prior.samps) <- traits
prior.ensemble <- write.ensemble.configs(pftXml, ensemble_size, prior.samps, 'prior', outdir)

post.samps <- lapply(traits, function(trait) as.matrix(trait.mcmc[[trait]][,'beta.o']))
names(post.samps) <- traits
post.ensemble <- write.ensemble.configs(pftXml, ensemble_size, post.samps, 'post', outdir)


if(sensitivity_analysis) {
  quantiles<-vector()
  if (!is.null(settings$quantiles$sigma)){
    sigma <- as.numeric(settings$quantiles[names(settings$quantiles)=='sigma'])
    quantiles <- pnorm(1-sigma)
  }
  if (!is.null(settings$quantiles$quantile)) {
    quantiles <- append(quantiles, as.numeric(settings$quantiles[names(settings$quantiles)=='quantile']))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1-pnorm(-3:3) #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  quantiles <- sort(quantiles)

  calculate.quantiles <- function(x,samps, quantiles) {
    quantile(samps[,x], quantiles)
  }
  quantile.samples <- list(post  = lapply(traits, calculate.quantiles, post.samps, quantiles),
                           prior = lapply(traits, calculate.quantiles, prior.samps, quantiles))
  for(i in names(quantile.samples)) {
    names(quantile.samples[[i]]) <- traits
  }
  write.sa.configs(pftXml, quantile.samples, runname, outdir)

  save(quantile.samples, file = "out/quantile.samples.Rdata")
}

save(sample.ensemble, file = paste(outdir, 'sample.ensemble.RData', sep=''))
save(samps, file='samps.Rdata')
