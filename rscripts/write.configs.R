### LOAD SETTINGS ###
library(XML)
if(interactive()){
  settings.file = '~/pecan/settings.pavi.xml'
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
outdir   <- settings$outdir

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)
quantiles <- settings$sensitivity.analysis$quantiles

pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))

save(outdir, 'outdir.Rdata')
save(c(settings, outdir, pft.names, outdirs), file = 'settings.Rdata')




##TODO determine what Rdata objects need to be loaded for this script and save it in meta.analysis.R then load it. Perhaps get the load.object/save.object fns working
## ensemble.size 
## for each pft
## traits, prior distributions, trait.mcmc

##TODO make sure that all of the traits with priors end up in the trait.samples and other lists and config files

trait.samples <- list()
sa.samples <- list()
ensemble.samples <- list()
run.ids <- list()
names(trait.samples) <- names(sa.samples) <- names(ensemble.samples) <- names(run.ids) <- pft.names

for (i in seq(pft.names)){
  pft.name <- pft.names[i]
  ## config header
  pft.xml <- pecan.config.constants(pft.name)

  ## when no ma for a trait, sample from  prior
  ma.traits <- names(trait.mcmc)
  trait.samples[[pft.name]] <- lapply(traits[which(!traits %in% ma.traits)],
                  function(trait) get.sample(prior.distns[trait,],
                                             nrow(as.matrix(trait.mcmc[[trait]]))))
  ## convert trait.mcmc for each pft to dataframes in trait.samples
  trait.samples[[pft.name]] <- append(trait.samples[[pft.name]],
                  lapply(ma.traits, function(trait) as.matrix(trait.mcmc[[trait]][,'beta.o'])))

  ## TODO ed specific tranform function
  ## convert SLA from kg leaf / m2 to kg C / m2
  ## result[, c('mean','stat')] <- result[, c('mean','stat')] / 0.48 
  ## convert leaf width / 1000
  
  names(trait.samples[[pft.name]]) <- traits
  ## subset the trait.samples to ensemble size using Halton sequence 
  ensemble.samples[[pft.name]] <- get.ensemble.samples(ensemble.size, trait.samples[[pft.name]])
  run.ids[[pft.name]] <- write.ensemble.configs(pft.xml, ensemble.samples[[pft.name]], outdirs[i], pft.name)
  
  if('sensitivity.analysis' %in% settings) {
    sa.samples[[pft.name]] <-  get.sa.samples(trait.samples[[pft.name]], quantiles)
    names(sa.samples[[run.name]]) <- traits
    run.ids <- write.sa.configs(pft.xml, sa.samples[[run.name]], run.name, outdirs[i])
  } 
}

save(ensemble.samples, file = paste(outdir, 'sample.ensemble.RData', sep=''))
save(trait.samples, file=paste(outdir, 'trait.samples.Rdata')
save(sa.samples, file = paste(outdir, 'sa.samples.Rdata', sep=''))
