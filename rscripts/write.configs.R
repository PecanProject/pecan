### LOAD SETTINGS ###
library(XML)
if(interactive()){
  settings.file = '~/pecan/tundra.xml'
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
outdir   <- settings$outdir

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))


##TODO determine what Rdata objects need to be loaded for this script and save it in meta.analysis.R then load it. Perhaps get the load.object/save.object fns working
## ensemble.size 
## for each pft
## traits, prior distributions, trait.mcmc

##TODO make sure that all of the traits with priors end up in the trait.samples and other lists and config files
trait.samples <- list()
sa.samples <- list()
ensemble.samples <- list()
run.ids <- list()

for (i in seq(pft.names)){
  load(paste(outdirs[i], 'trait.mcmc.Rdata', sep=''))
  load(paste(outdirs[i], 'prior.distns.Rdata', sep=''))

  pft.name <- pft.names[i]
  ## config header
  pft.xml <- pecan.config.constants(pft.name)

  ## when no ma for a trait, sample from  prior
  traits <- names(trait.mcmc)
  #KLUDGE: assumes mcmc for first trait is the same size as others
  samples.num <- nrow(as.matrix(trait.mcmc[[1]]))
  browser()
  priors<-rownames(prior.distns)
  ## convert trait.mcmc for each pft to dataframes in trait.samples
  trait.samples <- list()
  for (prior in priors) {
    if (prior %in% traits)
      sample <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
    else
      sample <- get.sample(prior.distns[prior,], samples.num)
    trait.samples[[pft.name]][[prior]] <- sample
  }
  ## TODO ed specific tranform function
  ## convert SLA from kg leaf / m2 to kg C / m2
  ## result[, c('mean','stat')] <- result[, c('mean','stat')] / 0.48 
  ## convert leaf width / 1000
  
  ## subset the trait.samples to ensemble size using Halton sequence 
  ensemble.samples[[pft.name]] <- get.ensemble.samples(settings$ensemble$size, trait.samples[[pft.name]])
  run.ids[[pft.name]] <- write.ensemble.configs(pft.xml, ensemble.samples[[pft.name]], outdirs[i])
  
  if(!is.null(settings$sensitivity.analysis)) {
    quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
    sa.samples[[pft.name]] <-  get.sa.samples(trait.samples[[pft.name]], quantiles)
    run.ids <- write.sa.configs(pft.xml, sa.samples[[pft.name]], outdirs[i])
  } 
}

save(ensemble.samples, file = paste(outdir, 'sample.ensemble.RData', sep=''))
save(trait.samples, file=paste(outdir, 'trait.samples.Rdata'))
save(sa.samples, file = paste(outdir, 'sa.samples.Rdata', sep=''))
