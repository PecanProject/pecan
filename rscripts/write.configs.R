### LOAD SETTINGS ###
library(XML)
if(interactive()){
  user <- system('echo $USER', intern = TRUE)
  if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in write.configs.R')
  }
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
  priors<-rownames(prior.distns)
  trait.samples <- list()
  for (prior in priors) {
    if (prior %in% traits)
      samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
    else
      samples <- get.sample(prior.distns[prior,], samples.num)
    trait.samples[[pft.name]][[prior]] <- samples
  }

  ## subset the trait.samples to ensemble size using Halton sequence 
  ensemble.samples[[pft.name]] <- get.ensemble.samples(settings$ensemble$size, trait.samples[[pft.name]])
  run.ids[[pft.name]] <- write.ensemble.configs(pft.xml, ensemble.samples[[pft.name]], outdirs[i])
  
  if('sensitivity.analysis' %in% names(settings)) {
    quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
    sa.samples[[pft.name]] <-  get.sa.samples(trait.samples[[pft.name]], quantiles)
    run.ids[[pft.name]] <- append(run.ids[[pft.name]], write.sa.configs(pft.xml, sa.samples[[pft.name]], outdirs[i]))
  } 
  system(paste(settings$pecanDir, 'bash/rename.configs.sh ', outdirs[i] sep = '')) 
}
save(run.ids, file = paste(outdir, 'run.ids.Rdata', sep = ''))
save(ensemble.samples, file = paste(outdir, 'sample.ensemble.RData', sep=''))
save(trait.samples, file=paste(outdir, 'trait.samples.Rdata', sep = ''))
save(sa.samples, file = paste(outdir, 'sa.samples.Rdata', sep=''))


