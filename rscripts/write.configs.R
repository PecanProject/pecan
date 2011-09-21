### LOAD SETTINGS ###
library(XML)
if(interactive()){
  user <- system('echo $USER', intern = TRUE)
  if(user == 'dlebauer'){
    settings.file = '/home/dlebauer/pecan/2011.07.18/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
outdir   <- settings$outdir
host<- settings$run$host

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))

##TODO determine what Rdata objects need to be loaded for this script and save 
#it in meta.analysis.R then load it. Perhaps get the load.object/save.object fns working
## ensemble.size 
## for each pft
## traits, prior distributions, trait.mcmc

trait.samples <- list()
sa.samples <- list()
ensemble.samples <- list()

#Remove existing config files locally and on  host

todelete <- dir(paste(settings$pfts$pft$outdir, '/out/', sep = ''),
                c('ED2INc.*','c.*'),
                recursive=TRUE, full.names = TRUE)
file.remove(todelete)

system(paste("ssh -T ", host$name,
             " '",'find ', host$rundir, 'ED2INc.* -delete',"'",sep=''))
system(paste("ssh -T ", host$name,
             " '",'find ', host$rundir, 'c.* -delete',"'",sep=''))


for (i in seq(pft.names)){
  load(paste(outdirs[i], '/prior.distns.Rdata', sep=''))

  if("trait.mcmc.Rdata" %in% dir(outdirs)) {
    load(paste(outdirs[i], '/trait.mcmc.Rdata', sep=''))
  }

  pft.name <- pft.names[i]

  ## when no ma for a trait, sample from  prior
  traits <- if(exists('trait.mcmc')) {
    names(trait.mcmc)
  } else {
    NA
  }
  #KLUDGE: assumes mcmc for first trait is the same size as others
  samples.num <- ifelse(exists('trait.mcmc'), nrow(as.matrix(trait.mcmc[[1]])), 20000)

  priors <- rownames(prior.distns)
  for (prior in priors) {
    if (prior %in% traits) {
      samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
    } else {
      samples <- get.sample(prior.distns[prior,], samples.num)
    }
    trait.samples[[pft.name]][[prior]] <- samples
  }
    

  ## subset the trait.samples to ensemble size using Halton sequence 
  if('ensemble' %in% names(settings) && settings$ensemble$size > 0) {
    ensemble.samples[[pft.name]] <- get.ensemble.samples(settings$ensemble$size, trait.samples[[pft.name]])
    write.ensemble.configs(settings$pfts[[i]], ensemble.samples[[pft.name]], 
        host, outdir, settings)
  }

 

  if('sensitivity.analysis' %in% names(settings)) {
    if( is.null(settings$sensitivity.analysis)) {
      print(paste('sensitivity analysis settings are NULL'))
    } else {
      quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
      sa.samples[[pft.name]] <-  get.sa.samples(trait.samples[[pft.name]], quantiles)
      write.sa.configs(settings$pfts[[i]], sa.samples[[pft.name]], 
                       host, outdir, settings)
    }
  }
}

#Make outdirectory
ssh(host$name, 'mkdir ', host$outdir)
save(ensemble.samples, trait.samples, sa.samples, settings, file = paste(outdir, 'samples.Rdata', sep=''))
rsync(paste(outdir, 'samples.Rdata', sep=''),
      paste(host$name, ':', host$outdir, sep=''))
 
