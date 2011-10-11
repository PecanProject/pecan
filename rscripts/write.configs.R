### LOAD SETTINGS ###
library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '/home/dlebauer/out/2011.08.26/settings.ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
outdir   <- settings$outdir
host<- settings$run$host

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PECAn)

pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))

trait.samples <- list()
sa.samples <- list()
ensemble.samples <- list()

## Remove existing config files

todelete <- dir(paste(settings$pfts$pft$outdir, '/out/', sep = ''),
                c('ED2INc.*','c.*'),
                recursive=TRUE, full.names = TRUE)
file.remove(todelete)


if(host$name == 'localhost'){
  todelete <- dir(host$outdir,
                  c('ED2INc.*','c.*'),
                  recursive=TRUE, full.names = TRUE)
  file.remove(todelete)
} else {
  system(paste("ssh -T ", host$name,
               " '",'find ', host$rundir, 'ED2INc.* -delete',"'",sep=''))
  system(paste("ssh -T ", host$name,
               " '",'find ', host$rundir, 'c.* -delete',"'",sep=''))
}

## Load priors and posteriors

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
  ## trim all chains to shortest mcmc chain, else 20000 samples
  samples.num <- ifelse(exists('trait.mcmc'),
                        min(sapply(trait.mcmc,
                                   function(x) nrow(as.matrix(x)))),
                        20000)

  priors <- rownames(prior.distns)
  for (prior in priors) {
    if (prior %in% traits) {
      samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
    } else {
      samples <- get.sample(prior.distns[prior,], samples.num)
    }
    trait.samples[[pft.name]][[prior]] <- samples
  }
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


                                        #Make outdirectory
save(ensemble.samples, trait.samples, sa.samples, settings,
     file = paste(outdir, 'samples.Rdata', sep=''))

if(host$name == 'localhost'){
  if(!host$outdir == outdir) {
    dir.create(host$outdir)
    file.copy(from = paste(outdir, 'samples.Rdata', sep=''),
              to   = paste(host$outdir, 'samples.Rdata', sep = ''),
              overwrite = TRUE)
  }
} else {
  ssh(host$name, 'mkdir ', host$outdir)
  rsync(paste(outdir, 'samples.Rdata', sep=''),
        paste(host$name, ':', host$outdir, sep=''))
}

 
