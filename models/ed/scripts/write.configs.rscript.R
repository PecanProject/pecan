#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
### LOAD SETTINGS ###
library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '~/in/ebifarm/fast/ebifarm.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/atqasuk.xml'
  } else {
    paste('please specify settings file in write.configs.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
outdir   <- settings$outdir
host <- settings$run$host

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
library(PEcAn)

pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
outdirs <- unlist(xpathApply(settings.xml, '//pfts//pft//outdir', xmlValue))

trait.samples <- list()
sa.samples <- list()
ensemble.samples <- list()
env.samples <- list()

## Remove existing config files
if(FALSE){
  todelete <- dir(paste(settings$pfts$pft$outdir, 'out/', sep = ''),
      c('ED2INc.*','c.*'),
      recursive=TRUE, full.names = TRUE)
  if(length(todelete>0)) file.remove(todelete)
  
  filename.root <- get.run.id('c.','*')
  
  if(host$name == 'localhost'){
    if(length(dir(host$rundir, pattern = filename.root)) > 0) {
      todelete <- dir(host$outdir,
          pattern = paste(filename.root, "*[^log]", sep = ''), 
          recursive=TRUE, full.names = TRUE)
      file.remove(todelete)
    }
  } else {
    files <- system(paste("ssh ", host$name, " 'ls ", host$rundir, "*", filename.root, "*'", sep = ''), intern = TRUE)
    if(length(files) > 0 ) {
      todelete <- files[-grep('log', files)]
      system(paste("ssh -T ", host$name,
              " 'for f in ", paste(todelete, collapse = ' '),"; do rm $f; done'",sep=''))
    }
  }
}

## Load PFT priors and posteriors

for (i in seq(pft.names)){
  
  load(paste(outdirs[i], 'prior.distns.Rdata', sep=''))
  
  if("trait.mcmc.Rdata" %in% dir(outdirs)) {
    load(paste(outdirs[i], 'trait.mcmc.Rdata', sep=''))
  }
  
  pft.name <- pft.names[i]
  
  ## when no ma for a trait, sample from  prior
  ## trim all chains to shortest mcmc chain, else 20000 samples
  if(exists('trait.mcmc')) {
    traits <- names(trait.mcmc)
    samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
  } else {
    traits <- NA
    samples.num <- 20000
  }
  
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

## NEED TO IMPLEMENT:
## Load Environmental Priors and Posteriors


## write SENSITIVITY ANALYSIS
if('sensitivity.analysis' %in% names(settings)) {
  quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
  if( is.null(settings$sensitivity.analysis)) {
    print(paste('sensitivity analysis settings are NULL'))
  } else {
    sa.samples <-  get.sa.sample.list(trait.samples, 
        env.samples,
        quantiles)
    write.sa.configs(settings$pfts, sa.samples, 
        host, outdir, settings)
  }
}

## Write ENSEMBLE
if('ensemble' %in% names(settings) && settings$ensemble$size > 0) {
  ## subset the trait.samples to ensemble size using Halton sequence 
  ensemble.samples <- get.ensemble.samples(settings$ensemble$size, trait.samples, env.samples)
  write.ensemble.configs(settings$pfts, ensemble.samples, 
      host, outdir, settings)
}




save(ensemble.samples, trait.samples, sa.samples, settings,
    file = paste(outdir, 'samples.Rdata', sep=''))

## Make outdirectory, send samples to outdir

if(host$name == 'localhost'){
  if(!host$outdir == outdir) {
    dir.create(host$outdir)
    file.copy(from = paste(outdir, 'samples.Rdata', sep=''),
        to   = paste(host$outdir, 'samples.Rdata', sep = ''),
        overwrite = TRUE)
  }
} else {  
  mkdir.cmd <- paste("'if ! ls ", host$outdir, " > /dev/null ; then mkdir -p ", host$outdir," ; fi'",sep='')
  system(paste("ssh", host$name, mkdir.cmd))
  system(paste('rsync -routi ', paste(outdir, 'samples.Rdata', sep=''),
          paste(host$name, ':', host$outdir, sep='')))
}
