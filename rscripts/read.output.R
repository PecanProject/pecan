#As is the case with ED, input files must be <32 characters long.
#this function abbreviates run.ids for use in input files
##TODO fix this filename restriction bug in ED, remove abbreviate.run.id.ED 
abbreviate.run.id.ED <- function(run.id){
  #TODO: remove references to specific pft names and use outdir
  run.id <- gsub('tundra.', '', run.id)
  run.id <- gsub('ebifarm.', '', run.id)
  run.id <- gsub('deciduous', 'decid', run.id)
  run.id <- gsub('evergreen', 'everg', run.id)
  run.id <- gsub('_', '', run.id)
  run.id <- gsub('root', 'rt', run.id)
  run.id <- gsub('water', 'h2o', run.id)
  run.id <- gsub('factor', '', run.id)
  run.id <- gsub('turnover', 'tnvr', run.id)
  run.id <- gsub('mortality', 'mort', run.id)
  run.id <- gsub('conductance', 'cond', run.id)
  run.id <- gsub('respiration', 'resp', run.id)
  run.id <- gsub('stomatalslope', 'stmslope', run.id)
  run.id <- gsub('nonlocaldispersal', 'nldisprs', run.id)
  run.id <- gsub('quantumefficiency', 'quantef', run.id)
  
  return(run.id)
} 
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
}

##' Extract ED output for specific variables from an hdf5 file
##' @title 
##' @param filename 
##' @param variables 
##' @return single value of AGB from  filename for all plants
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  library(hdf5)
  MAGIC_NUMBER = 20
  data <- hdf5load(filename, load = FALSE)[variables]
  if(all(c("AGB_CO", "NPLANT") %in% variables)) {
    return(sum(data$AGB_CO * data$NPLANT) * MAGIC_NUMBER)
  }
}

##' ##' .. content for \description{} (no empty lines) ..
##'
##' Reads the output of a single model run
##' @title 
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @return vector of output variable for all runs within ensemble
read.output.ed <- function(run.id, outdir){
  file.names <- dir(outdir, pattern=run.id, full.names=TRUE)
  file.names <- file.names[grep('-Y-', file.names)]
  return(sum(sapply(file.names, read.output.file.ed)))
}

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @returns a list of ensemble output 
##' @author David
read.ensemble.output <- function(ensemble.size, outdir, run.time, pft.name='', read.output = read.output.ed){
  ensemble.output <- list()
  for(ensemble.id in seq(ensemble.size)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), pft.name=pft.name)#log10(ensemble.size)+1))
    ensemble.output[[ensemble.id]] <- read.output(run.id, outdir)
  }
  return(ensemble.output)
}

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @author David
read.sa.output <- function(traits, quantiles, outdir, pft.name='', read.output = read.output.ed){
  sa.output <- data.frame()
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
      print(run.id)
      sa.output[as.character(round(quantile*100,3)), trait] <- read.output(run.id, outdir)
    }
  }
  sa.output['50',] <- read.output(get.run.id('SA', 'median'), outdir)
  return(sa.output)
}

outdir=commandArgs(trailingOnly=TRUE)
load(paste(outdir, 'samples.Rdata', sep=''))
sa.agb<-list()
for(pft.name in names(trait.samples)){
  
  traits <- names(trait.samples[[pft.name]])
  quantiles.str <- rownames(sa.samples[[pft.name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100
  
  sa.agb[[pft.name]] <- read.sa.output(traits, quantiles, outdir, pft.name=pft.name)
  #ensemble.output[[pft.name]]<-read.ensemble.output(ensemble.size, outdir, pft.name=pft.name)
  
}
save(sa.agb, file = paste(outdir, 'output.Rdata', sep=''))
