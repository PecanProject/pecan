##' Read ED output 

##'
##' Extract ED output for specific variables from an hdf5 file
##' @title 
##' @param filename 
##' @param variables 
##' @return single value of AGB from  filename for all plants
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  library(hdf5, lib.loc='~/lib/R/')
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
read.ensemble.output <- function(ensemble.size, host, outdir, run.time, pft.name='', read.output = read.output.ed){
  ensemble.output <- list()
  rsync(paste(host$name, ':', host$outdir, run.time, 
              '/*', get.run.id('ENS', '', pft.name=pft.name), '*', sep=''),
        outdir)
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
read.sa.output <- function(traits, quantiles, host, outdir, run.time, pft.name='', read.output = read.output.ed){
  sa.output <- data.frame()
  rsync(paste(host$name, ':', host$outdir, run.time, 
              '/*', get.run.id('SA', '', pft.name=pft.name), '*', sep=''),
        outdir)
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
