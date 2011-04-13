##' Read ED output 
##'
##' Extract ED output for specific variables from an hdf5 file
##' @title 
##' @param filename 
##' @param variables 
##' @return single value of AGB from  filename for all plants
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  MAGIC_NUMBER = 20
  data <- hdf5load(filename, load = FALSE)[variables]
  if(all(c("AGB_CO", "NPLANT") %in% variables)) {
    return(AGB  <- sum(data$AGB_CO * data$NPLANT) * MAGIC_NUMBER)
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
  file.names <- file.names[grep('-E-', file.names)]
  return(lapply(file.names, read.output.file.ed))
}

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @returns a list of ensemble output 
##' @author David
read.ensemble.output <- function(ensemble.size, outdir, read.output = read.output.ed){
  ensemble.output <- list()
  for(ensemble.id in seq(ensemble.size)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, log10(ensemble.size)+1))
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
read.sa.output <- function(traits, quantiles, outdir, read.output = read.output.ed){
  sa.output <- list()
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- get.run.id('SA', round(quantile,3), trait=trait)
      sa.output[[trait]][[as.character(round(quantile,3))]] <- read.output(run.id, outdir)
    }
  }
  return(sa.output)
}
