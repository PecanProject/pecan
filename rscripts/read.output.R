# As is the case with 
#
##TODO fix this filename restriction bug in ED, remove abbreviate.run.id.ED 

##' Abbreviates run.ids 
##'
##' For use in input files, because ED input files must be <32 characters long.
##' @title abbreviate run.ids
##' @param run.id 
##' @return abbreviated run.id
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
##' @title read output - ED
##' @param filename string, name of file with data
##' @param variables variables to extract from file
##' @return single value of AGB from  filename for all plants
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  library(hdf5)
  MAGIC_NUMBER = 20
  data <- hdf5load(filename, load = FALSE)[variables]
  if(all(c("AGB_CO", "NPLANT") %in% variables)) {
    return(sum(data$AGB_CO * data$NPLANT) * MAGIC_NUMBER)
  }
  else return(sum(data[[variables]]))
}

##' .. content for \description{} (no empty lines) ..
##'
##' Reads the output of a single model run
##' @title 
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @param start.date 
##' @param end.date 
##' @return vector of output variable for all runs within ensemble
read.output.ed <- function(run.id, outdir, start.date=NA, end.date=NA){
  file.names <- dir(outdir, pattern=run.id, full.names=TRUE)
  file.names <- grep('-Y-([0-9]{4}).*', file.names, value=TRUE)
  years <- sub('((?!-Y-).)*-Y-([0-9]{4}).*', '\\2', file.names, perl=TRUE)
  if(!is.na(start.date) && nchar(start.date) > 0){
    start.year <- strftime(as.POSIXlt(start.date), format='%Y')
    file.names <- file.names[years>=start.year]
  }
  if(!is.na(end.date) && nchar(end.date) > 0){
    end.year <- strftime(as.POSIXlt(end.date), format='%Y')
    file.names <- file.names[years<=end.year]
  }
  file.names <- file.names[!is.na(file.names)]
  return(mean(sapply(file.names, read.output.file.ed), na.rm = TRUE))
}

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @returns a list of ensemble output 
##' @param ensemble.size 
##' @param outdir 
##' @param run.time 
##' @param pft.name 
##' @param start.date 
##' @param end.date 
##' @param read.output 
read.ensemble.output <- function(ensemble.size, outdir, run.time, pft.name='', 
    start.date, end.date, read.output = read.output.ed){
  ensemble.output <- list()
  for(ensemble.id in seq(ensemble.size)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), pft.name=pft.name)#log10(ensemble.size)+1))
    ensemble.output[[ensemble.id]] <- read.output(run.id, outdir, start.date, end.date)
  }
  return(ensemble.output)
}

##' .. content for \description{} (no empty lines) ..
##'
##' 
##' @title 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @param traits 
##' @param quantiles 
##' @param outdir 
##' @param pft.name 
##' @param start.date 
##' @param end.date 
##' @param read.output 
read.sa.output <- function(traits, quantiles, outdir, pft.name='', 
    start.date, end.date, read.output = read.output.ed){
  sa.output <- data.frame()
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
      print(run.id)
      sa.output[as.character(round(quantile*100,3)), trait] <- read.output(run.id, outdir, start.date, end.date)
    }
  }
  sa.output['50',] <- read.output(get.run.id('SA', 'median'), outdir)
  return(sa.output)
}

load('samples.Rdata')
sa.agb<-list()
for(pft.name in names(trait.samples)){
  
  traits <- names(trait.samples[[pft.name]])
  quantiles.str <- rownames(sa.samples[[pft.name]])
  quantiles.str <- quantiles.str[which(quantiles.str != '50')]
  quantiles <- as.numeric(quantiles.str)/100
  
  sa.agb[[pft.name]] <- read.sa.output(traits, quantiles, outdir = getwd(), 
      pft.name=pft.name, settings$run$start.date, settings$run$end.date)
  #ensemble.output[[pft.name]]<-read.ensemble.output(ensemble.size, outdir, 
  #    pft.name=pft.name, start.year, end.year)
}
save(sa.agb, file = paste(settings$run$host$outdir, 'output.Rdata', sep=''))
