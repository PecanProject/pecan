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
