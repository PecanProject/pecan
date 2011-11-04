##' Get parameter values used in ensemble
##'
##' Returns a matrix of trait values sampled quasi-randomly based on the Halton sequence
##' to be assigned to traits over several model runs.
##' given the number of model runs and a list of sample distributions for traits
##' The model run is indexed first by model run, then by trait
##' 
##' @title Get Ensemble Samples 
##' @param ensemble.size number of runs in model ensemble
##' @param samples random samples from parameter distribution, e.g. from a MCMC chain or a 
##' @return matrix of quasi-random (overdispersed) samples from trait distributions
##' @references \href{http://dx.doi.org/10.1145/355588.365104}{Halton, J. (1964), Algorithm 247: Radical-inverse quasi-random point sequence, ACM, p. 701, doi:10.1145/355588.365104.
get.ensemble.samples <- function(ensemble.size, samples) {
  ##force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  if(ensemble.size <= 0) return(NULL)
  halton.samples <- halton(n = ensemble.size, dim=length(samples))
  ##force as a matrix in case length(samples)=1
  halton.samples <- as.matrix(halton.samples)
  
  ensemble.samples <- matrix(nrow = ensemble.size, ncol = length(samples))
  colnames(ensemble.samples) <- names(samples)
  for(ensemble.id in 1:ensemble.size) {
    for(trait.i in seq(samples)) {
      ensemble.samples[ensemble.id, trait.i] <- 
        quantile(samples[[trait.i]], halton.samples[ensemble.id, trait.i])
    }
  }
  return(ensemble.samples)
}


##' Write ensemble config files
##'
##' Writes config files for use in meta-analysis and returns a list of run ids.
##' Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
##' a name to distinguish the output files, and the directory to place the files.
##' @title Write ensemble configs 
##' @param pft pft
##' @param ensemble.samples list of lists supplied by \link{get.ensemble.samples}
##' @param host server to which config files will be sent
##' @param outdir directory for model output (on server)
##' @param settings list of settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param convert.samples a model-specific function that transforms variables from units used in database to units used by model, e.g. \link{convert.samples.ED} 
##' @return nothing, writes ensemble configuration files as a side effect 
write.ensemble.configs <- function(pft, ensemble.samples,
                                   host, outdir, settings,
                                   write.config = write.config.ED,
                                   convert.samples=convert.samples.ED){

  if(host$name == 'localhost') {
    if("ENS" %in% dir(host$rundir)){
      file.remove(paste(host$rundir, '*',
                        get.run.id('ENS', '', pft.name=pft.name), '*"', sep=''))
    }
  } else {
    ssh(host$name, 'rm -f ', host$rundir, '*',
        get.run.id('ENS', '', pft.name=pft.name, '*'))
  }
  
  if(is.null(ensemble.samples)) return(NULL)

  run.ids<-list()
 
  for(ensemble.id in 1:nrow(ensemble.samples)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), 
                         pft.name=pft$name)
    unlink(paste(outdir, '*', run.id, '*', sep=''))
    write.config(pft, convert.samples(ensemble.samples[ensemble.id,]), 
                 settings, outdir, run.id)
  }
  if(host$name == 'localhost'){
    rsync(paste(outdir, '*',
                get.run.id('ENS', '', pft.name=pft.name), '*', sep=''),
          host$rundir)
  } else {
    system(paste('rsync -routi ',
                 paste(outdir, '*', get.run.id('ENS', '', pft.name=pft.name), '*', sep=''), 
                 paste(host$name, ':', host$rundir,  sep=''), sep = ' '))
  }
}




##' Returns a vector of quantiles specified by a given <quantiles> xml tag
##'
##' @title Get Quantiles  
##' @param quantiles.tag specifies tag used to specify quantiles
##' @return vector of quantiles
get.quantiles <- function(quantiles.tag) {
  quantiles<-vector()
  if (!is.null(quantiles.tag$quantile)) {
    quantiles <- as.numeric(quantiles.tag[names(quantiles.tag)=='quantile'])
  }
  if (!is.null(quantiles.tag$sigma)){
    sigmas <- as.numeric(quantiles.tag[names(quantiles.tag)=='sigma'])
    quantiles <- append(quantiles, 1 - pnorm(sigmas))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1-pnorm(-3:3) #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  return(sort(quantiles))
}

##' Samples parameters for a model run at specified quantiles.
##' 
##' Samples from long (>2000) vectors that represent random samples from a trait distribution.
##' Samples are either the MCMC chains output from the Bayesian meta-analysis or are randomly sampled from
##' the closed-form distribution of the parameter probabiolity distribution function.
##' The list is indexed first by trait, then by quantile.
##' @title get sensitivity analysis samples
##' @param samples random samples from trait distribution   
##' @param quantiles list of quantiles to at which to sample, set in settings file
##' @return a list of lists representing quantile values of trait distributions 
get.sa.samples <- function(samples, quantiles){
  sa.samples <- data.frame()
  for(trait in names(samples)){
    for(quantile in quantiles){
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], quantile)
    }
  }
  return(sa.samples)
}

##' Write sensitivity analysis config files
##'
##' Writes config files for use in sensitivity analysis.
##' @title Write sensitivity analysis configs
##' @param pft pft id used to query BETYdb
##' @param quantile.samples 
##' @param host server to which config files will be sent
##' @param outdir directory for model output (on server)
##' @param settings list of settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param convert.samples a model-specific function that transforms variables from units used in database to units used by model, e.g. \link{convert.samples.ED} 
##' @param ensemble.samples list of lists supplied by \link{get.sa.samples}
##' @return nothing, writes sensitivity analysis configuration files as a side effect 
write.sa.configs <- function(pft, quantile.samples, host, outdir, settings, 
                             write.config=write.config.ED, convert.samples=convert.samples.ED){
  MEDIAN <- '50'
  traits <- colnames(quantile.samples)
  
  if(host$name == 'localhost'){
      if("SA" %in% dir(host$rundir)){
        file.remove(paste(host$rundir, '*',
                      get.run.id('SA', '', pft.name=pft.name), '*"', sep=''))
      }
  } else {
    ssh(host$name, 'rm -f ', host$rundir, '*',
        get.run.id('SA', '', pft.name=pft.name, '*'))
  }
  
  median.samples <- quantile.samples[MEDIAN,]
  run.id <- get.run.id('SA', 'median', pft.name=pft$name)
  write.config(pft, convert.samples(median.samples), settings, outdir, run.id)
  
  for (trait in traits) {
    quantiles.str <- rownames(quantile.samples)
    for(quantile.str in quantiles.str) {
      if (quantile.str != MEDIAN) {
        quantile <- as.numeric(quantile.str)/100
        trait.samples <- median.samples
        trait.samples[trait] <- quantile.samples[quantile.str, trait]
        run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft$name)
        unlink(paste(outdir, '*', run.id, '*', sep=''))
        write.config(pft, convert.samples(trait.samples), settings, outdir, run.id)
      }
    }
  }
  if(host$name == 'localhost'){
    rsync(paste(outdir, '*',
                get.run.id('SA', '', pft.name=pft.name), '*', sep=''),
          host$rundir)
  } else {
    system(paste('rsync -routi ',
                 paste(outdir, '*', get.run.id('SA', '', pft.name=pft.name), '*', sep=''), 
                 paste(host$name, ':', host$rundir,  sep='')))
  }
}
