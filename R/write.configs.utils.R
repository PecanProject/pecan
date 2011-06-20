##### Ensemble functions #####
                                        #Returns a matrix of pseudo random values assigned to traits over several model runs.
                                        #given the number of model runs and a list of sample distributions for traits
                                        #The model run is indexed first by model run, then by trait
get.ensemble.samples <- function(ensemble.size, samples) {
                                        #force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  
  halton.samples <- halton(n = ensemble.size, dim=length(samples))
                                        #force as a matrix in case length(samples)=1
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
                                        #Writes config files for use in meta-analysis and returns a list of run ids.
                                        #Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
                                        #a name to distinguish the output files, and the directory to place the files.
write.ensemble.configs <- function(pft, ensemble.samples, host, outdir, settings,
                                   write.config = write.config.ED, convert.samples=convert.samples.ED){
  
  system(paste('ssh -T ', host$name, 
               ' "rm ', host$rundir, '/*', get.run.id('ENS', '', pft.name=pft.name), '*"', sep=''))
  for(ensemble.id in 1:nrow(ensemble.samples)) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), 
                         pft.name=pft$name)
    unlink(paste(outdir, '/*', run.id, '*', sep=''))
    write.config(pft, convert.samples(ensemble.samples[ensemble.id,]), 
                 settings, outdir, run.id)
  }
  rsync(paste(outdir, '/*', get.run.id('ENS', '', pft.name=pft.name), '*', sep=''), 
        paste(host$name, ':', host$rundir,  sep=''))
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
    quantiles <- append(quantiles, pnorm(1-sigmas))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1-pnorm(-3:3) #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  return(sort(quantiles))
}

                                        #Returns a list of lists representing quantile values of trait distributions,
                                        #given a list of sample distributions for traits and a list of quantiles
                                        #The list is indexed first by trait, then by quantile
get.sa.samples <- function(samples, quantiles){
  sa.samples <- data.frame()
  for(trait in names(samples)){
    for(quantile in quantiles){
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], quantile)
    }
  }
  return(sa.samples)
}




##'
##'
##' Writes config files for use in sensitivity analysis, and returns a list of run ids... content for \details{} ..
##' @title 
##' @param pft a pft.xml object, a list of lists as supplied by get.sa.samples 
##' @param quantile.samples 
##' @param host host where model is run
##' @param outdir directory to place the files.
##' @param settings 
##' @param write.config 
##' @param convert.samples 
##' @return 
write.sa.configs <- function(pft, quantile.samples, host, outdir, settings, 
                             write.config=write.config.ED, convert.samples=convert.samples.ED){
  MEDIAN <- '50'
  traits <- colnames(quantile.samples)
  
  ssh(host$name, 'rm ', host$rundir, '/*', get.run.id('SA', '', pft.name=pft.name, '*'))
  
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
        unlink(paste(outdir, '/*', run.id, '*', sep=''))
        write.config(pft, convert.samples(trait.samples), settings, outdir, run.id)
      }
    }
  }
  rsync(paste(outdir, '/*', get.run.id('SA', '', pft.name=pft.name), '*', sep=''), 
        paste(host$name, ':', host$rundir,  sep=''))
}

