#--------------------------------------------------------------------------------------------------#
### TODO: Generalize this code for all ecosystem models (e.g. ED2.2, SiPNET, etc).
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Get parameter values used in ensemble
##'
##' Returns a matrix of trait values sampled quasi-randomly based on the Halton sequence
##' to be assigned to traits over several model runs.
##' given the number of model runs and a list of sample distributions for traits
##' The model run is indexed first by model run, then by trait
##' 
##' @title Get Ensemble Samples
##' @name get.ensemble.samples
##' @param ensemble.size number of runs in model ensemble
##' @param pft.samples random samples from parameter distribution, e.g. from a MCMC chain or a 
##' @param env.samples env samples
##' @param method the method used to generate the ensemble samples.  default = halton
##' @return matrix of quasi-random (overdispersed) samples from trait distributions
##' @export
##' @import randtoolbox
##' @references Halton, J. (1964), Algorithm 247: Radical-inverse quasi-random point sequence, ACM, p. 701, doi:10.1145/355588.365104.
##' @author David LeBauer
get.ensemble.samples <- function(ensemble.size, pft.samples,env.samples,method="halton") {
  ##force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  if(ensemble.size <= 0){
    ans <- NULL
  } else if (ensemble.size == 1) {
    ans <- get.sa.sample.list(pft.samples,env.samples,0.50)
  } else {
    pft.samples[[length(pft.samples)+1]] = env.samples
    names(pft.samples)[length(pft.samples)] <- 'env'
    pft2col <- NULL
    for(i in 1:length(pft.samples)){
      pft2col <- c(pft2col,rep(i,length(pft.samples[[i]])))
    }

    halton.samples <- NULL
    if(method == "halton"){
      halton.samples <- halton(n = ensemble.size, dim=length(pft2col))
      ##force as a matrix in case length(samples)=1
      halton.samples <- as.matrix(halton.samples)
    } else {
      #uniform random
      halton.samples <- matrix(runif(ensemble.size*length(pft2col))
                                ,ensemble.size,length(pft2col))
      
    }
    
    total.sample.num <- sum(sapply(pft.samples, length))
    halton.samples <- NULL
    if(method == "halton"){
      halton.samples <- halton(n = ensemble.size, dim=total.sample.num)
      ##force as a matrix in case length(samples)=1
      halton.samples <- as.matrix(halton.samples)
    } else {
      #uniform random
      halton.samples <- matrix(runif(ensemble.size*total.sample.num),
          ensemble.size, dim=total.sample.num)
    }
    
    ensemble.samples <- list()
    
    col.i <- 0
    for(pft.i in seq(pft.samples)){
      ensemble.samples[[pft.i]] <-
          matrix(nrow=ensemble.size,ncol=length(pft.samples[[pft.i]]))
      for(trait.i in seq(pft.samples[[pft.i]])) {
        col.i<-col.i+1
        ensemble.samples[[pft.i]][, trait.i] <- 
            quantile(pft.samples[[pft.i]][[trait.i]],
                     halton.samples[, col.i])
      } # end trait
      ensemble.samples[[pft.i]] <- as.data.frame(ensemble.samples[[pft.i]])
      colnames(ensemble.samples[[pft.i]]) <- names(pft.samples[[pft.i]])
    } #end pft
    names(ensemble.samples) <- names(pft.samples)
    ans <- ensemble.samples
  }
  return(ans)
}  ### End of function: get.ensemble.samples
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Write ensemble config files
##'
##' Writes config files for use in meta-analysis and returns a list of run ids.
##' Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
##' a name to distinguish the output files, and the directory to place the files.
##' @title Write ensemble configs 
##' @param defaults pft
##' @param ensemble.samples list of lists supplied by \link{get.ensemble.samples}
##' @param host server to which config files will be sent
##' @param outdir directory for model output (on server)
##' @param settings list of PEcAn settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param clean remote old output first?
##' @return nothing, writes ensemble configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.ensemble.configs <- function(defaults, ensemble.samples,
                                   host, outdir, settings,
                                   model="ED2",clean=FALSE){

  my.write.config <- paste("write.config",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    exit()
  }

  
  if(clean){
    ## Remove old files
    if(host$name == 'localhost') {
      if("ENS" %in% dir(host$rundir)){
        file.remove(paste(host$rundir, '*',
                          get.run.id('ENS', '', pft.name=pft.name), '*"', sep=''))
      }
    } else {
      ssh(host$name, 'rm -f ', host$rundir, '*',
          get.run.id('ENS', '', pft.name=pft.name, '*'))
    }
  }
  
  if(is.null(ensemble.samples)) return(NULL)
  for(ensemble.id in 1:settings$ensemble$size) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5))
    if(clean) unlink(paste(outdir, '*', run.id, '*', sep=''))
    
    do.call(my.write.config,args=list(defaults,
                 lapply(ensemble.samples,function(x,n){x[n,]},n=ensemble.id),
                 settings, outdir, run.id))
  }
  if(host$name == 'localhost'){
    rsync(paste(outdir, '*',
                get.run.id('ENS', ''), '*', sep=''),
          host$rundir)
  } else {
    system(paste('rsync -routi ',
                 paste(outdir, '*', get.run.id('ENS', ''), '*', sep=''), 
                 paste(host$name, ':', host$rundir,  sep=''), sep = ' '))
  }
} ### End of function: write.ensemble.configs
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Returns a vector of quantiles specified by a given <quantiles> xml tag
##'
##' @title Get Quantiles  
##' @param quantiles.tag specifies tag used to specify quantiles
##' @return vector of quantiles
##' @export
##' @author David LeBauer
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
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' @name get.sa.sample.list
##' @title Get Sensitivity Analysis samples as list
##' @param pft 
##' @param env 
##' @param quantiles
##' @export
##' @author unknown
get.sa.sample.list <- function(pft,env,quantiles){
  sa.sample.list <- list()
  for(i in 1:length(pft)){
    sa.sample.list[[i]] = get.sa.samples(pft[[i]],quantiles)
  }
  sa.sample.list[[length(pft)+1]] <- get.sa.samples(env,quantiles)
  names(sa.sample.list) <- c(names(pft),"env")
  return(sa.sample.list)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
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
##' @export
##' @author David LeBauer
get.sa.samples <- function(samples, quantiles){
  sa.samples <- data.frame()
  for(trait in names(samples)){
    for(quantile in quantiles){
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], 
                                                                         quantile)
    }
  }
  return(sa.samples)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Write sensitivity analysis config files
##'
##' Writes config files for use in sensitivity analysis.
##' @title Write sensitivity analysis configs
##' @param pft pft id used to query PEcAn database
##' @param quantile.samples 
##' @param host server to which config files will be sent
##' @param outdir directory for model output (on server)
##' @param settings list of settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param convert.samples a model-specific function that transforms variables from units used in database to units used by model, e.g. \link{convert.samples.ED} 
##' @param ensemble.samples list of lists supplied by \link{get.sa.samples}
##' @return nothing, writes sensitivity analysis configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.sa.configs <- function(defaults, quantile.samples, host, outdir, settings, 
                             model="ED2",clean=FALSE){

  my.write.config <- paste("write.config",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    exit()
  }

  
  MEDIAN <- '50'
  ## clean out old files
  if(clean){
    if(host$name == 'localhost'){
      if("SA" %in% dir(host$rundir)){
        file.remove(paste(host$rundir, '*',
                          get.run.id('SA', ''), '*', sep=''))
      }
    } else {
      ssh(host$name, 'rm -f ', host$rundir, '*',
          get.run.id('SA', '', '*'))
    }
  }
  
  ##write median run
  median.samples <- list()
  for(i in 1:length(quantile.samples)){
    median.samples[[i]] <- quantile.samples[[i]][MEDIAN,]
  }
  names(median.samples) = names(quantile.samples)
  run.id <- get.run.id('SA', 'median')
  do.call(my.write.config,list(defaults, median.samples, settings, outdir, run.id))
  
  ## loop over pfts
  for(i in seq(names(quantile.samples))){
    
    traits <- colnames(quantile.samples[[i]])
    quantiles.str <- rownames(quantile.samples[[i]])
    
    ## loop over variables
    for (trait in traits) {
      for(quantile.str in quantiles.str) {
        if (quantile.str != MEDIAN) {
          quantile <- as.numeric(quantile.str)/100
          trait.samples <- median.samples
          trait.samples[[i]][trait] <- quantile.samples[[i]][quantile.str, trait]
          run.id <- get.run.id('SA', round(quantile,3), trait=trait, 
                               pft.name=names(trait.samples)[i])
          if(clean){unlink(paste(outdir, '*', run.id, '*', sep=''))}
          do.call(my.write.config,list(defaults, trait.samples, settings, outdir, run.id))
        }
      }
    }
  }
  if(host$name == 'localhost'){
    rsync('-outi', from = outdir, to = host$rundir, 
          pattern = paste('*', get.run.id('SA', ''), '*',sep='') )
  } else {
    # rsync(args, from, to, pattern).  pattern --> file patter for rsync
    rsync('-outi', from = outdir, to = paste(host$name, ':', host$rundir,  sep=''), 
          pattern = paste('*', get.run.id('SA', ''), '*',sep='') )
  }
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'   Counter function for writing configs
##' 
##' @title counter 
##' @param cnt 
##' @return updated value of cnt to global environment
##' @export
counter <- function(cnt){
  cnt = cnt + 1
  #return(cnt)
  assign("cnt",cnt,.GlobalEnv) # Assign count to the environment
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
