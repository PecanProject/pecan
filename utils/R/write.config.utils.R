#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
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
##' @param settings list of PEcAn settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param clean remove old output first?
##' @return nothing, writes ensemble configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.ensemble.configs <- function(defaults, ensemble.samples, settings,
                                   model, clean=FALSE, write.to.db = TRUE){

  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }

  # TODO RK : fix this since names have changed
  if(clean){
    ## Remove old files
    if(settings$run$host$name == 'localhost') {
      if("ENS" %in% dir(settings$run$host$rundir)){
        file.remove(paste(settings$run$host$rundir, '*',
                          get.run.id('ENS', '', pft.name=pft.name), '*"', sep=''))
      }
    } else {
      ssh(settings$run$host$name, 'rm -f ', settings$run$host$rundir, '*',
          get.run.id('ENS', '', pft.name=pft.name, '*'))
    }
  }
  
  if(is.null(ensemble.samples)) return(NULL)

  # Open connection to database so we can store all run/ensemble information
  if(write.to.db){
    con <- try(query.base.con(settings), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    log.warn("Run provenance not being logged by database")
    con <- NULL
  }

  # create an ensemble id
  if (!is.null(con)) {
    # write enseblem first
# TODO add column to ensemble with workflow_id
#      if ("workflow" %in% names(settings)) {
#        query.base(paste("INSERT INTO workflows_runs values ('", settings$workflow$id, "', '", run.id, "')", sep=''), con)
#      }
    query.base(paste("INSERT INTO ensembles (created_at, runtype) values (NOW(), 'ensemble')", sep=''), con)
    ensemble.id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
  }

  # write configuration for each run of the ensemble
  for(counter in 1:settings$ensemble$size) {
    if (!is.null(con)) {
      query.base(paste("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$run$outdir , "', NOW(), ", ensemble.id, ")", sep=''), con)
      run.id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
    } else {
      run.id <- get.run.id('ENS', left.pad.zeros(counter, 5))
    }

    # create folders (cleaning up old ones if needed)
    if(clean) {
      unlink(file.path(settings$rundir, run.id))
      unlink(file.path(settings$modeloutdir, run.id))
    }
    dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)
    
    # write run information to disk
    cat("runtype     : ensemble\n",
        "ensemble id : ", ensemble.id, "\n",
        "run         : ", counter, "/", settings$ensemble$size, "\n",
        "run id      : ", run.id, "\n",
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
        "model       : ", model, "\n",
        "model id    : ", settings$model$id, "\n",
        "site        : ", settings$run$site$name, "\n",
        "site  id    : ", settings$run$site$id, "\n",
        "met data    : ", settings$run$site$met, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$run$host$name, "\n",
        "rundir      : ", file.path(settings$run$host$rundir, run.id), "\n",
        "outdir      : ", file.path(settings$run$host$outdir, run.id), "\n",
        file=file.path(settings$rundir, run.id, "README.txt"), sep='')

    do.call(my.write.config,args=list(defaults,
                 lapply(ensemble.samples,function(x,n){x[n,]},n=counter),
                 settings, run.id))
    cat(run.id, file=file.path(settings$rundir, "runs.txt"), sep="\n", append=TRUE)
  }
  if (!is.null(con)) {
    dbDisconnect(con)
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
##==================================================================================================#

##' get sensitivity samples as a list
##'
##' @title get.sa.sample.list 
##' @param pft Plant Functional Type
##' @param env 
##' @param quantiles quantiles at which to obtain samples from parameter for
##' sensitivity analysis
##' @export
##' @return sa.sample.list
get.sa.sample.list <- function(pft, env, quantiles){
  sa.sample.list <- list()
  for(i in 1:length(pft)){
    sa.sample.list[[i]] = get.sa.samples(pft[[i]], quantiles)
  }
  sa.sample.list[[length(pft)+1]] <- get.sa.samples(env, quantiles)
  names(sa.sample.list) <- c(names(pft), "env")
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
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], quantile)
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
##' @param settings list of settings
##' @param write.config a model-specific function to write config files, e.g. \link{write.config.ED}  
##' @param convert.samples a model-specific function that transforms variables from units used in database to units used by model, e.g. \link{convert.samples.ED} 
##' @param ensemble.samples list of lists supplied by \link{get.sa.samples}
##' @return nothing, writes sensitivity analysis configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.sa.configs <- function(defaults, quantile.samples, settings, model,
                             clean=FALSE, write.to.db = TRUE){

  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
  
  ## clean out old files
  if(clean){
    if(settings$run$host$name == 'localhost'){
      if("SA" %in% dir(settings$run$host$rundir)){
        file.remove(paste(settings$run$host$rundir, '*',
                          get.run.id('SA', ''), '*', sep=''))
      }
    } else {
      ssh(settings$run$host$name, 'rm -f ', settings$run$host$rundir, '*',
        get.run.id('SA', '', '*'))
    }
  }

  if(write.to.db){
    con <- try(query.base.con(settings), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    log.warn("Run provenance not being logged by database")
    con <- NULL
  }
 
  ##write median run
  MEDIAN <- '50'
  median.samples <- list()
  for(i in 1:length(quantile.samples)){
    median.samples[[i]] <- quantile.samples[[i]][MEDIAN,]
  }
  names(median.samples) <- names(quantile.samples)

  if (!is.null(con)) {
# TODO add column to ensemble with workflow_id
#    if ("workflow" %in% names(settings)) {
#      query.base(paste("INSERT INTO workflows_runs values ('", settings$workflow$id, "', '", run.id, "')", sep=''), con)
#    }
    query.base(paste("INSERT INTO ensembles (created_at, runtype) values (NOW(), 'sensitivity analysis')", sep=''), con)
    ensemble.id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
    query.base(paste("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$run$outdir , "', NOW(), ", ensemble.id, ")", sep=''), con)
    run.id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
  } else {
    run.id <- get.run.id('SA', 'median')
    ensemble.id <- "NA"
  }

  # create folders (cleaning up old ones if needed)
  if(clean) {
    unlink(file.path(settings$rundir, run.id))
    unlink(file.path(settings$modeloutdir, run.id))
  }
  dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
  dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)

  # write run information to disk
  cat("runtype     : sensitivity analysis\n",
      "ensemble id : ", ensemble.id, "\n",
      "quantile    : MEDIAN\n",
      "run id      : ", run.id, "\n",
      "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
      "model       : ", model, "\n",
      "model id    : ", settings$model$id, "\n",
      "site        : ", settings$run$site$name, "\n",
      "site  id    : ", settings$run$site$id, "\n",
      "met data    : ", settings$run$site$met, "\n",
      "start date  : ", settings$run$start.date, "\n",
      "end date    : ", settings$run$end.date, "\n",
      "hostname    : ", settings$run$host$name, "\n",
      "rundir      : ", file.path(settings$run$host$rundir, run.id), "\n",
      "outdir      : ", file.path(settings$run$host$outdir, run.id), "\n",
      file=file.path(settings$rundir, run.id, "README.txt"), sep='')

  # write configuration
  do.call(my.write.config, args=list(defaults = defaults,
                             trait.values = median.samples,
                             settings = settings,
                             run.id = run.id))
  cat(run.id, file=file.path(settings$rundir, "runs.txt"), sep="\n", append=TRUE)

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

          if (!is.null(con)) {
            query.base(paste("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$run$host$outdir , "', NOW(), ", ensemble.id, ")", sep=''), con)
            run.id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
          } else { 
            run.id <- get.run.id('SA', round(quantile,3), trait=trait, 
                                 pft.name=names(trait.samples)[i])
          } 

          # create folders (cleaning up old ones if needed)
          if(clean) {
            unlink(file.path(settings$rundir, run.id))
            unlink(file.path(settings$modeloutdir, run.id))
          }
          dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
          dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)

          # write run information to disk
          cat("runtype     : sensitivity analysis\n",
              "ensemble id : ", ensemble.id, "\n",
              "pft name    : ", names(trait.samples)[i], "\n",
              "quantile    : ", quantile.str, "\n",
              "trait       : ", trait, "\n",
              "run id      : ", run.id, "\n",
              "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
              "model       : ", model, "\n",
              "model id    : ", settings$model$id, "\n",
              "site        : ", settings$run$site$name, "\n",
              "site  id    : ", settings$run$site$id, "\n",
              "met data    : ", settings$run$site$met, "\n",
              "start date  : ", settings$run$start.date, "\n",
              "end date    : ", settings$run$end.date, "\n",
              "hostname    : ", settings$run$host$name, "\n",
              "rundir      : ", file.path(settings$run$host$rundir, run.id), "\n",
              "outdir      : ", file.path(settings$run$host$outdir, run.id), "\n",
              file=file.path(settings$rundir, run.id, "README.txt"), sep='')

          # write configuration
          do.call(my.write.config,
                  args = list(defaults = defaults,
                    trait.values = trait.samples,
                    settings = settings, run.id))
          cat(run.id, file=file.path(settings$rundir, "runs.txt"), sep="\n", append=TRUE)
        }
      }
    }
  }
  if (!is.null(con)) {
    dbDisconnect(con)
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
