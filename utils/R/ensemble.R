#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

##' Reads output from model ensemble
##'
##' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year} 
##' and \code{end.year}
##' @title Read ensemble output
##' @return a list of ensemble model output 
##' @param ensemble.size the number of ensemble members run
##' @param pecandir specifies where pecan writes its configuration files
##' @param outdir directory with model output to use in ensemble analysis
##' @param start.year first year to include in ensemble analysis
##' @param end.year last year to include in ensemble analysis
##' @param variables targe variables for ensemble analysis
##' @export
##' @author Ryan Kelly, David LeBauer, Rob Kooper
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, pecandir, outdir, 
                                 start.year, end.year, variable, ens.run.ids=NULL){
  if (is.null(ens.run.ids)) {
    samples.file <- file.path(pecandir, 'samples.Rdata')
    if(file.exists(samples.file)){
      load(samples.file)
      ens.run.ids <- runs.samples$ensemble
    } else {
      stop(samples.file, "not found required by read.ensemble.output")      
    }
  }

  ensemble.output <- list()
  for(row in rownames(ens.run.ids)) {
    run.id <- ens.run.ids[row, 'id']
    logger.info("reading ensemble output from run id: ", run.id)
    ensemble.output[[row]] <- sapply(read.output(run.id, file.path(outdir, run.id),
                                                 start.year, end.year, variable),
                                     mean,na.rm=TRUE)
  }
  return(ensemble.output)
}
#==================================================================================================#
##' Get parameter values used in ensemble
##'
##' Returns a matrix of randomly sampled trait values 
##' to be assigned to traits over several model runs.
##' given the number of model runs and a list of sample distributions for traits
##' The model run is indexed first by model run, then by trait
##' 
##' @title Get Ensemble Samples
##' @name get.ensemble.samples
##' @param ensemble.size number of runs in model ensemble
##' @param pft.samples random samples from parameter distribution, e.g. from a MCMC chain or a 
##' @param env.samples env samples
##' @param method the method used to generate the ensemble samples.  default = uniform
##' @return matrix of random samples from trait distributions
##' @export
##' @import randtoolbox
##' @references Halton, J. (1964), Algorithm 247: Radical-inverse quasi-random point sequence, 
##' ACM, p. 701, doi:10.1145/355588.365104.
##' @author David LeBauer
get.ensemble.samples <- function(ensemble.size, pft.samples,env.samples,method="uniform") {
  
  if(is.null(method)) {
    logger.info("No sampling method supplied, defaulting to uniform random sampling")
    method="uniform"
  }
  
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
        
    total.sample.num <- sum(sapply(pft.samples, length))
    random.samples <- NULL

      if(method == "halton"){
        logger.info("Using ", method, "method for sampling")
        random.samples <- halton(n = ensemble.size, dim=total.sample.num)
        ##force as a matrix in case length(samples)=1
        random.samples <- as.matrix(random.samples)
      } else if(method == "uniform"){
        logger.info("Using ", method, "random sampling")
        #uniform random
        random.samples <- matrix(runif(ensemble.size*total.sample.num), ensemble.size, total.sample.num)
      } else {
        logger.info("Method ", method, " has not been implemented yet, using uniform random sampling")
        #uniform random
        random.samples <- matrix(runif(ensemble.size*total.sample.num), ensemble.size, total.sample.num)
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
                   random.samples[, col.i])
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
##' @return list, containing $runs = data frame of runids, and $ensemble.id = the ensemble ID for these runs. Also writes sensitivity analysis configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.ensemble.configs <- function(defaults, ensemble.samples, settings,
                                   model, clean=FALSE, write.to.db = TRUE){
  
  my.write.config <- paste("write.config.", model, sep="")

  if(is.null(ensemble.samples)) return(list(runs=NULL, ensemble.id=NULL))
  
  # Open connection to database so we can store all run/ensemble information
  if(write.to.db){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }
  
  # Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  # create an ensemble id
  if (!is.null(con)) {
    # write enseblem first
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste0("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                     now, "', 'ensemble', ", workflow.id, ")"), con=con)
    ensemble.id <- db.query(paste0("SELECT id FROM ensembles WHERE created_at='", now, "' AND runtype='ensemble'"), con=con)[['id']]
    for (pft in defaults) {
      db.query(paste0("INSERT INTO posteriors_ensembles (posterior_id, ensemble_id, created_at, updated_at) values (",
                      pft$posteriorid, ", ", ensemble.id, ", '", now, "', '", now, "');"), con=con)
    }
  } else {
    ensemble.id <- NA
  }

  # find all inputs that have an id
  inputs <- names(settings$run$inputs)
  inputs <- inputs[grepl('.id$', inputs)]
  
  # write configuration for each run of the ensemble
  runs <- data.frame()
  for(counter in 1:settings$ensemble$size) {
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- paste("ensemble=", counter, sep='')
      db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id,",
                       " parameter_list) values ('", 
                       settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", 
                       settings$run$end.date, "', '", settings$run$outdir , "', '", now, "', ", ensemble.id, ", '", 
                       paramlist, "')"), con=con)
      run.id <- db.query(paste0("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'"), con=con)[['id']]

      # associate inputs with runs
      if (!is.null(inputs)) {
        for(x in inputs) {
          db.query(paste0("INSERT INTO inputs_runs (input_id, run_id, created_at) ",
                          "values (", settings$run$inputs[[x]], ", ", run.id, ", NOW());"), con=con)
        }
      }

    } else {
      run.id <- get.run.id('ENS', left.pad.zeros(counter, 5))
    }
    runs[counter, 'id'] <- run.id
    
    # create folders (cleaning up old ones if needed)
    if(clean) {
      unlink(file.path(settings$rundir, run.id))
      unlink(file.path(settings$modeloutdir, run.id))
    }
    dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)
    
    # write run information to disk
    cat("runtype     : ensemble\n",
        "workflow id : ", workflow.id, "\n",
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
    
    do.call(my.write.config, args = list(defaults = defaults,
                                         trait.values = lapply(ensemble.samples, function(x, n){x[n, ]},n = counter),
                                         settings = settings, run.id = run.id))
    cat(run.id, file=file.path(settings$rundir, "runs.txt"), sep="\n", append=TRUE)
  }
  if (!is.null(con)) {
    db.close(con)
  }
  
  invisible(list(runs=runs, ensemble.id=ensemble.id))
} ### End of function: write.ensemble.configs
#==================================================================================================#
