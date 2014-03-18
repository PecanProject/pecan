#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Main driver function to call the ecosystem model specific (e.g. ED, SiPNET) 
##' run and configuration file scripts 
##' 
##' @name run.write.configs
##' @title Run model specific write configuration functions
##' @param model the ecosystem model to generate the configuration files for
##' @param write should the runs be written to the database
##' @export
##'
##' @author David LeBauer, Shawn Serbin
run.write.configs <- function(model, write = TRUE) {

  ## remove previous runs.txt
  if (file.exists(file.path(settings$rundir, "runs.txt"))) {
    logger.warn("Existing runs.txt file will be removed.")
    unlink(file.path(settings$rundir, "runs.txt"))
  }

  load.modelpkg(model)
  ## Check for model-specific write configs

  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    logger.error(my.write.config, "does not exist, please make sure that the package ", 
                 pecan.pkg, "contains a function called",  my.write.config)
  }
  
  # TODO RK : need to write to runs_inputs table
  
  ## Prepare for model output.  Cleanup any old config files (if exists)
  do.call(paste("remove.config", model, sep="."), args = list(settings$rundir, settings))

  ### NEED TO IMPLEMENT: 
  ## Load Environmental Priors and Posteriors
  
  ### Sensitivity Analysis
  if('sensitivity.analysis' %in% names(settings)) {
    
      ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
      quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
      ### Get info on the years to run the sensitivity analysis (if requested)
      sa.years <- data.frame(sa.start = settings$sensitivity.analysis$start.year,
                            sa.end = settings$sensitivity.analysis$end.year)
      
      logger.info("\n Selected Quantiles: ", vecpaste(round(quantiles, 3)))
      
      ### Generate list of sample quantiles for SA run
      sa.samples <-  get.sa.sample.list(pft       = trait.samples, 
                                        env       = env.samples, 
                                        quantiles = quantiles)
      ### Write out SA config files
      if(!exists("cnt")) {            
        cnt <- 0
        assign("cnt", cnt, .GlobalEnv)
      }
      logger.info("\n ----- Writing model run config files ----")
      runs.samples$sa <- write.sa.configs(defaults = settings$pfts,
                                          quantile.samples = sa.samples,
                                          settings = settings,
                                          model = model,
                                          write.to.db = write)
  } ### End of SA
  
  ### Write ENSEMBLE
  if('ensemble' %in% names(settings)){
      if(settings$ensemble$size == 1) {
          ## run at median if only one run in ensemble
          ensemble.samples <- get.sa.sample.list(pft = trait.samples,
                                                 env = env.samples,
                                                 quantiles = 0.5)
      } else if (settings$ensemble$size > 1) {
          
          ## subset the trait.samples to ensemble size using Halton sequence 
          ensemble.samples <- get.ensemble.samples(settings$ensemble$size, 
                                                   trait.samples, env.samples)
      }
          logger.info("Ensemble size: ",settings$ensemble$size)
          
          runs.samples$ensemble <- write.ensemble.configs(defaults = settings$pfts,
                                                          ensemble.samples = ensemble.samples,
                                                          settings = settings,
                                                          model = model,
                                                          write.to.db = write)
  } else {
      logger.info('not writing config files for ensemble, settings are NULL')
  } ### End of Ensemble
  logger.info("\n  ######################## Finished writing model run config files ########################")
}
#==================================================================================================#


##' Convert priors / MCMC samples to chains that can be sampled for model parameters 
##' 
##' @name get.parameter.samples
##' @title Sample from priors or posteriors
##' @param pfts the pfts node of the list of pecan settings
##' @export
##'
##' @author David LeBauer, Shawn Serbin
get.parameter.samples <- function(pfts = settings$pfts){
  
  trait.samples <- sa.samples <- ensemble.samples <- list()
  env.samples <- runs.samples <- ma.samples <- list()
  for (i in seq(pfts)){
    pft.name <- pfts[[i]]$name
    trait.samples[[pft.name]] <- list()
    ma.samples[[pft.name]] <- list()
    ## Load priors
    load(file.path(pfts[[i]]$outdir, 'prior.distns.Rdata'))
    
    ### Load trait mcmc data (if exists)
    if("trait.mcmc.Rdata" %in% dir(pfts[[i]]$outdir)) {
      ma.results <- TRUE
      load(file.path(pfts[[i]]$outdir, 'trait.mcmc.Rdata'))
    }
    

    ### When no ma for a trait, sample from  prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    priors <- rownames(prior.distns)
    if(exists('trait.mcmc')) {
      ma.traits <- names(trait.mcmc)
      samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
      
      ## report which traits use MA results, which use priors 
      if(length(ma.traits) > 0){
        logger.info("PFT",  pft.name, "has meta analysis results for:\n", 
                    paste0(ma.traits, collapse = "\n "))        
      }
      if(!all(priors %in% ma.traits)){
        logger.info("PFT", pft.name, "will use prior distributions for:\n", 
                    paste0(priors[!priors %in% ma.traits], collapse = "\n "))        
      }
    } else {
      ma.traits <- NULL
      samples.num <- 20000
      logger.info("No meta analysis results for PFT",  pft.name)  
      logger.info("PFT", pft.name, "will use prior distributions for", priors )
    }
    
    
    logger.info("using ", samples.num, "samples per trait")
    for (prior in priors) {
      if (prior %in% ma.traits) {
        samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
        ma.samples[[pft.name]][[prior]] <- as.matrix(trait.mcmc[[prior]])
      } else {
        samples <- get.sample(prior.distns[prior,], samples.num)
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
    
  } ### End for loop
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, 
       file = file.path(settings$outdir, 'samples.Rdata'))
}
####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
