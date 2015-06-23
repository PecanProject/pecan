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
##' @param ens.sample.method how to sample the ensemble members("halton" sequence or "uniform" random)
##' @export
##'
##' @author David LeBauer, Shawn Serbin
run.write.configs <- function(settings, write = TRUE, ens.sample.method="halton") {
  model = settings$model$type
  scipen = getOption("scipen")
  options(scipen=12)
  
  ## remove previous runs.txt
  if (file.exists(file.path(settings$rundir, "runs.txt"))) {
    logger.warn("Existing runs.txt file will be removed.")
    unlink(file.path(settings$rundir, "runs.txt"))
  }

  load.modelpkg(model)
  ## Check for model-specific write configs

  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    logger.error(my.write.config, "does not exist, please make sure that the model package contains a function called",  my.write.config)
  }
  
  # TODO RK : need to write to runs_inputs table
  
  ### Identify PFTs in the input settings.xml file
  num.pfts <- length(settings$pfts)
  pft.names <- list()
  outdirs <- list()
  for (i.pft in 1:num.pfts){
    pft.names[i.pft] <- settings$pfts[i.pft]$pft$name
    
    ### If no PFT(s) are specified insert NULL to warn user 
    if(length(pft.names)==0) pft.names[1] <- "NULL" 
    
    ### Get output directory info
    outdirs[i.pft] <- settings$pfts[i.pft]$pft$outdir
    
  } ### End of for loop to extract pft names
  
  logger.info("Selected PFT(s): ", pft.names)
  
  ## Generate empty list arrays for output.
  trait.samples <- sa.samples <- ensemble.samples <- env.samples <- runs.samples <- list()
  
  ## Prepare for model output.  Cleanup any old config files (if exists)
  my.remove.config <- paste0("remove.config.",model)
  if(exists(my.remove.config)) {
    do.call(my.remove.config, args = list(settings$rundir, settings))
  }

  ## Load PFT priors and posteriors
  for (i in seq(pft.names)){
    ## Load posteriors
    fname = file.path(outdirs[i], 'post.distns.Rdata')
    if(file.exists(fname)){
      load(fname)
      prior.distns = post.distns
    } else {
      load(file.path(outdirs[i], 'prior.distns.Rdata'))
    }

    ### Load trait mcmc data (if exists)
    if("trait.mcmc.Rdata" %in% dir(unlist(outdirs))) {
      ma.results <- TRUE
      load(file.path(outdirs[i], 'trait.mcmc.Rdata'))
    }
    
    pft.name <- unlist(pft.names[i])
    
    ### When no ma for a trait, sample from  prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    priors <- rownames(prior.distns)
    if(exists('trait.mcmc')) {
      ma.traits <- names(trait.mcmc)
      samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
      
      ## report which traits use MA results, which use priors 
      if(length(ma.traits) > 0){
        logger.info("PFT",  pft.names[i], "has MCMC samples for:\n", paste0(ma.traits, collapse = "\n "))        
      }
      if(!all(priors %in% ma.traits)){
        logger.info("PFT", pft.names[i], "will use prior distributions for:\n", paste0(priors[!priors %in% ma.traits], collapse = "\n "))        
      }
    } else {
      ma.traits <- NULL
      samples.num <- 20000
      logger.info("No MCMC results for PFT",  pft.names[i])  
      logger.info("PFT", pft.names[i], "will use prior distributions for", priors )
    }


    logger.info("using ", samples.num, "samples per trait")
    for (prior in priors) {
      if (prior %in% ma.traits) {
        samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
      } else {
        samples <- get.sample(prior.distns[prior,], samples.num)
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
    
  } ### End for loop
  
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
                                                   trait.samples, env.samples, ens.sample.method)
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

  logger.info("###### Finished writing model run config files #####")
  logger.info("config files samples in ", file.path(settings$outdir, "run"))
  
  ### Save output from SA/Ensemble runs
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, 
       file = file.path(settings$outdir, 'samples.Rdata'))
  logger.info("parameter values for runs in ", file.path(settings$outdir, "samples.RData"))
  options(scipen=scipen)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
