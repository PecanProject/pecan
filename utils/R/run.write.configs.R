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
run.write.configs <- function(settings, write = TRUE) {
  model = settings$model$type
  scipen = getOption("scipen")
  options(scipen=12)
  get.parameter.samples(pfts = settings$pfts)
  load(file.path(settings$outdir, "samples.Rdata"))

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

  ## Prepare for model output.  Cleanup any old config files (if exists)
  my.remove.config <- paste0("remove.config.",model)
  if(exists(my.remove.config)) {
    do.call(my.remove.config, args = list(settings$rundir, settings))
  }
  
  
  # TODO RK : need to write to runs_inputs table
  
  ### NEED TO IMPLEMENT: 
  ## Load Environmental Priors and Posteriors
  
  ### Sensitivity Analysis
  if('sensitivity.analysis' %in% names(settings)) {

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
  env.samples <- list()
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
       file = file.path(settings$outdir, 'samples.Rdata'))
  logger.info("parameter values for runs in ", file.path(settings$outdir, "samples.RData"))
  options(scipen=scipen)
}
#==================================================================================================#

 
####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
