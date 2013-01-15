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
##' @export
##'
##' @author David LeBauer, Shawn Serbin
run.write.configs <- function(model=settings$model$name){
  if (file.exists(file.path(settings$rundir, "runs.txt"))) {
    log.warn("Existing runs.txt file will be removed.")
    unlink(file.path(settings$rundir, "runs.txt"))
  }

  # TODO RK : need to write to runs_inputs table
  
  ### Identify PFTs in the input settings.xml file
  num.pfts <- length(settings$pfts)
  pft.names <- list()
  outdirs <- list()
  for (i in 1:num.pfts){
    pft.names[i] <- settings$pfts[i]$pft$name
    
    ### If no PFT(s) are specified insert NULL to warn user 
    if(length(pft.names)==0) pft.names[1] <- "NULL" 
    ###
    
    ### Get output directory info
    outdirs[i] <- settings$pfts[i]$pft$outdir
    
  } ### End of for loop to extract pft names
  
  print(" ")
  print("-------------------------------------------------------------------")
  print("Selected PFT(s): ")
  print(pft.names)
  print("-------------------------------------------------------------------")
  print(" ")
  
  ### Generate empty list arrays for output.
  trait.samples <- list()
  sa.samples <- list()
  ensemble.samples <- list()
  env.samples <- list()
  ###
  
  ## Prepare for model output.  Cleanup any old config files (if exists)
  #remove.config(settings$rundir,settings,model)
  do.call(paste("remove.config", model, sep="."),
          args = list(settings$rundir, settings))

  ## Load PFT priors and posteriors
  for (i in seq(pft.names)){

    ## Load priors
    load(file.path(outdirs[i], 'prior.distns.Rdata'))
    
    ### Load trait mcmc data (if exists)
    if("trait.mcmc.Rdata" %in% dir(unlist(outdirs))) {
      load(file.path(outdirs[i], 'trait.mcmc.Rdata'))
    }
    
    pft.name <- unlist(pft.names[i])
    
    ### When no ma for a trait, sample from  prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    if(exists('trait.mcmc')) {
      print(names(trait.mcmc))
      traits <- names(trait.mcmc)
      samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
    } else {
      traits <- NA
      samples.num <- 20000
    }
    priors <- rownames(prior.distns)
    print(priors)
    for (prior in priors) {
      if (prior %in% traits) {
        samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
      } else {
        samples <- get.sample(prior.distns[prior,], samples.num)
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
    
  } ### End for loop
  
  ### NEED TO IMPLEMENT: 
  ## Load Environmental Priors and Posteriors
  ###
  
  ### Sensitivity Analysis
  if('sensitivity.analysis' %in% names(settings)) {
    
    if(is.null(settings$sensitivity.analysis)) {
      print(paste('sensitivity analysis settings are NULL'))
    } else {      
      ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
      quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
      ### Get info on the years to run the sensitivity analysis (if requested)
      sa.years <- data.frame(sa.start = settings$sensitivity.analysis$start.year, 
                            sa.end = settings$sensitivity.analysis$end.year)
      
      print(" ")
      print(" ")
      print("-------------------------------------------------------------------")
      print("Selected Quantiles: ")
      print(round(quantiles, 3))
      print("-------------------------------------------------------------------")
      print(" ")
      print(" ")
      
      ### Generate list of sample quantiles for SA run
      sa.samples <-  get.sa.sample.list(trait.samples, 
                                        env.samples,
                                        quantiles)
      ### Write out SA config files
      if(!exists("cnt")) {            
        cnt <- 0
        assign("cnt", cnt, .GlobalEnv)
      }
      write.sa.configs(defaults = settings$pfts,
                       quantile.samples = sa.samples,
                       settings = settings,
                       model = model)
    }
  } ### End of SA
  
  ### Write ENSEMBLE
  if('ensemble' %in% names(settings) && settings$ensemble$size > 0) {
    
    ## subset the trait.samples to ensemble size using Halton sequence 
    ensemble.samples <- get.ensemble.samples(settings$ensemble$size, 
                                             trait.samples, env.samples)
    
    print(" ")
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste("Ensemble size: ",settings$ensemble$size))
    print("-------------------------------------------------------------------")
    print(" ")
    print(" ")
    
    write.ensemble.configs(settings$pfts, ensemble.samples, settings, model = model)
    
  }else{
    print(paste('Ensemble analysis settings are NULL'))
  } ### End of Ensemble

  print("  ######################## Finish up runs ########################")
  ### Save output from SA/Ensemble runs
  save(ensemble.samples, trait.samples, sa.samples, file = paste(settings$outdir, 'samples.Rdata', sep = ''))

  if (FALSE) {
    # TODO RK : move this to run model, why copy before the model is executed.  
    # copy all run files to remote host
    if(settings$run$host$name == 'localhost'){
  #    rsync('-outi', from = outdir, to = settings$rundir, 
  #          pattern = paste('*', get.run.id('SA', ''), '*',sep='') )
    } else {
      # rsync(args, from, to, pattern).  pattern --> file patter for rsync
      rsync('-outi', from = settings$rundir, to = paste(settings$run$host$name, ':', settings$run$host$rundir,  sep=''), 
            pattern = paste('*', get.run.id('SA', ''), '*',sep='') )
    }

    
    ### Make outdirectory, send samples to outdir
    print(settings$run$host$name)
    if(settings$run$host$name == 'localhost'){
      print(c(settings$run$host$outdir,"move to",settings$outdir))
      if(!(settings$run$host$outdir == settings$outdir)) {
        dir.create(settings$run$host$outdir,showWarnings=FALSE)
        file.copy(from = paste(settings$outdir, 'samples.Rdata', sep = ''),
                  to   = paste(settings$run$host$outdir, 'samples.Rdata', sep = '/'),
                  overwrite = TRUE)
      }
    } else {  
      mkdir.cmd <- paste("'if ! ls ", settings$run$host$outdir, " > /dev/null ; then mkdir -p ", 
                         settings$run$host$outdir," ; fi'",sep = '')
      system(paste("ssh", settings$run$host$name, mkdir.cmd))
      system(paste('rsync -routi ', paste(settings$rundir, 'samples.Rdata', sep=''),
                   paste(settings$run$host$name, ':', settings$run$host$outdir, sep = '')))
    }
  }
  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
