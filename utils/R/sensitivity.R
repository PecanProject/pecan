##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##' Reads output of sensitivity analysis runs
##'
##' 
##' @title Read Sensitivity Analysis output 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @param traits model parameters included in the sensitivity analysis
##' @param quantiles quantiles selected for sensitivity analysis
##' @param pecandir specifies where pecan writes its configuration files
##' @param outdir directory with model output to use in sensitivity analysis
##' @param pft.name name of PFT used in sensitivity analysis (Optional)
##' @param start.year first year to include in sensitivity analysis 
##' @param end.year last year to include in sensitivity analysis
##' @param variables variables to be read from model output
##' @export
#--------------------------------------------------------------------------------------------------#
read.sa.output <- function(traits, quantiles, pecandir, outdir, pft.name='', 
                           start.year, end.year, variables){
  
  if (!exists('runs.samples')) {
    samples.file <- file.path(pecandir, 'samples.Rdata')
    if(file.exists(samples.file)){
      load(samples.file)
      sa.runs <- runs.samples$sa
    } else {
      logger.error(samples.file, "not found, this file is required by the read.sa.output function")      
    }
  }
  
  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- sa.runs[[pft.name]][quantile, trait]
      out <- read.output(run.id, file.path(outdir, run.id),
                         start.year, end.year, variables)
      sa.output[quantile, trait] <- sapply(out, mean, na.rm=TRUE)
    } ## end loop over quantiles
    logger.info("reading sensitivity analysis output for model run at ", quantiles, "quantiles of trait", trait)
  } ## end loop over traits
  sa.output <- as.data.frame(sa.output)
  return(sa.output)
}
##==================================================================================================#

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
##' @return list, containing $runs = data frame of runids, and $ensemble.id = the ensemble ID for these runs. Also writes sensitivity analysis configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.sa.configs <- function(defaults, quantile.samples, settings, model,
                             clean=FALSE, write.to.db = TRUE){
  scipen = getOption("scipen")
  options(scipen=12)
  
  my.write.config <- paste("write.config.", model,sep="")
  
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

  # find all inputs that have an id
  inputs <- names(settings$run$inputs)
  inputs <- inputs[grepl('.id$', inputs)]
    
  runs <- data.frame()
  
  ##write median run
  MEDIAN <- '50'
  median.samples <- list()
  for(i in 1:length(quantile.samples)){
    median.samples[[i]] <- quantile.samples[[i]][MEDIAN,]
  }
  names(median.samples) <- names(quantile.samples)
  
  if (!is.null(con)) {
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste0("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", now, "', 'sensitivity analysis', ", format(workflow.id,scientific=FALSE), ")"), con=con)
    ensemble.id <- db.query(paste0("SELECT id FROM ensembles WHERE created_at='", now, "' AND runtype='sensitivity analysis'"), con=con)[['id']]
    paramlist <- paste0("quantile=MEDIAN,trait=all,pft=", paste(lapply(settings$pfts, function(x) x[['name']]), sep=','))
    db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id, parameter_list) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$run$outdir , "', '", now, "', ", ensemble.id, ", '", paramlist, "')"), con=con)
    run.id <- db.query(paste0("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'"), con=con)[['id']]

    # associate inputs with runs
    if (!is.null(inputs)) {
      for(x in inputs) {
        db.query(paste0("INSERT INTO inputs_runs (input_id, run_id, created_at) ",
                        "values (", settings$run$inputs[[x]], ", ", run.id, ", NOW());"), con=con)
      }
    }
  } else {
    run.id <- get.run.id('SA', 'median')
    ensemble.id <- "NA"
  }
  medianrun <- run.id
  
  # create folders (cleaning up old ones if needed)
  if(clean) {
    unlink(file.path(settings$rundir, run.id))
    unlink(file.path(settings$modeloutdir, run.id))
  }
  dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
  dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)
  
  # write run information to disk
  ## TODO need to print list of pft names and trait names
  cat("runtype     : sensitivity analysis\n",
      "workflow id : ", workflow.id, "\n",
      "ensemble id : ", ensemble.id, "\n",
      "pft name    : ALL PFT", "\n",
      "quantile    : MEDIAN\n",
      "trait       : ALL TRAIT", "\n",
      "run id      : ", run.id, "\n",
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
  runs <- list()
  for(i in seq(names(quantile.samples))){
    pftname <- names(quantile.samples)[i]
    if (pftname == "env") {
      next
    }
    
    traits <- colnames(quantile.samples[[i]])
    quantiles.str <- rownames(quantile.samples[[i]])
    
    runs[[pftname]] <- data.frame()
    
    ## loop over variables
    for (trait in traits) {
      for(quantile.str in quantiles.str) {
        if (quantile.str != MEDIAN) {
          quantile <- as.numeric(quantile.str)/100
          trait.samples <- median.samples
          trait.samples[[i]][trait] <- quantile.samples[[i]][quantile.str, trait]
          
          if (!is.null(con)) {
            now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            paramlist <- paste0("quantile=", quantile.str, ",trait=", trait, ",pft=", pftname)
            db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id, parameter_list) values ('", settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$run$outdir , "', '", now, "', ", ensemble.id, ", '", paramlist, "')"), con=con)
            run.id <- db.query(paste0("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'"), con=con)[['id']]

            # associate inputs with runs
            if (!is.null(inputs)) {
              for(x in inputs) {
                db.query(paste0("INSERT INTO inputs_runs (input_id, run_id, created_at) ",
                                "values (", settings$run$inputs[[x]], ", ", run.id, ", NOW());"), con=con)
              }
            }
          } else { 
            run.id <- get.run.id('SA', round(quantile,3), trait=trait, 
                                 pft.name=names(trait.samples)[i])
          }
          runs[[pftname]][quantile.str, trait] <- run.id
          
          # create folders (cleaning up old ones if needed)
          if(clean) {
            unlink(file.path(settings$rundir, run.id))
            unlink(file.path(settings$modeloutdir, run.id))
          }
          dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
          dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)
          
          # write run information to disk
          cat("runtype     : sensitivity analysis\n",
              "workflow id : ", workflow.id, "\n",
              "ensemble id : ", ensemble.id, "\n",
              "pft name    : ", names(trait.samples)[i], "\n",
              "quantile    : ", quantile.str, "\n",
              "trait       : ", trait, "\n",
              "run id      : ", run.id, "\n",
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
        } else {
          runs[[pftname]][MEDIAN, trait] <- medianrun
        }
      }
    }
  }
  if (!is.null(con)) {
    db.close(con)
  }
  options(scipen=scipen)
  invisible(list(runs=runs, ensemble.id=ensemble.id))
}
#==================================================================================================#
