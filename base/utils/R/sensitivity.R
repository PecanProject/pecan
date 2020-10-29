#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
##' @param per.pft flag to determine whether we want SA on pft-specific variables
##' @export
##' @importFrom magrittr %>%
##' @author Ryan Kelly, David LeBauer, Rob Kooper, Mike Dietze, Istem Fer
#--------------------------------------------------------------------------------------------------#
##' @author Ryan Kelly, David LeBauer, Rob Kooper, Mike Dietze
read.sa.output <- function(traits, quantiles, pecandir, outdir, pft.name = "", 
                           start.year, end.year, variable, sa.run.ids = NULL, per.pft = FALSE) {
  
  
  if (is.null(sa.run.ids)) {
    samples.file <- file.path(pecandir, "samples.Rdata")
    if (file.exists(samples.file)) {
      load(samples.file)
      sa.run.ids <- runs.samples$sa
    } else {
      PEcAn.logger::logger.error(samples.file, "not found, this file is required by the read.sa.output function")
    }
  }
  
  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))
  
  expr <- variable$expression
  variables <- variable$variables
  
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- sa.run.ids[[pft.name]][quantile, trait]
      
      for(var in seq_along(variables)){
        # if SA is requested on a variable available per pft, pass pft.name to read.output
        # so that it only returns values for that pft
        pass_pft <- switch(per.pft + 1, NULL, pft.name) 
        out.tmp <- read.output(runid = run.id, outdir = file.path(outdir, run.id), 
                               start.year = start.year, end.year = end.year, 
                               variables = variables[var], 
                               pft.name = pass_pft)
        assign(variables[var], out.tmp[[variables[var]]])
      }
      
      # derivation
      out <- eval(parse(text = expr))
      
      sa.output[quantile, trait] <- mean(out, na.rm=TRUE)

    } ## end loop over quantiles
    PEcAn.logger::logger.info("reading sensitivity analysis output for model run at ", quantiles, "quantiles of trait", trait)
  } ## end loop over traits
  sa.output <- as.data.frame(sa.output)
  return(sa.output)
} # read.sa.output


##' Write sensitivity analysis config files
##'
##' Writes config files for use in sensitivity analysis.
##'
##' @param defaults named list with default parameter values
##' @param quantile.samples list of lists supplied by \link{get.sa.samples}
##' @param settings list of settings
##' @param model name of model to be run
##' @param clean logical: Delete any existing contents of the directory specified by \code{settings$rundir} before writing to it?
##' @param write.to.db logical: Record this run to BETY? If TRUE, uses connection settings specified in \code{settings$database}
##'
##' @return list, containing $runs = data frame of runids, and $ensemble.id = the ensemble ID for these runs. Also writes sensitivity analysis configuration files as a side effect
##' @export
##' @author David LeBauer, Carl Davidson
write.sa.configs <- function(defaults, quantile.samples, settings, model,
                             clean = FALSE, write.to.db = TRUE) {
  scipen <- getOption("scipen")
  options(scipen = 12)
  my.write.config <- paste("write.config.", model, sep = "")
  
  if (write.to.db) {
    con <- try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(PEcAn.DB::db.close(con), add = TRUE)
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
  inputs <- inputs[grepl(".id$", inputs)]
  
  runs <- data.frame()
  
  # Reading the site.pft specific tags from xml
  site.pfts.vec <- settings$run$site$site.pft %>% unlist %>% as.character
  
  if(!is.null(site.pfts.vec)){
    # find the name of pfts defined in the body of pecan.xml
    defined.pfts <- settings$pfts %>% purrr::map('name') %>% unlist %>% as.character
    # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
    if (length(which(site.pfts.vec %in% defined.pfts)) > 0 )
      quantile.samples <- quantile.samples [site.pfts.vec[ which(site.pfts.vec %in% defined.pfts) ]]
    # warn if there is a pft specified in the site but it's not defined in the pecan xml.
    if (length(which(!(site.pfts.vec %in% defined.pfts)))>0) 
      PEcAn.logger::logger.warn(paste0("The following pfts are specified for the siteid ", settings$run$site$id ," but they are not defined as a pft in pecan.xml:",
                                       site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))]))
  }
  
  
  ## write median run
  MEDIAN <- "50"
  median.samples <- list()
  for (i in seq_along(quantile.samples)) {
    median.samples[[i]] <- quantile.samples[[i]][MEDIAN, , drop=FALSE]
  }
  names(median.samples) <- names(quantile.samples)

  if (!is.null(con)) {
    ensemble.id <- PEcAn.DB::db.query(paste0(
      "INSERT INTO ensembles (runtype, workflow_id) ",
      "VALUES ('sensitivity analysis', ", format(workflow.id, scientific = FALSE), ") ",
      "RETURNING id"), con = con)[['id']]
      
    paramlist <- paste0("quantile=MEDIAN,trait=all,pft=",
                        paste(lapply(settings$pfts, function(x) x[["name"]]), sep = ","))
    run.id <- PEcAn.DB::db.query(paste0("INSERT INTO runs ",
      "(model_id, site_id, start_time, finish_time, outdir, ensemble_id, parameter_list) ",
      "values ('", 
        settings$model$id, "', '", 
        settings$run$site$id, "', '", 
        settings$run$start.date, "', '", 
        settings$run$end.date, "', '", 
        settings$run$outdir, "', ", 
        ensemble.id, ", '", 
        paramlist, "') ",
      "RETURNING id"), con = con)[['id']]
    
    # associate posteriors with ensembles
    for (pft in defaults) {
      PEcAn.DB::db.query(paste0(
        "INSERT INTO posteriors_ensembles (posterior_id, ensemble_id) ",
        "values (", pft$posteriorid, ", ", ensemble.id, ")"), con = con)
    }
    
    # associate inputs with runs
    if (!is.null(inputs)) {
      for (x in inputs) {
        PEcAn.DB::db.query(paste0(
          "INSERT INTO inputs_runs (input_id, run_id) ", 
          "values (", settings$run$inputs[[x]], ", ", run.id, ")"), con = con)
      }
    }
  } else {
    run.id <- get.run.id("SA", "median")
    ensemble.id <- NA
  }
  medianrun <- run.id
  
  # create folders (cleaning up old ones if needed)
  if (clean) {
    unlink(file.path(settings$rundir, run.id))
    unlink(file.path(settings$modeloutdir, run.id))
  }
  dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
  dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)
  
  # write run information to disk TODO need to print list of pft names and trait
  # names
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
      "hostname    : ", settings$host$name, "\n", 
      "rundir      : ", file.path(settings$host$rundir, run.id), "\n", 
      "outdir      : ", file.path(settings$host$outdir, run.id), "\n", 
      file = file.path(settings$rundir, run.id, "README.txt"), 
      sep = "")
 
  
  # I check to make sure the path under the met is a list. if it's specified what met needs to be used in 'met.id' under sensitivity analysis of pecan xml we used that otherwise, I use the first met.
  if (is.list(settings$run$inputs$met$path)){
    # This checks for met.id tag in the settings under sensitivity analysis - if it's not there it creates it. Then it's gonna use what it created.
    if (is.null(settings$sensitivity.analysis$met.id))  settings$sensitivity.analysis$met.id <- 1
    
    settings$run$inputs$met$path <- settings$run$inputs$met$path[[settings$sensitivity.analysis$met.id]]
    
  }
  
  
  # write configuration
  do.call(my.write.config, args = list(defaults = defaults, 
                                       trait.values = median.samples, 
                                       settings = settings,
                                       run.id = run.id))
  cat(run.id, file = file.path(settings$rundir, "runs.txt"), sep = "\n", append = TRUE)
  
  ## loop over pfts
  runs <- list()
  for (i in seq_along(names(quantile.samples))) {
    pftname <- names(quantile.samples)[i]
    if (pftname == "env") {
      next
    }
    
    traits <- colnames(quantile.samples[[i]])
    quantiles.str <- rownames(quantile.samples[[i]])
    
    runs[[pftname]] <- data.frame()
    
    ## loop over variables
    for (trait in traits) {
      for (quantile.str in quantiles.str) {
        if (quantile.str != MEDIAN) {
          quantile <- as.numeric(quantile.str) / 100
          trait.samples <- median.samples
          trait.samples[[i]][trait] <- quantile.samples[[i]][quantile.str, trait, drop=FALSE]
          
          if (!is.null(con)) {
            paramlist <- paste0("quantile=", quantile.str, ",trait=", trait, ",pft=", pftname)
            insert_result <- PEcAn.DB::db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, ensemble_id, parameter_list) values ('",
                            settings$model$id, "', '", 
                            settings$run$site$id, "', '", 
                            settings$run$start.date, "', '",
                            settings$run$end.date, "', '", 
                            settings$run$outdir, "', ", 
                            ensemble.id, ", '", 
                            paramlist, "') RETURNING id"), con = con)
            run.id <- insert_result[["id"]]
            
            # associate posteriors with ensembles
            for (pft in defaults) {
              PEcAn.DB::db.query(paste0("INSERT INTO posteriors_ensembles (posterior_id, ensemble_id) values (",
                              pft$posteriorid, ", ", 
                              ensemble.id, ");"), con = con)
            }
            
            # associate inputs with runs
            if (!is.null(inputs)) {
              for (x in inputs) {
                PEcAn.DB::db.query(paste0("INSERT INTO inputs_runs (input_id, run_id) ",
                                "values (", settings$run$inputs[[x]], ", ", run.id, ");"),
                         con = con)
              }
            }
          } else {
            run.id <- get.run.id("SA", 
                                 round(quantile, 3), 
                                 trait = trait, 
                                 pft.name = names(trait.samples)[i])
          }
          runs[[pftname]][quantile.str, trait] <- run.id
          
          # create folders (cleaning up old ones if needed)
          if (clean) {
            unlink(file.path(settings$rundir, run.id))
            unlink(file.path(settings$modeloutdir, run.id))
          }
          dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
          dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)
          
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
              "hostname    : ", settings$host$name, "\n", 
              "rundir      : ", file.path(settings$host$rundir, run.id), "\n", 
              "outdir      : ", file.path(settings$host$outdir, run.id), "\n", 
              file = file.path(settings$rundir, run.id, "README.txt"), 
              sep = "")
          

          # write configuration
          do.call(my.write.config, args = list(defaults = defaults,
                                               trait.values = trait.samples, 
                                               settings = settings,
                                               run.id))
          cat(run.id, file = file.path(settings$rundir, "runs.txt"), sep = "\n", 
              append = TRUE)
        } else {
          runs[[pftname]][MEDIAN, trait] <- medianrun
        }
      }
    }
  }

  options(scipen = scipen)
  return(invisible(list(runs = runs, ensemble.id = ensemble.id)))
} # write.sa.configs
