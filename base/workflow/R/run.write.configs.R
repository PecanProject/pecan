#' Write model-specific run scripts and configuration files
#'
#' Generates run scripts and configuration files for all analyses specified
#' in the provided settings. Most of the heavy lifting is done by the
#' \code{write.config.*} function for your specific ecosystem model
#' (e.g. write.config.ED2, write.config.SIPNET).
#'
#' @param settings a PEcAn settings list
#' @param write should the runs be written to the database?
#' @param ens.sample.method how to sample the ensemble members('halton' sequence or 'uniform' random)
#' @param posterior.files Filenames for posteriors for drawing samples for ensemble and sensitivity
#'    analysis (e.g. post.distns.Rdata, or prior.distns.Rdata)
#' @param overwrite logical: Replace output files that already exist?
#'
#' @details The default value for \code{posterior.files} is NA, in which case the
#'    most recent posterior or prior (in that order) for the workflow is used.
#'    When specified, \code{posterior.files} should be a vector of filenames with one entry for each PFT.
#'    Specify filenames with no path; PFT outdirs will be appended. This forces use of only
#'    files within this workflow, to avoid confusion.
#'
#' @return an updated settings list, which includes ensemble IDs for SA and ensemble analysis
#' @export
#'
#' @author David LeBauer, Shawn Serbin, Ryan Kelly, Mike Dietze
run.write.configs <- function(settings, write = TRUE, ens.sample.method = "uniform", 
                              posterior.files = rep(NA, length(settings$pfts)), 
                              overwrite = TRUE) {
  tryCatch({
    con <- PEcAn.DB::db.open(settings$database$bety)
    on.exit(PEcAn.DB::db.close(con), add = TRUE)
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Connection requested, but failed to open with the following error: ",
      conditionMessage(e))
  })
  
  ## Which posterior to use?
  for (i in seq_along(settings$pfts)) {
    ## if posterior.files is specified us that
    if (is.na(posterior.files[i])) {
      ## otherwise, check to see if posteriorid exists
      if (!is.null(settings$pfts[[i]]$posteriorid)) {
        #TODO: sometimes `files` is a 0x0 tibble and other operations with it fail.
        files <- PEcAn.DB::dbfile.check("Posterior",
                                        settings$pfts[[i]]$posteriorid, 
                                        con, settings$host$name, return.all = TRUE)
        pid <- grep("post.distns.*Rdata", files$file_name)  ## is there a posterior file?
        if (length(pid) == 0) {
          pid <- grep("prior.distns.Rdata", files$file_name)  ## is there a prior file?
        }
        if (length(pid) > 0) {
          posterior.files[i] <- file.path(files$file_path[pid], files$file_name[pid])
        }  ## otherwise leave posteriors as NA
      }
      ## otherwise leave NA and get.parameter.samples will look for local
    } else {
      ## does posterior.files point to a directory instead of a file?
      if(utils::file_test("-d",posterior.files[i])){
        pfiles = dir(posterior.files[i],pattern = "post.distns.*Rdata",full.names = TRUE)
        if(length(pfiles)>1){
          pid = grep("post.distns.Rdata",pfiles)
          if(length(pid > 0)){
            pfiles = pfiles[grep("post.distns.Rdata",pfiles)]
          } else {
            PEcAn.logger::logger.error(
              "run.write.configs: could uniquely identify posterior files within",
              posterior.files[i])
          }
          posterior.files[i] = pfiles
        }
      }
      ## also, double check PFT outdir exists
      if (is.null(settings$pfts[[i]]$outdir) || is.na(settings$pfts[[i]]$outdir)) {
        ## no outdir
        settings$pfts[[i]]$outdir <- file.path(settings$outdir, "pfts", settings$pfts[[i]]$name)
      }
    }  ## end else
  } ## end for loop over pfts
  
  ## Sample parameters
  model <- settings$model$type
  scipen <- getOption("scipen")
  options(scipen = 12)

  PEcAn.uncertainty::get.parameter.samples(settings, posterior.files, ens.sample.method)
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  if (file.exists(samples.file)) {
    samples <- new.env()
    load(samples.file, envir = samples) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
    trait.samples <- samples$trait.samples
    ensemble.samples <- samples$ensemble.samples
    sa.samples <- samples$sa.samples
    runs.samples <- samples$runs.samples
    ## env.samples <- samples$env.samples
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
  }
  
  ## remove previous runs.txt
  if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
    PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
    unlink(file.path(settings$rundir, "runs.txt"))
  }
  
  PEcAn.utils::load.modelpkg(model)
  
  ## Check for model-specific write configs
  
  my.write.config <- paste0("write.config.",model)
  if (!exists(my.write.config)) {
    PEcAn.logger::logger.error(my.write.config, 
                 "does not exist, please make sure that the model package contains a function called", 
                 my.write.config)
  }
  
  ## Prepare for model output.  Clean up any old config files (if exists)
  #TODO: shouldn't this check if the files exist before removing them?
  my.remove.config <- paste0("remove.config.", model)
  if (exists(my.remove.config)) {
    do.call(my.remove.config, args = list(settings$rundir, settings))
  }
  
  # TODO RK : need to write to runs_inputs table
  
  # Save names
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  
  ### NEED TO IMPLEMENT: Load Environmental Priors and Posteriors
  
  ### Sensitivity Analysis
  if ("sensitivity.analysis" %in% names(settings)) {
    
    ### Write out SA config files
    PEcAn.logger::logger.info("\n ----- Writing model run config files ----")
    sa.runs <- PEcAn.uncertainty::write.sa.configs(defaults = settings$pfts,
                                quantile.samples = sa.samples, 
                                settings = settings, 
                                model = model,
                                write.to.db = write)
    
    # Store output in settings and output variables
    runs.samples$sa <- sa.run.ids <- sa.runs$runs
    settings$sensitivity.analysis$ensemble.id <- sa.ensemble.id <- sa.runs$ensemble.id
    
    # Save sensitivity analysis info
    fname <- PEcAn.uncertainty::sensitivity.filename(settings, "sensitivity.samples", "Rdata",
                                  all.var.yr = TRUE, pft = NULL)
    save(sa.run.ids, sa.ensemble.id, sa.samples, pft.names, trait.names, file = fname)
    
  }  ### End of SA
  
  ### Write ENSEMBLE
  if ("ensemble" %in% names(settings)) {
    ens.runs <- PEcAn.uncertainty::write.ensemble.configs(defaults = settings$pfts,
                                       ensemble.samples = ensemble.samples, 
                                       settings = settings,
                                       model = model, 
                                       write.to.db = write)
    
    # Store output in settings and output variables
    runs.samples$ensemble <- ens.run.ids <- ens.runs$runs
    settings$ensemble$ensemble.id <- ens.ensemble.id <- ens.runs$ensemble.id
    ens.samples <- ensemble.samples  # rename just for consistency
    
    # Save ensemble analysis info
    fname <- PEcAn.uncertainty::ensemble.filename(settings, "ensemble.samples", "Rdata", all.var.yr = TRUE)
    save(ens.run.ids, ens.ensemble.id, ens.samples, pft.names, trait.names, file = fname)
  } else {
    PEcAn.logger::logger.info("not writing config files for ensemble, settings are NULL")
  }  ### End of Ensemble
  
  PEcAn.logger::logger.info("###### Finished writing model run config files #####")
  PEcAn.logger::logger.info("config files samples in ", file.path(settings$outdir, "run"))
  
  ### Save output from SA/Ensemble runs
  # A lot of this is duplicate with the ensemble/sa specific output above, but kept for backwards compatibility.   
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, pft.names, trait.names, 
       file = file.path(settings$outdir, "samples.Rdata"))
  PEcAn.logger::logger.info("parameter values for runs in ", file.path(settings$outdir, "samples.RData"))
  options(scipen = scipen)
  
  return(invisible(settings))
}
