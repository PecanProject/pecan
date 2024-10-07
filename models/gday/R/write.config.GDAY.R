##' Writes a config file for GDAY
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.GDAY
##' @title Write GDAY configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for GDAY for given run
##' @export
##' @author Martin De Kauwe
##-------------------------------------------------------------------------------------------------#
write.config.GDAY <- function(defaults, trait.values, settings, run.id) {
    
    # find out where to write run/ouput
    rundir <- file.path(settings$host$rundir, as.character(run.id))
    outdir <- file.path(settings$host$outdir, as.character(run.id))
    if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
        rundir <- file.path(settings$rundir, as.character(run.id))
        outdir <- file.path(settings$modeloutdir, as.character(run.id))
    }
    #-----------------------------------------------------------------------
    # create launch script (which will create symlink)
    if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
        jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
    } else {
        jobsh <- readLines(con = system.file("template.job", package = "PEcAn.GDAY"), n = -1)
    }
    
    # create host specific setttings
    hostsetup <- ""
    if (!is.null(settings$model$prerun)) {
        hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
    }
    if (!is.null(settings$host$prerun)) {
        hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
    }
    
    hostteardown <- ""
    if (!is.null(settings$model$postrun)) {
        hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
    }
    if (!is.null(settings$host$postrun)) {
        hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
    }
    
    
    # Write out base param file to run directory
    params <- readLines(con = system.file("base_start.cfg", package = "PEcAn.GDAY"), n = -1)
    writeLines(params, con = file.path(settings$rundir, run.id, "base_start.cfg"))
    
    
    # Create Python Run script
    runpy<- readLines(con = system.file("run_simulations.py", package = "PEcAn.GDAY"), n = -1)
      
    executable <- settings$model$binary
    gday_path <- strsplit(executable, "src")[[1]]
    scripts <- paste(gday_path[1],"scripts",sep = "")
    
    runpy <- gsub("@PATH_SCRIPTS@",scripts,runpy)
    runpy <- gsub("@PATHTOGDAY@",executable,runpy)
    runpy <- gsub("@PATH_PARAMS@",rundir, runpy)
    
    exp <- strsplit(settings$run$input$met$path, split = "/")[[1]]
    exp_tag <- exp[length(exp)]
    met_path <- strsplit(settings$run$input$met$path,exp_tag)[[1]]
      
    runpy <- gsub("@SITE_MET@", met_path , runpy)
    runpy <- gsub("@RUNDIR@", rundir, runpy)
    runpy <- gsub("@LATITUDE@", settings$run$site$lat,runpy)
    
    
    runpy <- gsub("@SITE@",exp_tag,runpy)
    
    writeLines(runpy, con = file.path(settings$rundir, run.id, "run_simulations.py"))
    Sys.chmod(file.path(settings$rundir, run.id, "run_simulations.py"))
    
    
    # create job.sh
    jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
    jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
    
    jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
    jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
    jobsh <- gsub("@SITE_MET@", settings$run$input$met$path, jobsh)
    
    jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
    jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
    
    jobsh <- gsub("@OUTDIR@", outdir, jobsh)
    jobsh <- gsub("@RUNDIR@", rundir, jobsh)
    
    jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
    
    writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
    Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.GDAY
