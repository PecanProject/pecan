##' Sanity checks. Checks the settings file to make sure expected fields exist. It will try to use
##' default values for any missing values, or stop the exection if no defaults are possible.
##'
##' Expected fields in settings file are:
##' - pfts with at least one pft defined
##' @title Check Settings
##' @param settings settings file
##' @return will return the updated settings values with defaults set.
##' @author Rob Kooper, David LeBauer
.check.settings <- function(settings) {
  if (!is.null(settings$nocheck)) {
    logger.info("Not doing sanity checks of pecan.xml")
    return(settings)
  }
  scipen = getOption("scipen")
  options(scipen = 12)
  
  settings <- .check.database.settings(settings)
  
  if(!is.null(settings$database$bety)) {
    dbcon <- db.open(settings$database$bety)
    on.exit(db.close(dbcon))
  } else {
    dbcon <- NULL
  }
  
  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    logger.warn("No PFTS specified.")
  }
  
  # check to make sure a host is given
  if (is.null(settings$host$name)) {
    logger.info("Setting localhost for execution host.")
    settings$host$name <- "localhost"
  }
  
  # check if there is either ensemble or sensitivy.analysis
  if (is.null(settings$ensemble) && is.null(settings$sensitivity.analysis)) {
    logger.warn("No ensemble or sensitivity analysis specified, no models will be executed!")
  }
  
  settings <- papply(settings, .check.run.settings, dbcon = dbcon)
  
  # check meta-analysis
  if(!is.null(settings$meta.analysis)) {  
    if (is.null(settings$meta.analysis$iter)) {
      settings$meta.analysis$iter <- 3000
      logger.info("Setting meta.analysis iterations to ", settings$meta.analysis$iter)
    }
    if (is.null(settings$meta.analysis$random.effects)) {
      settings$meta.analysis$random.effects <- FALSE
      logger.info("Setting meta.analysis random effects to ", settings$meta.analysis$random.effects)
    } else {
      settings$meta.analysis$random.effects <- as.logical(settings$meta.analysis$random.effects)
    }
    if (is.null(settings$meta.analysis$threshold)) {
      settings$meta.analysis$threshold <- 1.2
      logger.info("Setting meta.analysis threshold to ", settings$meta.analysis$threshold)
    }
    if (is.null(settings$meta.analysis$update)) {
      settings$meta.analysis$update <- 'AUTO'
      logger.info("Setting meta.analysis update to only update if no previous meta analysis was found")
    }
    if ((settings$meta.analysis$update != 'AUTO') && is.na(as.logical(settings$meta.analysis$update))) {
      logger.info("meta.analysis update can only be AUTO/TRUE/FALSE, defaulting to FALSE")
      settings$meta.analysis$update <- FALSE
    }
  }
  
  settings <- .check.model.settings(settings, dbcon)
  
  ## if run$host is localhost, set to "localhost
  if (any(settings$host %in% c(Sys.info()['nodename'], gsub("illinois", "uiuc", Sys.info()['nodename'])))){
    settings$host$name <- "localhost"
  }
  
  # check if we need to use qsub
  if ("qsub" %in% names(settings$host)) {
    if (is.null(settings$host$qsub)) {
      settings$host$qsub <- "qsub -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
      logger.info("qsub not specified using default value :", settings$host$qsub)
    }
    if (is.null(settings$host$qsub.jobid)) {
      settings$host$qsub.jobid <- "Your job ([0-9]+) .*"
      logger.info("qsub.jobid not specified using default value :", settings$host$qsub.jobid)
    }
    if (is.null(settings$host$qstat)) {
      settings$host$qstat <- "qstat -j @JOBID@ &> /dev/null || echo DONE"
      logger.info("qstat not specified using default value :", settings$host$qstat)
    }
  }
  
  # modellauncher to launch on multiple nodes/cores
  if ("modellauncher" %in% names(settings$host)) {
    if (is.null(settings$host$modellauncher$binary)) {
      settings$host$modellauncher$binary <- "modellauncher"
      logger.info("binary not specified using default value :", settings$host$modellauncher$binary)
    }
    if (is.null(settings$host$modellauncher$qsub.extra)) {
      logger.severe("qsub.extra not specified, can not launch in parallel environment.")
    }
    if (is.null(settings$host$modellauncher$mpirun)) {
      settings$host$modellauncher$mpirun <- "mpirun"
      logger.info("mpirun not specified using default value :", settings$host$modellauncher$mpirun)
    }
  }
  
  # some warnings for deprecated job.sh
  if ("job.sh" %in% names(settings$model)) {
    if ("prerun" %in% names(settings$model)) {
      logger.severe("You have both settings$model$job.sh and settings$model$prerun, please combine.")
    }
    logger.info("settings$model$job.sh is deprecated use settings$model$prerun instead.")
    settings$model$prerun <- settings$model$job.sh
    settings$model$job.sh <- NULL
  }
  if ("job.sh" %in% names(settings$host)) {
    if ("prerun" %in% names(settings$host)) {
      logger.severe("You have both settings$host$job.sh and settings$host$prerun, please combine.")
    }
    logger.info("settings$host$job.sh is deprecated use settings$host$prerun instead.")
    settings$host$prerun <- settings$host$job.sh
    settings$host$job.sh <- NULL
  }
  
  # Check folder where outputs are written before adding to dbfiles
  if(is.null(settings$database$dbfiles)) {
    settings$database$dbfiles <- full.path("~/.pecan/dbfiles")
  } else {
    if (substr(settings$database$dbfiles, 1, 1) != '/') {
      logger.warn("settings$database$dbfiles pathname", settings$database$dbfiles, " is invalid\n
                  placing it in the home directory ", Sys.getenv("HOME"))
      settings$database$dbfiles <- file.path(Sys.getenv("HOME"), settings$database$dbfiles)
    } 
    
    settings$database$dbfiles <- normalizePath(settings$database$dbfiles, mustWork = FALSE)
  }
  dir.create(settings$database$dbfiles, showWarnings = FALSE, recursive = TRUE)
  
  # check all inputs exist
  settings <- papply(settings, .check.inputs)
  
  settings <- .check.workflow.settings(settings, dbcon)
  
  # check/create the local run folder
  if (is.null(settings$rundir)) {
    settings$rundir <- file.path(settings$outdir, "run")
  }
  if (!file.exists(settings$rundir) && !dir.create(settings$rundir, recursive = TRUE)) {
    logger.severe("Could not create run folder", settings$rundir)
  }
  
  # check/create the local model out folder
  if (is.null(settings$modeloutdir)) {
    settings$modeloutdir <- file.path(settings$outdir, "out")
  }
  if (!file.exists(settings$modeloutdir) && !dir.create(settings$modeloutdir, recursive = TRUE)) {
    logger.severe("Could not create model out folder", settings$modeloutdir)
  }
  
  # make sure remote folders are specified if need be
  if (!is.localhost(settings$host)) {
    if (is.null(settings$host$folder)) {
      settings$host$folder <- paste0(remote.execute.cmd("pwd", host = settings$host), "/pecan_remote")
      logger.info("Using ", settings$host$folder, "to store output on remote machine")      
    }
    if (is.null(settings$host$rundir)) {
      settings$host$rundir <- paste0(settings$host$folder, "/@WORKFLOW@/run")
    }
    settings$host$rundir <- gsub("@WORKFLOW@", settings$workflow$id, settings$host$rundir)
    logger.info("Using ", settings$host$rundir, "to store runs on remote machine")
    if (is.null(settings$host$outdir)) {
      settings$host$outdir <- paste0(settings$host$folder, "/@WORKFLOW@/out")
    }
    settings$host$outdir <- gsub("@WORKFLOW@", settings$workflow$id, settings$host$outdir)
    logger.info("Using ", settings$host$outdir, "to store output on remote machine")
  } else if (settings$host$name == "localhost") {
    settings$host$rundir <- settings$rundir
    settings$host$outdir <- settings$modeloutdir
  }
  
  # check/create the pft folders
  if (!is.null(settings$pfts) && (length(settings$pfts) > 0)) {
    for (i in seq_along(settings$pfts)) {
      #check if name tag within pft
      if (!"name" %in% names(settings$pfts[i]$pft)) {
        logger.severe("No name specified for pft of index: ", i, ", please specify name")
      }
      if (settings$pfts[i]$pft$name == "") {
        logger.severe("Name specified for pft of index: ", i, " can not be empty.")
      }
      
      #check to see if name of each pft in xml file is actually a name of a pft already in database
      if (!is.null(dbcon)) { # change to if(class(dbcon) == "PostgreSQLConnection")??
        if (is.null(settings$model$type)) {
          x <- db.query(paste0("SELECT pfts.id FROM pfts", " WHERE pfts.name = '",  
                               settings$pfts[i]$pft$name, "'"), con = dbcon)
        } else {
          x <- db.query(paste0("SELECT pfts.id FROM pfts, modeltypes",
                               " WHERE pfts.name = '", settings$pfts[i]$pft$name, "'",
                               " AND modeltypes.name='", settings$model$type, "'",
                               " AND modeltypes.id=pfts.modeltype_id;"), con = dbcon)
        }
        if (nrow(x) == 0) {
          logger.severe("Did not find a pft with name ", settings$pfts[i]$pft$name,
                        "\nfor model type", settings$model$type)
        }
        if (nrow(x) > 1) {
          logger.warn("Found multiple entries for pft with name ", settings$pfts[i]$pft$name,
                      "\nfor model type", settings$model$type)
        }
      }
      
      if (is.null(settings$pfts[i]$pft$outdir)) {
        settings$pfts[i]$pft$outdir <- file.path(settings$outdir, "pft", settings$pfts[i]$pft$name)
        logger.info("Storing pft", settings$pfts[i]$pft$name, "in", settings$pfts[i]$pft$outdir)      
      } else {
        logger.debug("Storing pft", settings$pfts[i]$pft$name, "in", settings$pfts[i]$pft$outdir)      
      }
      out.dir <- settings$pfts[i]$pft$outdir
      if (!file.exists(out.dir) && !dir.create(out.dir, recursive = TRUE)) {
        if(identical(dir(out.dir), character(0))){
          logger.warn(out.dir, "exists but is empty")
        } else {
          logger.severe("Could not create folder", out.dir)        
        }
      }
    }
  }
  
  options(scipen = scipen)
  
  # all done return cleaned up settings
  return(invisible(settings))
}
