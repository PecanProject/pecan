##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
library(XML)

##--------------------------------------------------------------------------------------------------#
## EXTERNAL FUNCTIONS
##--------------------------------------------------------------------------------------------------#

##' Sanity checks. Checks the settings file to make sure expected fields exist. It will try to use
##' default values for any missing values, or stop the exection if no defaults are possible.
##'
##' Expected fields in settings file are:
##' - pfts with at least one pft defined
##' @title Check Settings
##' @param settings settings file
##' @return will return the updated settings values with defaults set.
##' @export
##' @author Rob Kooper
check.settings <- function(settings) {
  if (!is.null(settings$nocheck)) {
    logger.info("Not doing sanity checks of pecan.xml")
    return(0)
  }

  # should runs be written to database
  if (is.null(settings$bety$write)) {
    logger.info("Writing all runs/configurations to database.")
    settings$bety$write <- TRUE
  } else {
    settings$bety$write <- as.logical(settings$bety$write)
    if (settings$bety$write) {
      logger.debug("Writing all runs/configurations to database.")
    } else {
      logger.warn("Will not write runs/configurations to database.")
    }
  }

  # check database information
  if (is.null(settings$database)) {
    settings$database <- list(username = "bety", password = "bety", 
                              host = "localhost", dbname = "bety", driver = "MySQL")
    logger.info("No database information specified; using default bety bety bety.")
  }
  
  if (is.null(settings$database$driver)) {
    settings$database$driver <- "MySQL"
    logger.info("Using", settings$database$driver, "as database driver.")
  }
  if (!is.null(settings$database$userid) && (settings$database$driver == "MySQL")) {
    logger.info("userid in database section should be username for MySQL")
    settings$database$username <- settings$database$userid
    settings$database$userid <- NULL
  }
  if (!is.null(settings$database$passwd) && (settings$database$driver == "MySQL")) {
    logger.info("passwd in database section should be password for MySQL")
    settings$database$password <- settings$database$passwd
    settings$database$passwd <- NULL
  }
  if (!is.null(settings$database$name) && (settings$database$driver == "MySQL")) {
    logger.info("name in database section should be dbname for MySQL")
    settings$database$dbname <- settings$database$name
    settings$database$name <- NULL
  }
  if(paste0("R", settings$database$driver) %in% rownames(installed.packages())){
    require(PEcAn.DB)
    if (!db.exists(params=settings$database, write=settings$bety$write)) {
      logger.warn("Could not connect to the database.")
      database <- FALSE
    } else {
      logger.info("Successfully connected to database")
      database <- TRUE
    }
  } else {
    database <- FALSE
  }

  # TODO check userid and userpassword
  
  # check database version
  if(database){
    versions <- db.query("SELECT version FROM schema_migrations WHERE version >= 20130425152503;", params=settings$database)[['version']]
    if (length(versions) == 0) {
      logger.severe("Database is out of date, please update the database.")
    }
    if (length(versions) > 1) {
      logger.warn("Database is more recent than PEcAn expects this could result in PEcAn not working as expected.",
                  "If PEcAn fails, either revert database OR update PEcAn and edit expected database version in",
                  "utils/R/read.settings.R (Redmine #1673).")
    } else {
      logger.debug("Database is correct version", versions[1], ".")
    }
  }
  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    logger.severe("No PFTS specified.")
  }

  # check if there is either ensemble or sensitivy.analysis
  if (is.null(settings$ensemble) && is.null(settings$sensitivity.analysis)) {
    logger.warn("No ensemble or sensitivity analysis specified, runs will fail!")
  }

  # check ensemble
  if (!is.null(settings$ensemble)) {
    if (is.character(settings$ensemble) || is.null(settings$ensemble$variable)) {
      if (is.null(settings$sensitivity.analysis$variable)) {
        logger.severe("No variable specified to compute ensemble for.")
      }
      logger.info("Setting ensemble variable to the same as sensitivity analysis variable [", settings$sensitivity.analysis$variable, "]")
      settings$ensemble$variable <- settings$sensitivity.analysis$variable
    }

    if (is.null(settings$ensemble$size)) {
      logger.info("Setting ensemble size to 1.")
      settings$ensemble$size <- 1
    }

    if(is.null(settings$ensemble$start.date)) {
      if(is.null(settings$sensitivity.analysis$start.date)) {
        settings$ensemble$start.date <- settings$run$start.date 
        logger.info("No start date passed to ensemble - using the run date (", settings$ensemble$start.date, ").")
      } else { 
        settings$ensemble$start.date <- settings$sensitivity.analysis$start.date 
        logger.info("No start date passed to ensemble - using the sensitivity.analysis date (", settings$ensemble$start.date, ").")
      }
    }

    if(is.null(settings$ensemble$end.date)) {
      if(is.null(settings$sensitivity.analysis$end.date)) {
        settings$ensemble$end.date <- settings$run$end.date 
        logger.info("No end date passed to ensemble - using the run date (", settings$ensemble$end.date, ").")
      } else { 
        settings$ensemble$end.date <- settings$sensitivity.analysis$end.date 
        logger.info("No end date passed to ensemble - using the sensitivity.analysis date (", settings$ensemble$end.date, ").")
      }
    }
  }

  # check sensitivity analysis
  if (!is.null(settings$sensitivity.analysis)) {
    if (is.null(settings$sensitivity.analysis$variable)) {
      if (is.null(settings$ensemble$variable)) {
        logger.severe("No variable specified to compute sensitivity.analysis for.")
      }
      logger.info("Setting sensitivity.analysis variable to the same as ensemble variable [", settings$ensemble$variable, "]")
      settings$sensitivity.analysis$variable <- settings$ensemble$variable
    }

    if(is.null(settings$sensitivity.analysis$start.date)) {
      if(is.null(settings$ensemble$start.date)) {
        settings$sensitivity.analysis$start.date <- settings$run$start.date 
        logger.info("No start date passed to sensitivity.analysis - using the run date (", settings$sensitivity.analysis$start.date, ").")
      } else { 
        settings$sensitivity.analysis$start.date <- settings$ensemble$start.date 
        logger.info("No start date passed to sensitivity.analysis - using the ensemble date (", settings$sensitivity.analysis$start.date, ").")
      }
    }

    if(is.null(settings$sensitivity.analysis$end.date)) {
      if(is.null(settings$ensemble$end.date)) {
        settings$sensitivity.analysis$end.date <- settings$run$end.date 
        logger.info("No end date passed to sensitivity.analysis - using the run date (", settings$sensitivity.analysis$end.date, ").")
      } else { 
        settings$sensitivity.analysis$end.date <- settings$ensemble$end.date 
        logger.info("No end date passed to sensitivity.analysis - using the ensemble date (", settings$sensitivity.analysis$end.date, ").")
      }
    }
  }

  # check to make sure run information is filled out
  if (is.null(settings$run$host$name)) {
    logger.info("Setting localhost for execution host.")
    settings$run$host$name = "localhost"
  }
  if (settings$run$host$name != "localhost") {
    if (is.null(settings$run$host$rundir)) {
      logger.severe("not rundir specified on remote machine.")
    }
    if (is.null(settings$run$host$outdir)) {
      logger.severe("not outdir specified on remote machine.")
    }
  }

  # check meta-analysis
  if (is.null(settings$meta.analysis) || is.null(settings$meta.analysis$iter)) {
    settings$meta.analysis$iter <- 3000
    logger.info("Setting meta.analysis iterations to ", settings$meta.analysis$iter)
  }
  if (is.null(settings$meta.analysis$random.effects)) {
    settings$meta.analysis$random.effects <- FALSE
    logger.info("Setting meta.analysis random effects to ", settings$meta.analysis$random.effects)
  }

  # check modelid with values
  if(!is.null(settings$model)){
  if (is.null(settings$model$id)) {
    settings$model$id <- -1
  } else if (settings$model$id >= 0) {
    if(database){
      model <- db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params=settings$database)      
    }
    if(nrow(model) == 0) {
      logger.error("There is no record of model_id = ", settings$model$id, "in database")
    }
    model$binary <- tail(strsplit(model$model_path, ":")[[1]], 1)

    if (is.null(settings$model$name)) {
      if ((is.null(model$model_type) && model$model_type == "")) {
        logger.severe("No model type specified.")
      }
      settings$model$name <- model$model_type
      logger.info("Setting model type to ", settings$model$name)
    } else if (model$model_type != settings$model$name) {
      logger.warn("Specified model type [", settings$model$name, "] does not match model_type in database [", model$model_type, "]")
    }

    if (is.null(settings$model$binary)) {
      if ((is.null(model$binary) && model$binary == "")) {
        logger.severe("No model binary specified.")
      }
      settings$model$binary <- tail(strsplit(model$binary, ":")[[1]], 1)
      logger.info("Setting model binary to ", settings$model$binary)
    } else if (model$binary != settings$model$binary) {
      logger.warn("Specified binary [", settings$model$binary, "] does not match model_path in database [", model$binary, "]")
    }
  }
  }

  # check siteid with values
  if(!is.null(settings$run$site)){
  if (is.null(settings$run$site$id)) {
    settings$run$site$id <- -1
  } else if (settings$run$site$id >= 0) {
    site <- db.query(paste("SELECT * FROM sites WHERE id =", settings$run$site$id), params=settings$database);

    if (is.null(settings$run$site$name)) {
      if ((is.null(site$sitename) && site$sitename == "")) {
        logger.info("No site name specified.")
        settings$run$site$name <- "NA"
      } else {
        settings$run$site$name <- site$sitename        
        logger.info("Setting site name to ", settings$run$site$name)
      }
    } else if (site$sitename != settings$run$site$name) {
      logger.warn("Specified site name [", settings$run$site$name, "] does not match sitename in database [", site$sitename, "]")
    }

    if (is.null(settings$run$site$lat)) {
      if ((is.null(site$lat) && site$lat == "")) {
        logger.severe("No lat specified for site.")
      } else {
        settings$run$site$lat <- as.numeric(site$lat)
        logger.info("Setting site lat to ", settings$run$site$lat)
      }
    } else if (as.numeric(site$lat) != as.numeric(settings$run$site$lat)) {
      logger.warn("Specified site lat [", settings$run$site$lat, "] does not match lat in database [", site$lat, "]")
    }

    if (is.null(settings$run$site$lon)) {
      if ((is.null(site$lon) && site$lon == "")) {
        logger.severe("No lon specified for site.")
      } else {
        settings$run$site$lon <- as.numeric(site$lon)
        logger.info("Setting site lon to ", settings$run$site$lon)
      }
    } else if (as.numeric(site$lon) != as.numeric(settings$run$site$lon)) {
      logger.warn("Specified site lon [", settings$run$site$lon, "] does not match lon in database [", site$lon, "]")
    }
  }
  }

  # check/create the pecan folder
  if (is.null(settings$outdir)) {
    settings$outdir <- tempdir()
    logger.info("No output folder specified, using", tempdir())
  } else {
    logger.debug("output folder =", settings$outdir)
  }
  if (!file.exists(settings$outdir) && !dir.create(settings$outdir, recursive=TRUE)) {
    logger.severe("Could not create folder", settings$outdir)
  }

  # check/create the run folder
  if (settings$run$host$name == "localhost") {
    if (is.null(settings$run$host$rundir)) {
      settings$run$host$rundir <- file.path(settings$outdir, "run")
    }
    settings$rundir <- settings$run$host$rundir
  } else {
    if (is.null(settings$rundir)) {
      settings$rundir <- file.path(settings$outdir, "run")
    }
  }
  if (!file.exists(settings$rundir) && !dir.create(settings$rundir, recursive=TRUE)) {
    logger.severe("Could not create folder", settings$rundir)
  }

  # check/create the out folder
  if (settings$run$host$name == "localhost") {
    if (is.null(settings$run$host$outdir)) {
      settings$run$host$outdir <- file.path(settings$outdir, "out")
    }
    settings$modeloutdir <- settings$run$host$outdir
  } else {
    if (is.null(settings$modeloutdir)) {
      settings$modeloutdir <- file.path(settings$outdir, "out")
    }
  }
  if (!file.exists(settings$modeloutdir) && !dir.create(settings$modeloutdir, recursive=TRUE)) {
    logger.severe("Could not create folder", settings$modeloutdir)
  }

  # check/create the pft folders
  for (i in 1:sum(names(unlist(settings$pfts)) == "pft.name")) {
    if (is.null(settings$pfts[i]$pft$outdir)) {
      settings$pfts[i]$pft$outdir <- file.path(settings$outdir, "pft", settings$pfts[i]$pft$name)
      logger.info("Storing pft", settings$pfts[i]$pft$name, "in", settings$pfts[i]$pft$outdir)      
    } else {
      logger.debug("Storing pft", settings$pfts[i]$pft$name, "in", settings$pfts[i]$pft$outdir)      
    }
    out.dir <- settings$pfts[i]$pft$outdir
    if (!file.exists(out.dir) && !dir.create(out.dir, recursive=TRUE)) {
      if(identical(dir(out.dir), character(0))){
        logger.warn(out.dir, "exists but is empty")
      } else {
        logger.severe("Could not create folder", out.dir)        
      }
    }
  }

  # add defaults for qsub
  if (settings$run$host$name != "localhost") {
    if (is.null(settings$run$host$qsub)) {
      settings$run$host$qsub = "qsub -N @NAME@ -o @STDOUT@ -e @STDERR@"
      logger.info("qsub not specified using default value :", settings$run$host$qsub)
    }
    if (is.null(settings$run$host$qsub.jobid)) {
      settings$run$host$qsub.jobid = "Your job ([0-9]+) .*"
      logger.info("qsub.jobid not specified using default value :", settings$run$host$qsub.jobid)
    }
    if (is.null(settings$run$host$qstat)) {
      settings$run$host$qstat = "qstat -j @JOBID@ 2>1 >/dev/null || echo DONE"
      logger.info("qstat not specified using default value :", settings$run$host$qstat)
    }
  }

  # check for workflow defaults
  if(database){
    if (settings$bety$write) {
      if ("model" %in% names(settings) && !'workflow' %in% names(settings)) {
        con <- db.open(settings$database)
        if(!is.character(con)){
          db.query(paste("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, started_at, created_at, folder) values ('",
                         settings$run$site$id, "','", settings$model$id, "', '", settings$run$host$name, "', '",
                         settings$run$start.date, "', '", settings$run$end.date, "', NOW(), NOW(), '", dirname(settings$outdir), "')", sep=''), con)
          settings$workflow$id = db.query(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
          db.close(con)
        }
      }
    } else {
      settings$workflow$id = "NA"
    }
  }
  # all done return cleaned up settings
  invisible(settings)
}


##' Loads PEcAn settings file
##' 
##' This will try and find the PEcAn settings file in the following order:
##' 
##' \enumerate{
##' \item {--settings <file>}{passed as command line argument using --settings}
##' \item {inputfile}{passed as argument to function}
##' \item {PECAN_SETTINGS}{environment variable PECAN_SETTINGS pointing to a specific file}
##' \item {./pecan.xml}{pecan.xml in the current folder}
##' }
##' @param inputfile the PEcAn settings file to be used.
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' settings <- read.settings()
##' settings <- read.settings(file="willowcreek.xml")
##' test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
##' settings <- read.settings(test.settings.file)
##' }
read.settings <- function(inputfile=NULL, outputfile="pecan.xml"){
  if (is.null(outputfile)) {
    outputfile="pecan.xml"
  }
  if(inputfile == ""){
    logger.warn("settings files specified as empty string; \n\t\tthis may be caused by an incorrect argument to system.file.")
  }
  loc <- which(commandArgs() == "--settings")
  if (length(loc) != 0) {
    # 1 filename is passed as argument to R
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        logger.info("Loading --settings=", commandArgs()[idx+1])
        xml <- xmlParse(commandArgs()[idx+1])
        break
      }
    }

  } else if(!is.null(inputfile) && file.exists(inputfile)) {
    # 2 filename passed into function
    logger.info("Loading inpufile=", inputfile)
    xml <- xmlParse(inputfile)

  } else if (file.exists(Sys.getenv("PECAN_SETTINGS"))) {
    # 3 load from PECAN_SETTINGS
    logger.info("Loading PECAN_SETTINGS=", Sys.getenv("PECAN_SETTINGS"))
    xml <- xmlParse(Sys.getenv("PECAN_SETTINGS"))

  } else if (file.exists("pecan.xml")) {
    # 4 load ./pecan.xml
    logger.info("Loading ./pecan.xml")
    xml <- xmlParse("pecan.xml")

  } else {
    # file not found
    stop("Could not find a pecan.xml file")
  }

  ## convert the xml to a list for ease and return
  settings <- check.settings(xmlToList(xml))
  
  ## save the checked/fixed pecan.xml
  pecanfile <- file.path(settings$outdir, outputfile)
  if (file.exists(pecanfile)) {
    logger.warn(paste("File already exists [", pecanfile, "] file will be overwritten"))
  }
  saveXML(listToXml(settings, "pecan"), file=pecanfile)

  ## setup Rlib from settings
  if(!is.null(settings$Rlib)){
    .libPaths(settings$Rlib)
  }

  ## Return settings file as a list
  invisible(settings)
}
##=================================================================================================#

####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################
