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
##' - database
##' - model
##' - run with the following fields
##' -- site with id
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

  # check database information
  if (is.null(settings$database)) {
    logger.stop("No database information specified.")
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
  tryCatch(db.query("SELECT 1", params=settings$database), 
    error=function(e) {
      logger.stop("Could not connect to the database.")
    }
  )

  # TODO check userid and userpassword

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

  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    logger.stop("No PFTS specified.")
  }

  # check to make sure run information is filled out
  if (is.null(settings$run$host$name)) {
    logger.info("Setting localhost for execution host.")
    settings$run$host$name = "localhost"
  }
  if (settings$run$host$name != "localhost") {
    if (is.null(settings$run$host$rundir)) {
      logger.stop("not rundir specified on remote machine.")
    }
    if (is.null(settings$run$host$outdir)) {
      logger.stop("not outdir specified on remote machine.")
    }
  }

  # check/crate the pecan folder
  if (is.null(settings$outdir)) {
    logger.stop("No output folder specified")
  } else {
    logger.debug("output folder =", settings$outdir)
  }
  if (!file.exists(settings$outdir) && !dir.create(settings$outdir, recursive=TRUE)) {
    logger.stop("Could not create folder", settings$outdir)
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
    logger.stop("Could not create folder", settings$rundir)
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
    logger.stop("Could not create folder", settings$modeloutdir)
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
      logger.stop("Could not create folder", out.dir)
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
  if (settings$bety$write) {
    if (!'workflow' %in% names(settings)) {
      con <- db.open(settings$database)
      if(!is.character(con)){
        query.base(paste("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, started_at, created_at, folder) values ('",
                         settings$run$site$id, "','", settings$model$id, "', '", settings$run$host$name, "', '",
                         settings$run$start.date, "', '", settings$run$end.date, "', NOW(), NOW(), '", dirname(settings$outdir), "')", sep=''), con)
        settings$workflow$id = query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)[['ID']]
        db.close(con)
      }
    }
  } else {
    settings$workflow$id = -999
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

  loc <- which(commandArgs() == "--settings")
  if (length(loc) != 0) {
    # 1 filename is passed as argument to R
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        logger.info("Loading --settings=", idx+1)
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

  ## check to see if a saved version already exists.
  settings <- xmlToList(xml)
  output <- file.path(settings$outdir, outputfile)
  if (file.exists(output)) {
    logger.info("Loading saved settings file=", output)
    xml <- xmlParse(output)
    settings <- xmlToList(xml)
  }

  ## convert the xml to a list for ease and return
  settings <- check.settings(settings)
  
  ## save the checked/fixed pecan.xml
  if (file.exists(output)) {
    logger.warn(paste("File already exists [", output, "] file will be overwritten"))
  }
  saveXML(listToXml(settings, "pecan"), file=output)

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
