##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
library(XML)
library(lubridate)
library(PEcAn.DB)
library(PEcAn.utils)

##--------------------------------------------------------------------------------------------------#
## INTERNAL FUNCTIONS
##--------------------------------------------------------------------------------------------------#

check.database <- function(database) {
  if (is.null(database)) return(NULL);

  ## check database settings
  if (is.null(database$driver)) {
    database$driver <- "PostgreSQL"
    logger.warn("Please specify a database driver; using default 'PostgreSQL'")
  }
      
  # Attempt to load the driver
  if (!require(paste0("R", database$driver), character.only=TRUE)) {
    logger.warn("Could not load the database driver", paste0("R", database$driver))
  }
  
  # MySQL specific checks
  if (database$driver == "MySQL") {
    if (!is.null(database$passwd)) {
      logger.info("passwd in database section should be password for MySQL")
      database$password <- database$passwd
      database$passwd <- NULL
    }
    if (!is.null(database$name)) {
      logger.info("name in database section should be dbname for MySQL")
      database$dbname <- database$name
      database$name <- NULL
    }
  }
  
  # PostgreSQL specific checks
  if (database$driver == "PostgreSQL") {
    if (!is.null(database$passwd)) {
      logger.info("passwd in database section should be password for PostgreSQL")
      database$password <- database$passwd
      database$passwd <- NULL
    }
    if (!is.null(database$name)) {
      logger.info("name in database section should be dbname for PostgreSQL")
      database$dbname <- database$name
      database$name <- NULL
    }
  }

  ## The following hack handles *.illinois.* to *.uiuc.* aliases of ebi-forecast
  if(!is.null(database$host)){
      forcastnames <- c("ebi-forecast.igb.uiuc.edu",
                        "ebi-forecast.igb.illinois.edu") 
      if((database$host %in% forcastnames) &
         (Sys.info()['nodename'] %in% forcastnames)){
          database$host <- "localhost"
      }
  } else if(is.null(database$host)){
      database$host <- "localhost"
  }

  ## convert strings around from old format to new format
  if(is.null(database[["user"]])){
    if (!is.null(database$userid)) {
      logger.info("'userid' in database section should be 'user'")
      database$user <- database$userid
      
    } else if (!is.null(database$username)) {
      logger.info("'username' in database section should be 'user'")
      database$user <- database$username
  
    } else {
      logger.info("no database user specified, using 'bety'")
      database$user <- "bety"
    }
  } 
  database$userid <- database$username <- NULL

  # fill in defaults for the database
  if(is.null(database$password)) {
    database$password <- "bety"
  }
  if(is.null(database$dbname)) {
    database$dbname <- "bety"
  }

  if (!db.exists(params=database, FALSE)) {
    logger.severe("Invalid Database Settings : ", unlist(database))
  }

  # connected
  logger.info("Successfully connected to database : ", unlist(database))

  # return fixed up database
  return(database)
} 

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
  scipen = getOption("scipen")
  options(scipen=12)

  # check database secions if exist
  if (!is.null(settings$database)) {

    # simple check to make sure the database tag is updated
    if (!is.null(settings$database$dbname)) {
      log.severe("Database tag has changed, please use <database><bety> to store",
                 "information about accessing the BETY database. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#database-access.")
    }

    # check bety database access
    if (!is.null(settings$database$bety)) {
      settings$database$bety <- check.database(settings$database$bety)

      # warn user about change and update settings
      if (!is.null(settings$bety$write)) {
        logger.warn("<bety><write> is now part of the database settings. For more",
                    "information about the database settings see",
                    "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#database-access.")
        if (is.null(settings$database$bety$write)) {
          settings$database$bety$write <- settings$bety$write
          settings$bety$write <- NULL
          if (length(settings$bety) == 0) settings$bety <- NULL
        }
      }
    
      # should runs be written to database
      if (is.null(settings$database$bety$write)) {
        logger.info("Writing all runs/configurations to database.")
        settings$database$bety$write <- TRUE
      } else {
        settings$database$bety$write <- as.logical(settings$database$bety$write)
        if (settings$database$bety$write) {
          logger.debug("Writing all runs/configurations to database.")
        } else {
          logger.warn("Will not write runs/configurations to database.")
        }
      }

      # check if we can connect to the database with write permissions
      if (settings$database$bety$write && !db.exists(params=settings$database$bety, TRUE)) {
        logger.severe("Invalid Database Settings : ", unlist(settings$database))
      }

      # TODO check userid and userpassword
    }

    # check fia database access
    if (!is.null(settings$database$fia)) {
      settings$database$fia = check.database(settings$database$fia)
    }
  }

  ## allow PEcAn to run without database
  if (is.null(settings$database) || is.null(settings$database$bety)) {
    logger.warn("No database information specified; not using database.")
    dbcon <- "NONE"
  } else {
    # create connection we'll use
    dbcon <- db.open(settings$database$bety)
  }

  # check database version
  if(!is.character(dbcon)) {
    versions <- db.query("SELECT version FROM schema_migrations WHERE version >= '20130717162614';", con=dbcon)[['version']]
    if (length(versions) == 0) {
      logger.severe("Database is out of date, please update the database;\n",
                    "\t scripts/update.(psql/mysql).sh scripts will install a new, updated (mysql or psql) database",
                    "\t but any changes to your current database will be lost",
                    "otherwise, use Ruby migrations")
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
    logger.warn("No PFTS specified.")
  }

  # check for a run settings
  if (is.null(settings[['run']])) {
    logger.warn("No Run Settings specified")
  }

  # check start/end date are specified and correct
  if (is.null(settings$run$start.date)) {
    logger.warn("No start.date specified in run section.")
  }
  if (is.null(settings$run$end.date)) {
    logger.warn("No end.date specified in run section.")
  }
  startdate <- parse_date_time(settings$run$start.date, "ymd_hms", truncated=3)
  enddate <- parse_date_time(settings$run$end.date, "ymd_hms", truncated=3)
  if (startdate >= enddate) {
    logger.severe("Start date should come before the end date.")
  }

  # check if there is either ensemble or sensitivy.analysis
  if (is.null(settings$ensemble) && is.null(settings$sensitivity.analysis)) {
    logger.warn("No ensemble or sensitivity analysis specified, no models will be executed!")
  }

  # check ensemble
  if (!is.null(settings$ensemble)) {
    if (is.null(settings$ensemble$variable)) {
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

    if(is.null(settings$ensemble$start.year)) {
      if(is.null(settings$sensitivity.analysis$start.year)) {
        settings$ensemble$start.year <- year(settings$run$start.date) 
        logger.info("No start date passed to ensemble - using the run date (", settings$ensemble$start.date, ").")
      } else { 
        settings$ensemble$start.year <- settings$sensitivity.analysis$start.year 
        logger.info("No start date passed to ensemble - using the sensitivity.analysis date (", settings$ensemble$start.date, ").")
      }
    }

    if(is.null(settings$ensemble$end.year)) {
      if(is.null(settings$sensitivity.analysis$end.year)) {
        settings$ensemble$end.year <- year(settings$run$end.date) 
        logger.info("No end date passed to ensemble - using the run date (", settings$ensemble$end.date, ").")
      } else { 
        settings$ensemble$end.year <- settings$sensitivity.analysis$end.year 
        logger.info("No end date passed to ensemble - using the sensitivity.analysis date (", settings$ensemble$end.date, ").")
      }
    }

    # check start and end dates
    if (year(startdate) > settings$ensemble$start.year) {
      logger.severe("Start year of ensemble should come after the start.date of the run")
    }
    if (year(enddate) < settings$ensemble$end.year) {
      logger.severe("End year of ensemble should come before the end.date of the run")
    }
    if (settings$ensemble$start.year > settings$ensemble$end.year) {
      logger.severe("Start year of ensemble should come before the end year of the ensemble")
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

    if(is.null(settings$sensitivity.analysis$start.year)) {
      if(is.null(settings$ensemble$start.year)) {
        settings$sensitivity.analysis$start.year <- year(settings$run$start.date) 
        logger.info("No start date passed to sensitivity.analysis - using the run date (", settings$sensitivity.analysis$start.date, ").")
      } else { 
        settings$sensitivity.analysis$start.year <- settings$ensemble$start.year 
        logger.info("No start date passed to sensitivity.analysis - using the ensemble date (", settings$sensitivity.analysis$start.date, ").")
      }
    }

    if(is.null(settings$sensitivity.analysis$end.year)) {
      if(is.null(settings$ensemble$end.year)) {
        settings$sensitivity.analysis$end.year <- year(settings$run$end.date) 
        logger.info("No end date passed to sensitivity.analysis - using the run date (", settings$sensitivity.analysis$end.date, ").")
      } else { 
        settings$sensitivity.analysis$end.year <- settings$ensemble$end.year 
        logger.info("No end date passed to sensitivity.analysis - using the ensemble date (", settings$sensitivity.analysis$end.date, ").")
      }
    }

    # check start and end dates
    if (year(startdate) > settings$sensitivity.analysis$start.year) {
      logger.severe("Start year of sensitivity.analysis should come after the start.date of the run")
    }
    if (year(enddate) < settings$sensitivity.analysis$end.year) {
      logger.severe("End year of sensitivity.analysis should come before the end.date of the run")
    }
    if (settings$sensitivity.analysis$start.year > settings$sensitivity.analysis$end.year) {
      logger.severe("Start year of sensitivity.analysis should come before the end year of the ensemble")
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
  if (is.null(settings$meta.analysis$update)) {
    settings$meta.analysis$update <- 'AUTO'
    logger.info("Setting meta.analysis update to only update if no previous meta analysis was found")
  }
  if ((settings$meta.analysis$update != 'AUTO') && is.na(as.logical(settings$meta.analysis$update))) {
    logger.info("meta.analysis update can only be AUTO/TRUE/FALSE, defaulting to FALSE")
    settings$meta.analysis$update <- FALSE
  }

  # check modelid with values
  if(!is.null(settings$model)){
    if(!is.character(dbcon)){
      if(!is.null(settings$model$id)){
        if(as.numeric(settings$model$id) >= 0){
          model <- db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), con=dbcon)
          if(nrow(model) == 0) {
            logger.error("There is no record of model_id = ", settings$model$id, "in database")
          }
        } else {
          model <- settings$model
        }
      } else if (!is.null(settings$model$name)) {
        model <- db.query(paste0("SELECT * FROM models WHERE (model_name = '", settings$model$name,
                                 "' or model_type = '", toupper(settings$model$name), "')",
                                 " and model_path like '%", 
                                 ifelse(settings$run$host$name == "localhost", fqdn(), 
                                        settings$run$host$name), "%' ",
                                 ifelse(is.null(settings$model$revision), "", 
                                        paste0(" and revision like '%", settings$model$revision, "%' "))), 
                          con=dbcon)
        if(nrow(model) > 1){
          logger.warn("multiple records for", settings$model$name, "returned; using the most recent")
          row <- which.max(ymd_hms(model$updated_at))
          if (length(row) == 0) row <- nrow(model)
          model <- model[row, ]
        } else if (nrow(model) == 0) {
          logger.warn("Model", settings$model$name, "not in database")
          model <- list(id=-1, name=settings$model$name)
        }
      } else {
        logger.warn("no model settings given")
        model <- list()
      }
    } else {
      if(!is.null(settings$model$name)){
        model <- list(id=-1, name=settings$model$name)        
      } else {
        model <- list()
      }
    }
    
    if (!is.null(settings$model$name)) {
      model$model_type=settings$model$name
    }
    if (!is.null(settings$model$binary)) {
      model$model_path=paste0("hostname:", settings$model$binary)
    }
    if (!is.null(model$model_path)) {
      model$binary <- tail(strsplit(model$model_path, ":")[[1]], 1)        
    }
    
    # copy data from database into missing fields
    if (is.null(settings$model$id)) {
      if ((is.null(model$id) || model$id == "")) {
        logger.warn("No model id specified.")
        settings$model$id <- -1
      } else {
        settings$model$id <- model$id
      }
      logger.info("Setting model id to ", settings$model$id)
    }

    if (is.null(settings$model$name)) {
      if ((is.null(model$model_type) || model$model_type == "")) {
        logger.warn("No model type specified.")
      }
      settings$model$name <- model$model_type
      logger.info("Setting model type to ", settings$model$name)
    } else if ((is.null(model$model_type) || model$model_type == "")) {
      logger.warn("No model type sepcified in database for model ", settings$model$name)
    } else if (model$model_type != settings$model$name) {
      logger.warn("Specified model type [", settings$model$name, "] does not match model_type in database [", model$model_type, "]")
    }
    
    if (is.null(settings$model$binary)) {
      if ((is.null(model$binary) || model$binary == "")) {
        logger.warn("No model binary specified.")
      }
      settings$model$binary <- model$binary
      logger.info("Setting model binary to ", settings$model$binary)
    } else if ((is.null(model$binary) || model$binary == "")) {
      logger.warn("No model binary sepcified in database for model ", settings$model$name)
    } else if (model$binary != settings$model$binary) {
      logger.warn("Specified binary [", settings$model$binary, "] does not match model_path in database [", model$binary, "]")
    }
  }
  # end model check
  
  # check siteid with values
  if(!is.null(settings$run$site)){
    if (is.null(settings$run$site$id)) {
      settings$run$site$id <- -1
    } else if (settings$run$site$id >= 0) {
      if (!is.character(dbcon)) {
        site <- db.query(paste("SELECT * FROM sites WHERE id =", settings$run$site$id), con=dbcon)
      } else {
        site <- data.frame(id=settings$run$site$id)
        if (!is.null(settings$run$site$name)) {
          site$sitename=settings$run$site$name
        }
        if (!is.null(settings$run$site$lat)) {
          site$lat=settings$run$site$lat
        }
        if (!is.null(settings$run$site$lon)) {
          site$lon=settings$run$site$lon
        }
      }
      if((!is.null(settings$run$site$met)) && settings$run$site$met == "NULL") settings$run$site$met <- NULL
      if (is.null(settings$run$site$name)) {
        if ((is.null(site$sitename) || site$sitename == "")) {
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
        if ((is.null(site$lat) || site$lat == "")) {
          logger.severe("No lat specified for site.")
        } else {
          settings$run$site$lat <- as.numeric(site$lat)
          logger.info("Setting site lat to ", settings$run$site$lat)
        }
      } else if (as.numeric(site$lat) != as.numeric(settings$run$site$lat)) {
        logger.warn("Specified site lat [", settings$run$site$lat, "] does not match lat in database [", site$lat, "]")
      }

      if (is.null(settings$run$site$lon)) {
        if ((is.null(site$lon) || site$lon == "")) {
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
  # end site check code

  # check to make sure a host is given
  if (is.null(settings$run$host$name)) {
    logger.info("Setting localhost for execution host.")
    settings$run$host$name <- "localhost"
  }
  ## if run$host is localhost, set to "localhost
  if (any(settings$run$host %in% c(Sys.info()['nodename'], gsub("illinois", "uiuc", Sys.info()['nodename'])))){
    settings$run$host$name <- "localhost"
  }

  # check if we need to use qsub
  if ("qsub" %in% names(settings$run$host)) {
    if (is.null(settings$run$host$qsub)) {
      settings$run$host$qsub <- "qsub -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
      logger.info("qsub not specified using default value :", settings$run$host$qsub)
    }
    if (is.null(settings$run$host$qsub.jobid)) {
      settings$run$host$qsub.jobid <- "Your job ([0-9]+) .*"
      logger.info("qsub.jobid not specified using default value :", settings$run$host$qsub.jobid)
    }
    if (is.null(settings$run$host$qstat)) {
      settings$run$host$qstat <- "qstat -j @JOBID@ &> /dev/null || echo DONE"
      logger.info("qstat not specified using default value :", settings$run$host$qstat)
    }
  }

  # modellauncher to launch on multiple nodes/cores
  if ("modellauncher" %in% names(settings$run$host)) {
    if (is.null(settings$run$host$modellauncher$binary)) {
      settings$run$host$modellauncher$binary <- "modellauncher"
      logger.info("binary not specified using default value :", settings$run$host$modellauncher$binary)
    }
    if (is.null(settings$run$host$modellauncher$qsub.extra)) {
      logger.severe("qsub.extra not specified, can not launch in parallel environment.")
    }
    if (is.null(settings$run$host$modellauncher$mpirun)) {
      settings$run$host$modellauncher$mpirun <- "mpirun"
      logger.info("mpirun not specified using default value :", settings$run$host$modellauncher$mpirun)
    }
  }

  # Check folder where outputs are written before adding to dbfiles
  if(is.null(settings$run$dbfiles)) {
    settings$run$dbfiles <- normalizePath("~/.pecan/dbfiles", mustWork=FALSE)
  } else {
    settings$run$dbfiles <- normalizePath(settings$run$dbfiles, mustWork=FALSE)
  }
  dir.create(settings$run$dbfiles, showWarnings = FALSE, recursive = TRUE)

  # check for workflow defaults
  fixoutdir <- FALSE
  if(!is.character(dbcon) && settings$database$bety$write && ("model" %in% names(settings))) {
    if (!'workflow' %in% names(settings)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      db.query(paste0("INSERT INTO workflows (site_id, model_id, hostname, start_date, end_date, started_at, created_at) values ('",
                      settings$run$site$id, "','", settings$model$id, "', '", settings$run$host$name, "', '",
                      settings$run$start.date, "', '", settings$run$end.date, "', '", now, "', '", now, "')"), con=dbcon)
      settings$workflow$id <- as.character(db.query(paste0("SELECT id FROM workflows WHERE created_at='", now, "';"), con=dbcon)[['id']])
      fixoutdir <- TRUE
    }
  } else {
    settings$workflow$id <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  }

  # check/create the pecan folder
  if (is.null(settings$outdir)) {
    settings$outdir <- "PEcAn_@WORKFLOW@"
  }
  # replace @WORKFLOW@ with the id of the workflow
  settings$outdir <- gsub("@WORKFLOW@", format(settings$workflow$id,scientific=FALSE), settings$outdir)
  # create fully qualified pathname
  if (substr(settings$outdir, 1, 1) != '/') {
    settings$outdir <- file.path(getwd(), settings$outdir)
  }
  logger.info("output folder =", settings$outdir)
  if (!file.exists(settings$outdir) && !dir.create(settings$outdir, recursive=TRUE)) {
    logger.severe("Could not create folder", settings$outdir)
  }

  #update workflow
  if (fixoutdir) {
      db.query(paste0("UPDATE workflows SET folder='", normalizePath(settings$outdir), "' WHERE id=", settings$workflow$id), con=dbcon)
  }

  # check/create the local run folder
  if (is.null(settings$rundir)) {
    settings$rundir <- file.path(settings$outdir, "run")
  }
  if (!file.exists(settings$rundir) && !dir.create(settings$rundir, recursive=TRUE)) {
    logger.severe("Could not create run folder", settings$rundir)
  }

  # check/create the local model out folder
  if (is.null(settings$modeloutdir)) {
    settings$modeloutdir <- file.path(settings$outdir, "out")
  }
  if (!file.exists(settings$modeloutdir) && !dir.create(settings$modeloutdir, recursive=TRUE)) {
    logger.severe("Could not create model out folder", settings$modeloutdir)
  }
  
  # make sure remote folders are specified if need be
  if (!is.null(settings$run$host$qsub) || (settings$run$host$name != "localhost")) {
    homedir <- NA
    if (is.null(settings$run$host$rundir)) {
      if (is.na(homedir)) {
        homedir <- system2("ssh", c(settings$run$host$name, "pwd"), stdout=TRUE)
      }
      settings$run$host$rundir <- paste0(homedir, "/pecan_remote/@WORKFLOW@/run")
    }
    settings$run$host$rundir <- gsub("@WORKFLOW@", settings$workflow$id, settings$run$host$rundir)
    logger.info("Using ", settings$run$host$rundir, "to store runs on remote machine")
    if (is.null(settings$run$host$outdir)) {
      if (is.na(homedir)) {
        homedir <- system2("ssh", c(settings$run$host$name, "pwd"), stdout=TRUE)
      }
      settings$run$host$outdir <- paste0(homedir, "/pecan_remote/@WORKFLOW@/out")
    }
    settings$run$host$outdir <- gsub("@WORKFLOW@", settings$workflow$id, settings$run$host$outdir)
    logger.info("Using ", settings$run$host$outdir, "to store output on remote machine")
  } else if (settings$run$host$name == "localhost") {
    settings$run$host$rundir <- settings$rundir
    settings$run$host$outdir <- settings$modeloutdir
  }

  # check/create the pft folders
  if (!is.null(settings$pfts) && (length(settings$pfts) > 0)) {
    for (i in 1:length(settings$pfts)) {
      #check if name tag within pft
      if (!"name" %in% names(settings$pfts[i]$pft)) {
        logger.severe("No name specified for pft of index: ", i, ", please specify name")
      }
      if (settings$pfts[i]$pft$name == "") {
        logger.severe("Name specified for pft of index: ", i, " can not be empty.")
      }
      
      #check to see if name of each pft in xml file is actually a name of a pft already in database
      if (!is.character(dbcon)) {
        x <- db.query(paste0("SELECT COUNT(*) FROM pfts WHERE name = '",  settings$pfts[i]$pft$name, "';"), con=dbcon)
        if (x$count == 0) {
          logger.severe("Did not find a pft with name ", settings$pfts[i]$pft$name)
        }
        if (x$count > 1) {
          logger.warn("Found multiple entries for pft with name ", settings$pfts[i]$pft$name)
        }
      }
  
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
  }
  
  if (!is.character(dbcon)) {
    db.close(dbcon)
  }
  options(scipen=scipen)
  
  # all done return cleaned up settings
  invisible(settings)
}


##' Loads PEcAn settings file
##' 
##' This will try and find the PEcAn settings file in the following order:
##' \enumerate{
##' \item {--settings <file>}{passed as command line argument using --settings}
##' \item {inputfile}{passed as argument to function}
##' \item {PECAN_SETTINGS}{environment variable PECAN_SETTINGS pointing to a specific file}
##' \item {./pecan.xml}{pecan.xml in the current folder}
##' }
##' Once the function finds a valid file, it will not look further. 
##' Thus, if \code{inputfile} is supplied, \code{PECAN_SETTINGS} will be ignored. 
##' Even if a \code{file} argument is passed, it will be ignored if a file is passed through
##' a higher priority method.  
##' @param inputfile the PEcAn settings file to be used.
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir. If set to null nothing is saved.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' ## bash shell:
##' R --vanilla -- --settings path/to/mypecan.xml < workflow.R 
##' 
##' ## R:
##' 
##' settings <- read.settings()
##' settings <- read.settings(file="willowcreek.xml")
##' test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
##' settings <- read.settings(test.settings.file)
##' }
read.settings <- function(inputfile = "pecan.xml", outputfile = "pecan.xml"){
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
    if (!is.null(inputfile)){
      logger.info("input file ", inputfile, "not used, ", loc, "as environment variable")
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
  if (!is.null(outputfile)) {
    pecanfile <- file.path(settings$outdir, outputfile)
    if (file.exists(pecanfile)) {
      logger.warn(paste("File already exists [", pecanfile, "] file will be overwritten"))
    }
    saveXML(listToXml(settings, "pecan"), file=pecanfile)
  }

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
