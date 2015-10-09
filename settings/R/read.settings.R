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

# check to see if inputs are specified
# this should be part of the model code
check.inputs <- function(settings) {
  if (is.null(settings$model$type)) return(settings)

  # don't know how to check inputs
  if (is.null(settings$database$bety)) {
    logger.info("No databasse connection, can't check inputs.")
    return (settings)
  }

  # get list of inputs associated with model type
  dbcon <- db.open(settings$database$bety)
  inputs <- db.query(paste0("SELECT tag, format_id, required FROM modeltypes, modeltypes_formats WHERE modeltypes_formats.modeltype_id = modeltypes.id and modeltypes.name='", settings$model$type, "' AND modeltypes_formats.input;"), con=dbcon)

  # check list of inputs  
  allinputs <- names(settings$run$inputs)
  if (nrow(inputs) > 0) {
    for(i in 1:nrow(inputs)) {
      tag <- inputs$tag[i]
      hostname <- settings$run$host$name
      allinputs <- allinputs[allinputs != tag]

      # check if tag exists
      if (is.null(settings$run$inputs[[tag]])) {
        if (inputs$required[i]) {
          logger.warn("Missing required input :", tag)
        } else {
          logger.info("Missing optional input :", tag)
        }
        next
      }
      
      # check if <id> exists
      if ("id" %in% names(settings$run$inputs[[tag]])) {
        id <- settings$run$inputs[[tag]][['id']]
        file <- dbfile.file("Input", id, dbcon, hostname)
        if (is.na(file)) {
          logger.error("No file found for", tag, " and id", id, "on host", hostname)
        } else {
          if (is.null(settings$run$inputs[[tag]][['path']])) {
            settings$run$inputs[[tag]]['path'] <- file
          } else if (file != settings$run$inputs[[tag]][['path']]) {
            logger.warn("Input file and id do not match for ", tag)
          }
        }
      } else if ("path" %in% names(settings$run$inputs[[tag]])) {
        # can we find the file so we can set the tag.id
        id <- dbfile.id('Input', settings$run$inputs[[tag]][['path']], dbcon, hostname)
        if (!is.na(id)) {
          settings$run$inputs[[tag]][['id']] <- id
        }
      }
      
      # check to see if format is right type
      if ("id" %in% names(settings$run$inputs[[tag]])) {
        formats <- db.query(paste0("SELECT format_id FROM inputs WHERE id=", settings$run$inputs[[tag]][['id']]), con=dbcon)
        if (nrow(formats) > 1) {
          if (formats[1, 'format_id'] != inputs$format_id[i]) {
            logger.error("Format of input", tag, "does not match specified input.")
          }
        } else if (nrow(formats) == 1) {
          if (formats[1, 'format_id'] != inputs$format_id[i]) {
            logger.error("Format of input", tag, "does not match specified input.")
          }
        } else {
          logger.error("Could not check format of", tag, ".")
        }
      }
    }
  }
  
  if (length(allinputs) > 0) {
    logger.info("Unused inputs found :", paste(allinputs, collapse=" "))
  }
  
  db.close(dbcon)

  return(settings)
}

# check database section
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

  if (!db.exists(params=database, FALSE, table=NA)) {
    logger.severe("Invalid Database Settings : ", unlist(database))
  }

  # connected
  logger.info("Successfully connected to database : ", unlist(database))

  # return fixed up database
  return(database)
} 

# check to make sure BETY is up to date
check.bety.version <- function(dbcon) {
  versions <- db.query("SELECT version FROM schema_migrations;", con=dbcon)[['version']]
  
  # there should always be a versin 1
  if (! ("1" %in% versions)) {
    logger.severe("No version 1, how did this database get created?")
  }
  
  # check for specific version
  if (! ("20140617163304" %in% versions)) {
    logger.severe("Missing migration 20140617163304, this associates files with models.")
  }
  if (! ("20140708232320" %in% versions)) {
    logger.severe("Missing migration 20140708232320, this introduces geometry column in sites")
  }
  if (! ("20140729045640" %in% versions)) {
    logger.severe("Missing migration 20140729045640, this introduces modeltypes table")
  }
  
  # check if database is newer
  if (tail(versions, n=1) > "20141009160121") {
    logger.warn("Last migration", tail(versions, n=1), "is more recent than expected 20141009160121.",
                "This could result in PEcAn not working as expected.")
  }
}

##' Sanity checks. Checks the settings file to make sure expected fields exist. It will try to use
##' default values for any missing values, or stop the exection if no defaults are possible.
##'
##' Expected fields in settings file are:
##' - pfts with at least one pft defined
##' @title Check Settings
##' @param settings settings file
##' @return will return the updated settings values with defaults set.
##' @author Rob Kooper, David LeBauer
check.settings <- function(settings) {
  if (!is.null(settings$nocheck)) {
    logger.info("Not doing sanity checks of pecan.xml")
    return(settings)
  }
  scipen = getOption("scipen")
  options(scipen=12)

  # check database secions if exist
  dbcon <- "NONE"
  if (!is.null(settings$database)) {
    # check all databases
    for (name in names(settings$database)) {
      settings$database[[name]] <- check.database(settings$database[[name]])
    }

    # check bety database
    if (!is.null(settings$database$bety)) {
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
      if (settings$database$bety$write && !db.exists(params=settings$database$bety, TRUE, table='users')) {
        logger.severe("Invalid Database Settings : ", unlist(settings$database))
      }

      # TODO check userid and userpassword

      # Connect to database
      dbcon <- db.open(settings$database$bety)

      # check database version
      check.bety.version(dbcon)

    } else {
      logger.warn("No BETY database information specified; not using database.")
    }
  } else {
    logger.warn("No BETY database information specified; not using database.")
  }
  
  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    logger.warn("No PFTS specified.")
  }

  # check for a run settings
  if (is.null(settings[['run']])) {
    logger.warn("No Run Settings specified")
  }
  # check to make sure a host is given
  if (is.null(settings$run$host$name)) {
    logger.info("Setting localhost for execution host.")
    settings$run$host$name <- "localhost"
  }
  
  # check start/end date are specified and correct
  if (is.null(settings$run$start.date)) {
    logger.warn("No start.date specified in run section.")
  } else if (is.null(settings$run$end.date)) {
    logger.warn("No end.date specified in run section.")
  } else {
    startdate <- parse_date_time(settings$run$start.date, "ymd_hms", truncated=3)
    enddate <- parse_date_time(settings$run$end.date, "ymd_hms", truncated=3)
    if (startdate >= enddate) {
      logger.severe("Start date should come before the end date.")
    }
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

  # check modelid with values
  if(!is.null(settings$model)){
    if(!is.character(dbcon)){
      if(!is.null(settings$model$id)){
        if(as.numeric(settings$model$id) >= 0){
          model <- db.query(paste0("SELECT models.id AS id, models.revision AS revision, modeltypes.name AS type FROM models, modeltypes WHERE models.id=", settings$model$id, " AND models.modeltype_id=modeltypes.id;"), con=dbcon)
          if(nrow(model) == 0) {
            logger.error("There is no record of model_id = ", settings$model$id, "in database")
          }
        } else {
          model <- list()
        }
      } else if (!is.null(settings$model$type)) {
        model <- db.query(paste0("SELECT models.id AS id, models.revision AS revision, modeltypes.name AS type FROM models, modeltypes ",
                                 "WHERE modeltypes.name = '", toupper(settings$model$type), "' ",
                                 "AND models.modeltype_id=modeltypes.id ",
                                 ifelse(is.null(settings$model$revision), "", 
                                        paste0("AND revision like '%", settings$model$revision, "%' ")),
                                 "ORDER BY models.updated_at"), con=dbcon)
        if(nrow(model) > 1){
          logger.warn("multiple records for", settings$model$name, "returned; using the latest")
          row <- which.max(model$updated_at)
          if (length(row) == 0) row <- nrow(model)
          model <- model[row, ]
        } else if (nrow(model) == 0) {
          logger.warn("Model type", settings$model$type, "not in database")
        }
      } else {
        logger.warn("no model settings given")
        model <- list()
      }
    } else {
      model <- list()
    }

    # copy data from database into missing fields
    if (!is.null(model$id)) {
      if (is.null(settings$model$id) || (settings$model$id == "")) {
        settings$model$id <- model$id
        logger.info("Setting model id to ", settings$model$id)
      } else if (settings$model$id != model$id) {
        logger.warn("Model id specified in settings file does not match database.")
      }
    } else {
      if (is.null(settings$model$id) || (settings$model$id == "")) {
        settings$model$id <- -1
        logger.info("Setting model id to ", settings$model$id)
      }
    }
    if (!is.null(model$type)) {
      if (is.null(settings$model$type) || (settings$model$type == "")) {
        settings$model$type <- model$type
        logger.info("Setting model type to ", settings$model$type)
      } else if (settings$model$type != model$type) {
        logger.warn("Model type specified in settings file does not match database.")
      }
    }
    if (!is.null(model$revision)) {
      if (is.null(settings$model$revision) || (settings$model$revision == "")) {
        settings$model$revision <- model$revision
        logger.info("Setting model revision to ", settings$model$revision)
      } else if (settings$model$revision != model$revision) {
        logger.warn("Model revision specified in settings file does not match database.")
      }
    }

    # make sure we have model type
    if ((is.null(settings$model$type) || settings$model$type == "")) {
      logger.severe("Need a model type.")
    }
    
    # Set model$delete.raw to FALSE by default
    if (is.null(settings$model$delete.raw) || !is.logical(as.logical(settings$model$delete.raw))) {
      logger.info("Option to delete raw model output not set or not logical. Will keep all model output.")
      settings$model$delete.raw = FALSE
    }

    # check on binary for given host
    if (!is.null(settings$model$id) && (settings$model$id >= 0)) {
      binary <- dbfile.file("Model", settings$model$id, dbcon, settings$run$host$name)
      if (!is.na(binary)) {
        if (is.null(settings$model$binary)) {
          settings$model$binary <- binary
          logger.info("Setting model binary to ", settings$model$binary)
        } else if (binary != settings$model$binary) {
          logger.warn("Specified binary [", settings$model$binary, "] does not match path in database [", binary, "]")
        }
      }
    } else {
      logger.warn("No model binary sepcified in database for model ", settings$model$type)      
    }
  }
  # end model check
  
  # check siteid with values
  if(!is.null(settings$run$site)){
    if (is.null(settings$run$site$id)) {
      settings$run$site$id <- -1
    } else if (settings$run$site$id >= 0) {
      if (!is.character(dbcon)) {
        site <- db.query(paste("SELECT sitename, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =", settings$run$site$id), con=dbcon)
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
  } else {
    settings$run$site$id <- -1
  }
  # end site check code

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
    settings$run$dbfiles <- full.path("~/.pecan/dbfiles")
  } else {
      if (substr(settings$run$dbfiles, 1, 1) != '/'){
          logger.warn("settings$run$dbfiles pathname", settings$run$dbfiles, " is invalid\n
                  placing it in the home directory ", Sys.getenv("HOME"))
          settings$run$dbfiles <- file.path(Sys.getenv("HOME"), settings$run$dbfiles)
      } 
      
      settings$run$dbfiles <- normalizePath(settings$run$dbfiles, mustWork=FALSE)
  }
  dir.create(settings$run$dbfiles, showWarnings = FALSE, recursive = TRUE)

  # check all inputs exist
  settings <- check.inputs(settings)

  # check for workflow defaults
  fixoutdir <- FALSE
  if(!is.character(dbcon) && settings$database$bety$write && ("model" %in% names(settings))) {
    if (!'workflow' %in% names(settings)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      db.query(paste0("INSERT INTO workflows (folder, site_id, model_id, hostname, start_date, end_date, started_at, created_at) values ('",
                      settings$outdir, "','" , settings$run$site$id, "','", settings$model$id, "', '", settings$run$host$name, "', '",
                      settings$run$start.date, "', '", settings$run$end.date, "', '", now, "', '", now, "')"), con=dbcon)
      settings$workflow$id <- db.query(paste0("SELECT id FROM workflows WHERE created_at='", now, "' ORDER BY id DESC LIMIT 1;"), con=dbcon)[['id']]
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
      db.query(paste0("UPDATE workflows SET folder='", full.path(settings$outdir), "' WHERE id=", settings$workflow$id), con=dbcon)
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
      if (!is.character(dbcon)) {# change to if(class(dbcon) == "PostgreSQLConnection")??
        if (is.null(settings$model$type)) {
          x <- db.query(paste0("SELECT pfts.id FROM pfts",
                               " WHERE pfts.name = '",  settings$pfts[i]$pft$name, "'"), con=dbcon)
        } else {
          x <- db.query(paste0("SELECT pfts.id FROM pfts, modeltypes",
                               " WHERE pfts.name = '",  settings$pfts[i]$pft$name, "'",
                               " AND modeltypes.name='", settings$model$type, "'",
                               " AND modeltypes.id=pfts.modeltype_id;"), con=dbcon)
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

##' Updates a pecan.xml file to match new layout. This will take care of the
##' conversion to the latest pecan.xml file.
##'
##' @title Update Settings
##' @param settings settings file
##' @return will return the updated settings values
##' @author Rob Kooper
update.settings <- function(settings) {
  # update database section, now have different database definitions
  # under database section, e.g. fia and bety
  if (!is.null(settings$database)) {
    # simple check to make sure the database tag is updated
    if (!is.null(settings$database$dbname)) {
      if (!is.null(settings$database$bety)) {
        logger.severe("Please remove dbname etc from database configuration.")
      }

      logger.info("Database tag has changed, please use <database><bety> to store",
                 "information about accessing the BETY database. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#database-access.")

      bety <- list()
      for(name in names(settings$database)) {
        bety[[name]] <- settings$database[[name]]
      }
      settings$database <- list(bety=bety)
    }

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
  }

  # model$model_type is now simply model$type and model$name is no longer used
  if (!is.null(settings$model$model_type)) {
    if (!is.null(settings$model$type)) {
      if (settings$model$model_type != settings$model$type) {
        logger.severe("Please remove model_type from model configuration.")
      } else {
        logger.info("Please remove model_type from model configuration.")
      }
    }

    logger.info("Model tag has changed, please use <model><type> to specify",
                 "type of model. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#model_setup.")
    settings$model$type <- settings$model$model_type
    settings$model$model_type <- NULL
  }
  if (!is.null(settings$model$name)) {
    if (!is.null(settings$model$type)) {
      if (settings$model$name != settings$model$type) {
        logger.severe("Please remove name from model configuration.")
      } else {
        logger.info("Please remove name from model configuration.")
      }
    }

    logger.info("Model tag has changed, please use <model><type> to specify",
                 "type of model. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#model_setup.")
    settings$model$type <- settings$model$name
    settings$model$name <- NULL
  }

  # run$site$met is now run$inputs$met$path
  if (!is.null(settings$run$site$met)) {
    if (!is.null(settings$run$inputs$met)) {
      if (settings$run$site$met != settings$run$inputs$met) {
        logger.severe("Please remove met from model configuration.")
      } else {
        logger.info("Please remove met from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    logger.info("Model tag has changed, please use <inputs><met> to specify",
                 "met file for a run. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#run_setup.")
    settings$run$inputs$met$path <- settings$run$site$met
    settings$run$site$met <- NULL
  }
  
  # inputs now have path and id under tag
  for(tag in names(settings$run$inputs)) {
    if (grepl(".id$", tag)) {
      tagid <- tag
      tag <- substr(tagid, 1, nchar(tagid)-3)
      if (tag %in% names(settings$run$inputs)) {
        next
      } else {
        settings$run$inputs[[tag]]['id'] <- settings$run$inputs[[tagid]]
        settings$run$inputs[[tagid]] <- null
      }
    } else {
      if (!is.list(settings$run$inputs[[tag]])) {
        path <- settings$run$inputs[[tag]]
        settings$run$inputs[[tag]] <- list("path"=path)
      }

      tagid <- paste0(tag, ".id")
      if (tagid %in% names(settings$run$inputs)) {
        if ('id' %in% names(settings$run$inputs[[tag]])) {
          if (settings$run$inputs[[tagid]] != settings$run$inputs[[tag]][['id']]) {
            logger.severe("Please remove", tagid, "from inputs configuration.")
          } else {
            logger.info("Please remove", tagid, "from inputs configuration.")
          }
          settings$run$inputs[[tagid]] <- NULL
        } else {
          settings$run$inputs[[tag]][['id']] <- settings$run$inputs[[tagid]]
          settings$run$inputs[[tagid]] <- NULL
        }
      }
    }
  }

  # some specific ED changes
  if (!is.null(settings$model$veg)) {
    if (!is.null(settings$run$inputs$veg)) {
      if (settings$model$veg != settings$run$inputs$veg) {
        logger.severe("Please remove veg from model configuration.")
      } else {
        logger.info("Please remove veg from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    logger.info("Model tag has changed, please use <inputs><veg> to specify",
                 "veg file for a run. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#run_setup.")
    settings$run$inputs$veg <- settings$model$veg
    settings$model$veg <- NULL
  }
  if (!is.null(settings$model$soil)) {
    if (!is.null(settings$run$inputs$soil)) {
      if (settings$model$soil != settings$run$inputs$soil) {
        logger.severe("Please remove soil from model configuration.")
      } else {
        logger.info("Please remove soil from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    logger.info("Model tag has changed, please use <inputs><soil> to specify",
                 "soil file for a run. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#run_setup.")
    settings$run$inputs$soil <- settings$model$soil
    settings$model$soil <- NULL
  }
  if (!is.null(settings$model$psscss)) {
    if (!is.null(settings$run$inputs$pss)) {
      logger.info("Please remove psscss from model configuration.")
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    logger.info("Model tag has changed, please use <inputs><pss/css/site> to specify",
                 "pss/css/site file for a run. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#run_setup.")
    settings$run$inputs$pss <- file.path(settings$model$psscss, "foo.pss")
    settings$run$inputs$css <- file.path(settings$model$psscss, "foo.css")
    settings$run$inputs$site <- file.path(settings$model$psscss, "foo.site")
    settings$model$psscss <- NULL
  }
  if (!is.null(settings$model$inputs)) {
    if (!is.null(settings$run$inputs$inputs)) {
      logger.info("Please remove inputs from model configuration.")
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    logger.info("Model tag has changed, please use <inputs><lu/thsums> to specify",
                 "lu/thsums file for a run. See also",
                 "https://github.com/PecanProject/pecan/wiki/PEcAn-Configuration#run_setup.")
    settings$run$inputs$lu <- file.path(settings$model$inputs, "glu")
    settings$run$inputs$thsums <- settings$model$inputs
    settings$model$soil <- NULL
  }

  invisible(settings)
}

##' Add secret information from ~/.pecan.xml
##'
##' Copies certains sections from ~/.pecan.xml to the settings. This allows
##' a user to have their own unique parameters also when sharing the
##' pecan.xml file we don't expose these secrets.
##' Currently this will copy the database and browndog sections
##'
##' @title Add Users secrets
##' @param settings settings file
##' @return will return the updated settings values
##' @author Rob Kooper
addSecrets <- function(settings) {
  if (!file.exists("~/.pecan.xml")) {
    return(settings)
  }
  pecan <- xmlToList(xmlParse("~/.pecan.xml"))
  
  # always copy following sections
  for(key in c('database')) {
    for(section in names(pecan[[key]])) {
      if (section %in% names(settings[section])) {
        logger.info("Already have a section for", section)
      } else {
        logger.info("Imported section for", section)
        settings[[key]][section] <- pecan[[key]][section]
      }
    }
  }
  
  # only copy these sections if tag exists
  for(key in c('browndog')) {
    if (! key %in% names(settings)) next
    
    for(section in names(pecan[[key]])) {
      if (section %in% names(settings[section])) {
        logger.info("Already have a section for", section)
      } else {
        logger.info("Imported section for", section)
        settings[[key]][section] <- pecan[[key]][section]
      }
    }
  }  

  invisible(settings)
}

##--------------------------------------------------------------------------------------------------#
## EXTERNAL FUNCTIONS
##--------------------------------------------------------------------------------------------------#

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
##' @author David LeBauer
##' @examples
##' \dontrun{
##' ## bash shell:
##' ## example workflow.R and pecan.xml files in pecan/tests
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
  ## If settings file passed at cmd line
  if (length(loc) != 0) {  
    # 1 filename is passed as argument to R
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        logger.info("Loading --settings=", commandArgs()[idx+1])
        xml <- xmlParse(commandArgs()[idx+1])
        break
      }
    }
    ## if settings file on $PATH
  } else if (file.exists(Sys.getenv("PECAN_SETTINGS"))) { 
    # 2 load from PECAN_SETTINGS
    logger.info("Loading PECAN_SETTINGS=", Sys.getenv("PECAN_SETTINGS"))
    xml <- xmlParse(Sys.getenv("PECAN_SETTINGS"))
    ## if settings file passed to read.settings function
  } else if(!is.null(inputfile) && file.exists(inputfile)) {
    # 3 filename passed into function
    logger.info("Loading inpufile=", inputfile)
    xml <- xmlParse(inputfile)
    ## use pecan.xml in cwd only if none exists
  } else if (file.exists("pecan.xml")) {
    # 4 load ./pecan.xml
    logger.info("Loading ./pecan.xml")
    xml <- xmlParse("pecan.xml")
  } else {
    # file not found
    logger.severe("Could not find a pecan.xml file")
  }

  ## convert the xml to a list for ease and return
  settings <- xmlToList(xml)
  settings <- addSecrets(settings)
  settings <- update.settings(settings)
  settings <- check.settings(settings)

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
