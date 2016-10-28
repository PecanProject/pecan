.check.database.settings <- function(settings) {
  if (!is.null(settings$database)) {
    # check all databases
    for (name in names(settings$database)) {
      if(name != "dbfiles") # 'dbfiles' is kept in <database>, but isn't actually a db settings block
        settings$database[[name]] <- .check.database(settings$database[[name]])
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
      if (settings$database$bety$write &&
          !db.exists(params = settings$database$bety, TRUE, table = 'users')) {
        logger.severe("Invalid Database Settings : ", unlist(settings$database))
      }
      
      # TODO check userid and userpassword
      
      # Connect to database
      dbcon <- db.open(settings$database$bety)
      on.exit(db.close(dbcon))
      
      # check database version
      .check.bety.version(dbcon)
    } else {
      logger.warn("No BETY database information specified; not using database.")
    }
  } else {
    logger.warn("No BETY database information specified; not using database.")
  }
  return(settings)
}


# check database section
.check.database <- function(database) {
  if (is.null(database)) {
    return(NULL)
  }
  
  ## check database settings
  if (is.null(database$driver)) {
    database$driver <- "PostgreSQL"
    logger.warn("Please specify a database driver; using default 'PostgreSQL'")
  }
  
  # Attempt to load the driver
  if (!require(paste0("R", database$driver), character.only = TRUE)) {
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
  if(!is.null(database$host)) {
    forcastnames <- c("ebi-forecast.igb.uiuc.edu",
                      "ebi-forecast.igb.illinois.edu") 
    if((database$host %in% forcastnames) &
       (Sys.info()['nodename'] %in% forcastnames)) {
      database$host <- "localhost"
    }
  } else if(is.null(database$host)) {
    database$host <- "localhost"
  }
  
  ## convert strings around from old format to new format
  if(is.null(database[["user"]])) {
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
  
  if (!db.exists(params = database, FALSE, table = NA)) {
    logger.severe("Invalid Database Settings : ", unlist(database))
  }
  
  # connected
  logger.info("Successfully connected to database : ", unlist(database))
  
  # return fixed up database
  return(database)
}



# check to make sure BETY is up to date
.check.bety.version <- function(dbcon) {
  versions <- db.query("SELECT version FROM schema_migrations;", con=dbcon)[['version']]
  
  # there should always be a version 1
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
  if (! ("20151011190026" %in% versions)) {
    logger.severe("Missing migration 20151011190026, this introduces notes and user_id in workflows")
  }
  
  # check if database is newer
  if (tail(versions, n=1) > "20141009160121") {
    logger.warn("Last migration", tail(versions, n = 1), "is more recent than expected 20141009160121.",
                "This could result in PEcAn not working as expected.")
  }
} # .check.bety.version

