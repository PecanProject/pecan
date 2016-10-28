.check.run.settings <- function(settings, dbcon = NULL) {
  scipen <- getOption("scipen")
  options(scipen = 12)
  
  # check for a run settings
  if (is.null(settings[['run']])) {
    logger.warn("No Run Settings specified")
  }
  
  
  # check start/end date are specified and correct
  if (is.null(settings$run$start.date)) {
    logger.warn("No start.date specified in run section.")
  } else if (is.null(settings$run$end.date)) {
    logger.warn("No end.date specified in run section.")
  } else {
    startdate <- lubridate::parse_date_time(settings$run$start.date, "ymd_HMS", truncated = 3)
    enddate <- lubridate::parse_date_time(settings$run$end.date, "ymd_HMS", truncated = 3)
    if (startdate >= enddate) {
      logger.severe("Start date should come before the end date.")
    }
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
      if(!is.null(settings$run$start.date)) {
        settings$ensemble$start.year <- lubridate::year(settings$run$start.date) 
        logger.info("No start date passed to ensemble - using the run date (", 
                    settings$ensemble$start.year, ").")
      } else if(!is.null(settings$sensitivity.analysis$start.year)) {
        settings$ensemble$start.year <- settings$sensitivity.analysis$start.year 
        logger.info("No start date passed to ensemble - using the sensitivity.analysis date (",
                    settings$ensemble$start.year, ").")
      } else {
        logger.info("No start date passed to ensemble, and no default available.")
      }
    }
    
    if(is.null(settings$ensemble$end.year)) {
      if(!is.null(settings$run$end.date)) {
        settings$ensemble$end.year <- lubridate::year(settings$run$end.date) 
        logger.info("No end date passed to ensemble - using the run date (", 
                    settings$ensemble$end.year, ").")
      } else if(!is.null(settings$sensitivity.analysis$end.year)) { 
        settings$ensemble$end.year <- settings$sensitivity.analysis$end.year 
        logger.info("No end date passed to ensemble - using the sensitivity.analysis date (",  
                    settings$ensemble$end.year, ").")
      } else {
        logger.info("No end date passed to ensemble, and no default available.")
      }
    }
    
    # check start and end dates
    if (exists("startdate") && !is.null(settings$ensemble$start.year) &&
        lubridate::year(startdate) > settings$ensemble$start.year) {
      logger.severe("Start year of ensemble should come after the start.date of the run")
    }
    if (exists("enddate") && !is.null(settings$ensemble$end.year) &&
        lubridate::year(enddate) < settings$ensemble$end.year) {
      logger.severe("End year of ensemble should come before the end.date of the run")
    }
    if (!is.null(settings$ensemble$start.year) && !is.null(settings$ensemble$end.year) &&
        settings$ensemble$start.year > settings$ensemble$end.year) {
      logger.severe("Start year of ensemble should come before the end year of the ensemble")
    }
  }
  
  # check sensitivity analysis
  if (!is.null(settings$sensitivity.analysis)) {
    if (is.null(settings$sensitivity.analysis$variable)) {
      if (is.null(settings$ensemble$variable)) {
        logger.severe("No variable specified to compute sensitivity.analysis for.")
      }
      logger.info("Setting sensitivity.analysis variable to the same as ensemble variable [", 
                  settings$ensemble$variable, "]")
      settings$sensitivity.analysis$variable <- settings$ensemble$variable
    }
    
    if(is.null(settings$sensitivity.analysis$start.year)) {
      if(!is.null(settings$run$start.date)) {
        settings$sensitivity.analysis$start.year <- lubridate::year(settings$run$start.date) 
        logger.info("No start date passed to sensitivity.analysis - using the run date (",
                    settings$sensitivity.analysis$start.year, ").")
      } else if(!is.null(settings$ensemble$start.year)) {
        settings$sensitivity.analysis$start.year <- settings$ensemble$start.year 
        logger.info("No start date passed to sensitivity.analysis - using the ensemble date (", 
                    settings$sensitivity.analysis$start.year, ").")
      } else {
        logger.info("No start date passed to sensitivity.analysis, and no default available.")
      }
    }
    
    if(is.null(settings$sensitivity.analysis$end.year)) {
      if(!is.null(settings$run$end.date)) {
        settings$sensitivity.analysis$end.year <- lubridate::year(settings$run$end.date) 
        logger.info("No end date passed to sensitivity.analysis - using the run date (", 
                    settings$sensitivity.analysis$end.year, ").")
      } else if(!is.null(settings$ensemble$end.year)){ 
        settings$sensitivity.analysis$end.year <- settings$ensemble$end.year 
        logger.info("No end date passed to sensitivity.analysis - using the ensemble date (", 
                    settings$sensitivity.analysis$end.year, ").")
      } else {
        logger.info("No end date passed to sensitivity.analysis, and no default available.")
      }
    }
    
    # check start and end dates
    if (exists("startdate") && !is.null(settings$sensitivity.analysis$start.year) &&
        lubridate::year(startdate) > settings$sensitivity.analysis$start.year) {
      logger.severe("Start year of SA should come after the start.date of the run")
    }
    if (exists("enddate") && !is.null(settings$sensitivity.analysis$end.year) &&
        lubridate::year(enddate) < settings$sensitivity.analysis$end.year) {
      logger.severe("End year of SA should come before the end.date of the run")
    }
    if (!is.null(settings$sensitivity.analysis$start.year) && 
        !is.null(settings$sensitivity.analysis$end.year) &&
        settings$sensitivity.analysis$start.year > settings$sensitivity.analysis$end.year) {
      logger.severe("Start year of SA should come before the end year of the SA")
    }
  }
  
  # check siteid with values
  if(!is.null(settings$run$site)) {
    if (is.null(settings$run$site$id)) {
      settings$run$site$id <- -1
    } else if (settings$run$site$id >= 0) {
      if (!is.null(dbcon)) {
        site <- db.query(paste("SELECT sitename, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =", settings$run$site$id), con=dbcon)
      } else {
        site <- data.frame(id = settings$run$site$id)
        if (!is.null(settings$run$site$name)) {
          site$sitename <- settings$run$site$name
        }
        if (!is.null(settings$run$site$lat)) {
          site$lat <- settings$run$site$lat
        }
        if (!is.null(settings$run$site$lon)) {
          site$lon <- settings$run$site$lon
        }
      }
      if((!is.null(settings$run$site$met)) && settings$run$site$met == "NULL") {
        settings$run$site$met <- NULL
      }
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
  
  options(scipen = scipen)
  
  # all done return cleaned up settings
  return(invisible(settings))
}