##-----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-----------------------------------------------------------------------------

#' check to see if inputs are specified - this should be part of the model code
#' @title Check Inputs
#' @param settings settings file
#' @export check.inputs
check.inputs <- function(settings) {
  if (is.null(settings$model$type)) return(settings)

  # don't know how to check inputs
  if (is.null(settings$database$bety)) {
    PEcAn.logger::logger.info("No database connection, can't check inputs.")
    return(settings)
  }

  # get list of inputs associated with model type
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)

  inputs <- PEcAn.DB::db.query(
    paste0(
      "SELECT tag, format_id, required FROM modeltypes, modeltypes_formats ",
      "WHERE modeltypes_formats.modeltype_id = modeltypes.id ",
        "AND modeltypes.name='", settings$model$type, "' ",
        "AND modeltypes_formats.input"),
    con = dbcon)

  # check list of inputs
  allinputs <- names(settings$run$inputs)
  if (nrow(inputs) > 0) {
    for (i in seq_len(nrow(inputs))) {
      tag <- inputs$tag[i]
      hostname <- settings$host$name
      allinputs <- allinputs[allinputs != tag]

      # check if tag exists
      if (is.null(settings$run$inputs[[tag]])) {
        if (inputs$required[i]) {
          PEcAn.logger::logger.warn("Missing required input :", tag)
        } else {
          PEcAn.logger::logger.info("Missing optional input :", tag)
        }
        next
      }

      # check if <id> exists
      if ("id" %in% names(settings$run$inputs[[tag]])) {
        id <- settings$run$inputs[[tag]][["id"]]
        file <- PEcAn.DB::dbfile.file("Input", id, dbcon, hostname)
        if (is.na(file)) {
          PEcAn.logger::logger.error(
            "No file found for", tag, " and id", id, "on host", hostname)
        } else {
          if (is.null(settings$run$inputs[[tag]][["path"]])) {
            settings$run$inputs[[tag]]["path"] <- file
          } else if (file != settings$run$inputs[[tag]][["path"]]) {
            PEcAn.logger::logger.warn(
              "Input file and id do not match for ", tag)
          }
        }
      } else if ("path" %in% names(settings$run$inputs[[tag]])) {
        # can we find the file so we can set the tag.id
        id <- PEcAn.DB::dbfile.id(
          "Input",
          settings$run$inputs[[tag]][["path"]],
          dbcon,
          hostname)
        if (!is.na(id)) {
          settings$run$inputs[[tag]][["id"]] <- id
        }
      }
      PEcAn.logger::logger.info("path", settings$run$inputs[[tag]][["path"]])
      # check to see if format is right type
      if ("id" %in% names(settings$run$inputs[[tag]])) {
        formats <- PEcAn.DB::db.query(
          paste0(
            "SELECT format_id FROM inputs WHERE id=",
            settings$run$inputs[[tag]][["id"]]),
          con = dbcon)
        if (nrow(formats) >= 1) {
          if (formats[1, "format_id"] != inputs$format_id[i]) {
            PEcAn.logger::logger.warn(
              "@Format of input", tag,
              "does not match specified input:",
              formats[1, "format_id"], inputs$format_id[i])
            # zero out path, do_conversions will need to convert specified
            # input ID to model format
            settings$run$inputs[[tag]][["path"]] <- NULL
          }
        } else {
          PEcAn.logger::logger.error("Could not check format of", tag, ".")
        }
      }
      PEcAn.logger::logger.info("path", settings$run$inputs[[tag]][["path"]])
    }
  }

  if (length(allinputs) > 0) {
    PEcAn.logger::logger.info(
      "Unused inputs found :",
      paste(allinputs, collapse = " "))
  }

  return(settings)
}

# check database section
#' @title Check Database
#' @param database settings list to check.
#'    You'll probably use `settings$database`
#' @export check.database
check.database <- function(database) {
  if (is.null(database)) return(NULL)

  ## check database settings
  if (is.null(database$driver)) {
    database$driver <- "PostgreSQL"
    PEcAn.logger::logger.info("Database driver unspecified. ",
                              "Using 'PostgreSQL' (default)")
  }

  is_postgres_like <- database$driver %in% c("PostgreSQL", "Postgres")

  if (!is_postgres_like) {
    PEcAn.logger::logger.severe(
      "Database driver `", database$driver, "` is not supported. ",
      "Please use `PostgreSQL` or `Postgres`."
    )
  }

  if (database$driver == "Postgres") {
    PEcAn.logger::logger.warn(
      "Support for RPostgres is experimental. ",
      "Use at your own risk!."
    )
  }

  # Attempt to load the driver
  rdriver <- paste0("R", database$driver)
  if (!requireNamespace(rdriver, quietly = TRUE)) {
    PEcAn.logger::logger.severe("Could not load the database driver: ", rdriver)
  }

  # PostgreSQL specific checks
  if (is_postgres_like) {
    if (!is.null(database$passwd)) {
      PEcAn.logger::logger.warn(
        "Database field `passwd` is deprecated. ",
        "Please use `password` instead."
      )
      database$password <- database$passwd
      database$passwd <- NULL
    }
    if (!is.null(database$name)) {
      PEcAn.logger::logger.warn(
        "Database field `name` is deprecated. ",
        "Please use `dbname` instead."
      )
      database$dbname <- database$name
      database$name <- NULL
    }
  }

  ## The following hack handles *.illinois.* to *.uiuc.* aliases of ebi-forecast
  if (!is.null(database$host)) {
    forcastnames <- c("ebi-forecast.igb.uiuc.edu",
                      "ebi-forecast.igb.illinois.edu")
    if ((database$host %in% forcastnames)
        && (Sys.info()["nodename"] %in% forcastnames)) {
      database$host <- "localhost"
    }
  } else if (is.null(database$host)) {
    database$host <- "localhost"
  }

  ## convert strings around from old format to new format
  if (is.null(database[["user"]])) {
    if (!is.null(database$userid)) {
      PEcAn.logger::logger.info("'userid' in database section should be 'user'")
      database$user <- database$userid

    } else if (!is.null(database$username)) {
      PEcAn.logger::logger.info(
        "'username' in database section should be 'user'")
      database$user <- database$username

    } else {
      PEcAn.logger::logger.info("no database user specified, using 'bety'")
      database$user <- "bety"
    }
  }
  database$userid <- database$username <- NULL

  # fill in defaults for the database
  if (is.null(database$password)) {
    database$password <- "bety"
  }
  if (is.null(database$dbname)) {
    database$dbname <- "bety"
  }

  if (!PEcAn.DB::db.exists(params = database, FALSE, table = NA)) {
    PEcAn.logger::logger.severe(
      "Invalid Database Settings : ", unlist(database))
  }

  # connected
  PEcAn.logger::logger.info(
    "Successfully connected to database : ", unlist(database))

  # return fixed up database
  return(database)
}

#' check to make sure BETY is up to date
#' @title Check BETY Version
#' @param dbcon database connection object
#' @export check.bety.version
check.bety.version <- function(dbcon) {
  versions <- PEcAn.DB::db.query(
    "SELECT version FROM schema_migrations;",
    con = dbcon)[["version"]]

  # there should always be a version 1
  if (! ("1" %in% versions)) {
    PEcAn.logger::logger.severe(
      "No version 1, how did this database get created?")
  }

  # check for specific version
  if (! ("20140617163304" %in% versions)) {
    PEcAn.logger::logger.severe(
      "Missing migration 20140617163304, this associates files with models.")
  }
  if (! ("20140708232320" %in% versions)) {
    PEcAn.logger::logger.severe(
      "Missing migration 20140708232320,",
      "this introduces geometry column in sites")
  }
  if (! ("20140729045640" %in% versions)) {
    PEcAn.logger::logger.severe("Missing migration 20140729045640,",
      "this introduces modeltypes table")
  }
  if (! ("20151011190026" %in% versions)) {
    PEcAn.logger::logger.severe("Missing migration 20151011190026,",
      "this introduces notes and user_id in workflows")
  }

  # check if database is newer
  last_migration_date <- lubridate::ymd_hms(utils::tail(versions, n = 1))
  pecan_release_date <- lubridate::ymd(
    utils::packageDescription("PEcAn.DB")$Date)
  if (last_migration_date > pecan_release_date) {
    PEcAn.logger::logger.warn(
      "Last database migration", utils::tail(versions, n = 1),
      "is more recent than this", pecan_release_date, "release of PEcAn.",
      "This could result in PEcAn not working as expected.")
  }
}

#' Sanity checks. Checks the settings file to make sure expected fields exist.
#'  It will try to use default values for any missing values,
#'  or stop the exection if no defaults are possible.
#'
#' Expected fields in settings file are:
#' - pfts with at least one pft defined
#' @title Check Settings
#' @param settings settings file
#' @return will return the updated settings values with defaults set.
#' @author Rob Kooper, David LeBauer
#' @export check.settings
check.settings <- function(settings, force = FALSE) {
  if (!force
      && !is.null(settings$settings.info$checked)
      && settings$settings.info$checked == TRUE) {
    PEcAn.logger::logger.info("Settings have been checked already. Skipping.")
    return(invisible(settings))
  } else {
    PEcAn.logger::logger.info("Checking settings...")
  }

  if (is.MultiSettings(settings)) {
    return(invisible(papply(settings, check.settings, force = force)))
  }

  scipen <- getOption("scipen")
  on.exit(options(scipen = scipen), add = TRUE)
  options(scipen = 12)


  settings <- check.database.settings(settings)
  #checking the ensemble tag in settings
  settings <- check.ensemble.settings(settings)

  if (!is.null(settings$database$bety)) {
    dbcon <- PEcAn.DB::db.open(settings$database$bety)
    on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)
  } else {
    dbcon <- NULL
  }

  # make sure there are pfts defined
  if (is.null(settings$pfts) || (length(settings$pfts) == 0)) {
    PEcAn.logger::logger.warn("No PFTS specified.")
  }

  # check to make sure a host is given
  if (is.null(settings$host$name)) {
    PEcAn.logger::logger.info("Setting localhost for execution host.")
    settings$host$name <- "localhost"
  }

  # check if there is either ensemble or sensitivy.analysis
  if (is.null(settings$ensemble) && is.null(settings$sensitivity.analysis)) {
    PEcAn.logger::logger.warn(
      "No ensemble or sensitivity analysis specified.",
      "No models will be executed!")
  }

  settings <- papply(settings, check.run.settings, dbcon = dbcon)

  # check meta-analysis
  if (!is.null(settings$meta.analysis)) {
    if (is.null(settings$meta.analysis$iter)) {
      settings$meta.analysis$iter <- 3000
      PEcAn.logger::logger.info(
        "Setting meta.analysis iterations to ", settings$meta.analysis$iter)
    }
    if (is.null(settings$meta.analysis$random.effects)) {
      settings$meta.analysis$random.effects         <- list()
      settings$meta.analysis$random.effects$on      <- FALSE
      settings$meta.analysis$random.effects$use_ghs <- TRUE
      PEcAn.logger::logger.info(
        "Setting meta.analysis random effects to ",
        settings$meta.analysis$random.effects$on)
    } else if (!is.list(settings$meta.analysis$random.effects)) {
      # this handles the previous usage
      #  <meta.analysis>
      #    <random.effects>FALSE</random.effects>
      #  </meta.analysis>
      re_check <- as.logical(settings$meta.analysis$random.effects)
      settings$meta.analysis$random.effects    <- list()
      settings$meta.analysis$random.effects$on <- re_check
      settings$meta.analysis$random.effects$use_ghs <- TRUE
    } else {
      # everything is used as defined
      settings$meta.analysis$random.effects$on <- as.logical(
        settings$meta.analysis$random.effects$on)
      if (!is.null(settings$meta.analysis$random.effects$use_ghs)) {
        settings$meta.analysis$random.effects$use_ghs <- as.logical(
          settings$meta.analysis$random.effects$use_ghs)
      } else {
        settings$meta.analysis$random.effects$use_ghs <- TRUE
      }
    }
    if (is.null(settings$meta.analysis$threshold)) {
      settings$meta.analysis$threshold <- 1.2
      PEcAn.logger::logger.info(
        "Setting meta.analysis threshold to ", settings$meta.analysis$threshold)
    }
    if (is.null(settings$meta.analysis$update)) {
      settings$meta.analysis$update <- "AUTO"
      PEcAn.logger::logger.info(
        "Setting meta.analysis update to only update if no previous",
        "meta analysis was found")
    }
    if ((settings$meta.analysis$update != "AUTO")
        && is.na(as.logical(settings$meta.analysis$update))) {
      PEcAn.logger::logger.info(
        "meta.analysis update can only be AUTO/TRUE/FALSE, defaulting to FALSE")
      settings$meta.analysis$update <- FALSE
    }
  }

  settings <- check.model.settings(settings, dbcon)

  ## if run$host is localhost, set to "localhost
  if (any(
      settings$host %in% c(
        Sys.info()["nodename"],
        gsub("illinois", "uiuc", Sys.info()["nodename"])))) {
    settings$host$name <- "localhost"
  }

  # check if we need to use qsub
  if ("qsub" %in% names(settings$host)) {
    if (is.null(settings$host$qsub)) {
      settings$host$qsub <- "qsub -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
      PEcAn.logger::logger.info(
        "qsub not specified using default value :", settings$host$qsub)
    }
    if (is.null(settings$host$qsub.jobid)) {
      settings$host$qsub.jobid <- "Your job ([0-9]+) .*"
      PEcAn.logger::logger.info(
        "qsub.jobid not specified using default value :",
        settings$host$qsub.jobid)
    }
    if (is.null(settings$host$qstat)) {
      settings$host$qstat <- "qstat -j @JOBID@ &> /dev/null || echo DONE"
      PEcAn.logger::logger.info(
        "qstat not specified using default value :", settings$host$qstat)
    }
  }

  # modellauncher to launch on multiple nodes/cores
  if ("modellauncher" %in% names(settings$host)) {
    if (is.null(settings$host$modellauncher$binary)) {
      settings$host$modellauncher$binary <- "modellauncher"
      PEcAn.logger::logger.info(
        "binary not specified using default value :",
        settings$host$modellauncher$binary)
    }
    if (is.null(settings$host$modellauncher$qsub.extra)) {
      PEcAn.logger::logger.severe(
        "qsub.extra not specified, can not launch in parallel environment.")
    }
    if (is.null(settings$host$modellauncher$mpirun)) {
      settings$host$modellauncher$mpirun <- "mpirun"
      PEcAn.logger::logger.info(
        "mpirun not specified using default value :",
        settings$host$modellauncher$mpirun)
    }
  }

  # some warnings for deprecated job.sh
  if ("job.sh" %in% names(settings$model)) {
    if ("prerun" %in% names(settings$model)) {
      PEcAn.logger::logger.severe(
        "You have both settings$model$job.sh and settings$model$prerun,",
        "please combine.")
    }
    PEcAn.logger::logger.info(
      "settings$model$job.sh is deprecated use settings$model$prerun instead.")
    settings$model$prerun <- settings$model$job.sh
    settings$model$job.sh <- NULL
  }
  if ("job.sh" %in% names(settings$host)) {
    if ("prerun" %in% names(settings$host)) {
      PEcAn.logger::logger.severe(
        "You have both settings$host$job.sh and settings$host$prerun,",
        "please combine.")
    }
    PEcAn.logger::logger.info(
      "settings$host$job.sh is deprecated use settings$host$prerun instead.")
    settings$host$prerun <- settings$host$job.sh
    settings$host$job.sh <- NULL
  }

  # Check folder where outputs are written before adding to dbfiles
  if (is.null(settings$database$dbfiles)) {
    settings$database$dbfiles <- PEcAn.utils::full.path("~/.pecan/dbfiles")
  } else {
    if (substr(settings$database$dbfiles, 1, 1) != "/") {
      PEcAn.logger::logger.warn(
        "settings$database$dbfiles pathname", settings$database$dbfiles,
        "is invalid\n",
        "placing it in the home directory ",
        Sys.getenv("HOME"))
      settings$database$dbfiles <- file.path(
        Sys.getenv("HOME"),
        settings$database$dbfiles)
    }

    settings$database$dbfiles <- normalizePath(
      settings$database$dbfiles,
      mustWork = FALSE)
  }
  dir.create(settings$database$dbfiles, showWarnings = FALSE, recursive = TRUE)

  # check all inputs exist
  settings <- papply(settings, check.inputs)

  settings <- check.workflow.settings(settings, dbcon)

  # check/create the local run folder
  if (is.null(settings$rundir)) {
    settings$rundir <- file.path(settings$outdir, "run")
  }
  if (!file.exists(settings$rundir)
      && !dir.create(settings$rundir, recursive = TRUE)) {
    PEcAn.logger::logger.severe("Could not create run folder", settings$rundir)
  }

  # check/create the local model out folder
  if (is.null(settings$modeloutdir)) {
    settings$modeloutdir <- file.path(settings$outdir, "out")
  }
  if (!file.exists(settings$modeloutdir)
      && !dir.create(settings$modeloutdir, recursive = TRUE)) {
    PEcAn.logger::logger.severe(
      "Could not create model out folder", settings$modeloutdir)
  }

  # make sure remote folders are specified if need be
  if (!PEcAn.remote::is.localhost(settings$host)) {
    if (is.null(settings$host$folder)) {
      settings$host$folder <- paste0(
        PEcAn.remote::remote.execute.cmd("pwd", host = settings$host),
        "/pecan_remote")
      PEcAn.logger::logger.info(
        "Using ", settings$host$folder, "to store output on remote machine")
    }
    if (is.null(settings$host$rundir)) {
      settings$host$rundir <- paste0(settings$host$folder, "/@WORKFLOW@/run")
    }
    settings$host$rundir <- gsub(
      "@WORKFLOW@",
      settings$workflow$id,
      settings$host$rundir)
    PEcAn.logger::logger.info(
      "Using ", settings$host$rundir, "to store runs on remote machine")
    if (is.null(settings$host$outdir)) {
      settings$host$outdir <- paste0(settings$host$folder, "/@WORKFLOW@/out")
    }
    settings$host$outdir <- gsub(
      "@WORKFLOW@",
      settings$workflow$id,
      settings$host$outdir)
    PEcAn.logger::logger.info(
      "Using ", settings$host$outdir, "to store output on remote machine")
  } else if (settings$host$name == "localhost") {
    settings$host$rundir <- settings$rundir
    settings$host$outdir <- settings$modeloutdir
  }

  # check/create the pft folders
  if (!is.null(settings$pfts) && (length(settings$pfts) > 0)) {
    for (i in seq_along(settings$pfts)) {
      #check if name tag within pft
      if (!"name" %in% names(settings$pfts[i]$pft)) {
        PEcAn.logger::logger.severe(
          "No name specified for pft of index: ", i, ", please specify name")
      }
      if (settings$pfts[i]$pft$name == "") {
        PEcAn.logger::logger.severe(
          "Name specified for pft of index: ", i, " can not be empty.")
      }

      # check to see if name of each pft in xml file is actually
      # a name of a pft already in database
      if (!is.null(dbcon)) { # change to if(inherits(dbcon, "PostgreSQLConnection")) ??
        if (is.null(settings$model$type)) {
          x <- PEcAn.DB::db.query(
            paste0(
              "SELECT pfts.id FROM pfts",
              " WHERE pfts.name = '",  settings$pfts[i]$pft$name, "'"),
            con = dbcon)
        } else {
          x <- PEcAn.DB::db.query(
            paste0(
              "SELECT pfts.id FROM pfts, modeltypes",
              " WHERE pfts.name = '",  settings$pfts[i]$pft$name, "'",
              " AND modeltypes.name='", settings$model$type, "'",
              " AND modeltypes.id=pfts.modeltype_id;"),
            con = dbcon)
        }
        if (nrow(x) == 0) {
          PEcAn.logger::logger.severe(
            "Did not find a pft with name ", settings$pfts[i]$pft$name,
            "\nfor model type", settings$model$type)
        }
        if (nrow(x) > 1) {
          PEcAn.logger::logger.warn(
            "Found multiple entries for pft with name ",
            settings$pfts[i]$pft$name,
            "\nfor model type", settings$model$type)
        }
      }

      if (is.null(settings$pfts[i]$pft$outdir)) {
        settings$pfts[i]$pft$outdir <- file.path(
          settings$outdir,
          "pft",
          settings$pfts[i]$pft$name)
        PEcAn.logger::logger.info(
          "Storing pft", settings$pfts[i]$pft$name,
          "in", settings$pfts[i]$pft$outdir)
      } else {
        PEcAn.logger::logger.debug(
          "Storing pft", settings$pfts[i]$pft$name,
          "in", settings$pfts[i]$pft$outdir)
      }
      out.dir <- settings$pfts[i]$pft$outdir
      if (!file.exists(out.dir) && !dir.create(out.dir, recursive = TRUE)) {
        if (identical(dir(out.dir), character(0))) {
          PEcAn.logger::logger.warn(out.dir, "exists but is empty")
        } else {
          PEcAn.logger::logger.severe("Could not create folder", out.dir)
        }
      }
    }
  }

  # Set 'checked' flag so check.settings will be skipped in the future
  # (unless force=TRUE)
  settings$settings.info$checked <- TRUE

  # all done return cleaned up settings
  return(invisible(settings))
}

#' @title Check Run Settings
#' @param settings settings file
#' @export check.run.settings
check.run.settings <- function(settings, dbcon = NULL) {
  scipen <- getOption("scipen")
  on.exit(options(scipen = scipen), add = TRUE)
  options(scipen = 12)

  # check for a run settings
  if (is.null(settings[["run"]])) {
    PEcAn.logger::logger.warn("No Run Settings specified")
  }


  # check start/end date are specified and correct
  if (is.null(settings$run$start.date)) {
    PEcAn.logger::logger.warn("No start.date specified in run section.")
  } else if (is.null(settings$run$end.date)) {
    PEcAn.logger::logger.warn("No end.date specified in run section.")
  } else {
    startdate <- lubridate::parse_date_time(
      settings$run$start.date,
      "ymd_HMS",
      truncated = 3)
    enddate <- lubridate::parse_date_time(
      settings$run$end.date,
      "ymd_HMS",
      truncated = 3)
    if (startdate >= enddate) {
      PEcAn.logger::logger.severe("Start date should come before the end date.")
    }
  }


  # check sensitivity analysis
  if (!is.null(settings$sensitivity.analysis)) {
    if (is.null(settings$sensitivity.analysis$variable)) {
      if (is.null(settings$ensemble$variable)) {
        PEcAn.logger::logger.severe(
          "No variable specified to compute sensitivity.analysis for.")
      }
      PEcAn.logger::logger.info(
        "Setting sensitivity.analysis variable to the same as",
         "ensemble variable [", settings$ensemble$variable, "]")
      settings$sensitivity.analysis$variable <- settings$ensemble$variable
    }

    if (is.null(settings$sensitivity.analysis$start.year)) {
      if (!is.null(settings$run$start.date)) {
        settings$sensitivity.analysis$start.year <- lubridate::year(
          settings$run$start.date)
        PEcAn.logger::logger.info(
          "No start date passed to sensitivity.analysis - using the run date (",
          settings$sensitivity.analysis$start.year, ").")
      } else if (!is.null(settings$ensemble$start.year)) {
        settings$sensitivity.analysis$start.year <- settings$ensemble$start.year
        PEcAn.logger::logger.info(
          "No start date passed to sensitivity.analysis -",
          "using the ensemble date (",
          settings$sensitivity.analysis$start.year, ").")
      } else {
        PEcAn.logger::logger.info(
          "No start date passed to sensitivity.analysis,",
          "and no default available.")
      }
    }

    if (is.null(settings$sensitivity.analysis$end.year)) {
      if (!is.null(settings$run$end.date)) {
        settings$sensitivity.analysis$end.year <- lubridate::year(
          settings$run$end.date)
        PEcAn.logger::logger.info(
          "No end date passed to sensitivity.analysis - using the run date (",
          settings$sensitivity.analysis$end.year, ").")
      } else if (!is.null(settings$ensemble$end.year)) {
        settings$sensitivity.analysis$end.year <- settings$ensemble$end.year
        PEcAn.logger::logger.info(
          "No end date passed to sensitivity.analysis.",
          "Using the ensemble date (",
          settings$sensitivity.analysis$end.year, ").")
      } else {
        PEcAn.logger::logger.info(
          "No end date passed to sensitivity.analysis,",
          "and no default available.")
      }
    }


    # check start and end dates
    if (exists("startdate")
        && !is.null(settings$sensitivity.analysis$start.year)
        && lubridate::year(startdate) > settings$sensitivity.analysis$start.year) {
      PEcAn.logger::logger.severe(
        "Start year of SA should come after the start.date of the run")
    }
    if (exists("enddate")
        && !is.null(settings$sensitivity.analysis$end.year)
        && lubridate::year(enddate) < settings$sensitivity.analysis$end.year) {
      PEcAn.logger::logger.severe(
        "End year of SA should come before the end.date of the run")
    }
    if (!is.null(settings$sensitivity.analysis$start.year) &&
        !is.null(settings$sensitivity.analysis$end.year) &&
        settings$sensitivity.analysis$start.year > settings$sensitivity.analysis$end.year) {
      PEcAn.logger::logger.severe(
        "Start year of SA should come before the end year of the SA")
    }
  }


  # check siteid with values
  if (!is.null(settings$run$site)) {
    if (is.null(settings$run$site$id)) {
      settings$run$site$id <- -1
    } else if (settings$run$site$id >= 0) {
      if (!is.null(dbcon)) {
        site <- PEcAn.DB::db.query(
          paste(
            "SELECT sitename, ST_X(ST_CENTROID(geometry)) AS lon, ",
            "ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",
            settings$run$site$id),
          con = dbcon)
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
      if ((!is.null(settings$run$site$met))
         && settings$run$site$met == "NULL") {
        settings$run$site$met <- NULL
      }
      if (is.null(settings$run$site$name)) {
        if ((is.null(site$sitename) || site$sitename == "")) {
          PEcAn.logger::logger.info("No site name specified.")
          settings$run$site$name <- "NA"
        } else {
          settings$run$site$name <- site$sitename
          PEcAn.logger::logger.info(
            "Setting site name to ", settings$run$site$name)
        }
      } else if (site$sitename != settings$run$site$name) {
        PEcAn.logger::logger.warn(
          "Specified site name [", settings$run$site$name,
          "] does not match sitename in database [", site$sitename, "]")
      }

      if (is.null(settings$run$site$lat)) {
        if ((is.null(site$lat) || site$lat == "")) {
          PEcAn.logger::logger.severe("No lat specified for site.")
        } else {
          settings$run$site$lat <- as.numeric(site$lat)
          PEcAn.logger::logger.info(
            "Setting site lat to ", settings$run$site$lat)
        }
      } else if (as.numeric(site$lat) != as.numeric(settings$run$site$lat)) {
        PEcAn.logger::logger.warn(
          "Specified site lat [", settings$run$site$lat,
          "] does not match lat in database [", site$lat, "]")
      }

      if (is.null(settings$run$site$lon)) {
        if ((is.null(site$lon) || site$lon == "")) {
          PEcAn.logger::logger.severe("No lon specified for site.")
        } else {
          settings$run$site$lon <- as.numeric(site$lon)
          PEcAn.logger::logger.info(
            "Setting site lon to ", settings$run$site$lon)
        }
      } else if (as.numeric(site$lon) != as.numeric(settings$run$site$lon)) {
        PEcAn.logger::logger.warn(
          "Specified site lon [", settings$run$site$lon,
          "] does not match lon in database [", site$lon, "]")
      }
    }
  } else {
    settings$run$site$id <- -1
  }
  # end site check code

  # all done return cleaned up settings
  invisible(settings)
}

#' @title Check Model Settings
#' @param settings settings file
#' @export check.model.settings
check.model.settings <- function(settings, dbcon = NULL) {
  # check modelid with values
  if (!is.null(settings$model)) {
    if (!is.null(dbcon)) {
      if (!is.null(settings$model$id)) {
        if (as.numeric(settings$model$id) >= 0) {
          model <- PEcAn.DB::db.query(
            paste0(
              "SELECT models.id AS id, models.revision AS revision, ",
                "modeltypes.name AS type",
              " FROM models, modeltypes WHERE models.id=",
              settings$model$id,
              " AND models.modeltype_id=modeltypes.id;"),
            con = dbcon)
          if (nrow(model) == 0) {
            PEcAn.logger::logger.error(
              "There is no record of model_id = ", settings$model$id,
              "in database")
          }
        } else {
          model <- list()
        }
      } else if (!is.null(settings$model$type)) {
        model <- PEcAn.DB::db.query(
          paste0(
            "SELECT models.id AS id, models.revision AS revision, ",
            "modeltypes.name AS type FROM models, modeltypes ",
            "WHERE modeltypes.name = '", toupper(settings$model$type), "' ",
            "AND models.modeltype_id=modeltypes.id ",
            ifelse(
              is.null(settings$model$revision), "",
              paste0("AND revision like '%", settings$model$revision, "%' ")),
            "ORDER BY models.updated_at"),
          con = dbcon)
        if (nrow(model) > 1) {
          PEcAn.logger::logger.warn(
            "multiple records for", settings$model$name,
            "returned; using the latest")
          row <- which.max(model$updated_at)
          if (length(row) == 0) row <- nrow(model)
          model <- model[row, ]
        } else if (nrow(model) == 0) {
          PEcAn.logger::logger.warn(
            "Model type", settings$model$type, "not in database")
        }
      } else {
        PEcAn.logger::logger.warn("no model settings given")
        model <- list()
      }
    } else {
      model <- list()
    }

    # copy data from database into missing fields
    if (!is.null(model$id)) {
      if (is.null(settings$model$id) || (settings$model$id == "")) {
        settings$model$id <- model$id
        PEcAn.logger::logger.info("Setting model id to ", settings$model$id)
      } else if (settings$model$id != model$id) {
        PEcAn.logger::logger.warn(
          "Model id specified in settings file does not match database.")
      }
    } else {
      if (is.null(settings$model$id) || (settings$model$id == "")) {
        settings$model$id <- -1
        PEcAn.logger::logger.info("Setting model id to ", settings$model$id)
      }
    }
    if (!is.null(model$type)) {
      if (is.null(settings$model$type) || (settings$model$type == "")) {
        settings$model$type <- model$type
        PEcAn.logger::logger.info("Setting model type to ", settings$model$type)
      } else if (settings$model$type != model$type) {
        PEcAn.logger::logger.warn(
          "Model type specified in settings file does not match database.")
      }
    }
    if (!is.null(model$revision)) {
      if (is.null(settings$model$revision) || (settings$model$revision == "")) {
        settings$model$revision <- model$revision
        PEcAn.logger::logger.info(
          "Setting model revision to ", settings$model$revision)
      } else if (settings$model$revision != model$revision) {
        PEcAn.logger::logger.warn(
          "Model revision specified in settings file does not match database.")
      }
    }

    # make sure we have model type
    if ((is.null(settings$model$type) || settings$model$type == "")) {
      PEcAn.logger::logger.severe("Need a model type.")
    }

    # Set model$delete.raw to FALSE by default
    if (is.null(settings$model$delete.raw)
        || !is.logical(as.logical(settings$model$delete.raw))) {
      PEcAn.logger::logger.info(
        "Option to delete raw model output not set or not logical.",
        "Will keep all model output.")
      settings$model$delete.raw <- FALSE
    }

    # check on binary for given host
    if (!is.null(settings$model$id) && (settings$model$id >= 0)) {
      binary <- PEcAn.DB::dbfile.file(
        "Model",
        settings$model$id,
        dbcon,
        settings$host$name)
      if (!is.na(binary)) {
        if (is.null(settings$model$binary)) {
          settings$model$binary <- binary
          PEcAn.logger::logger.info(
            "Setting model binary to ", settings$model$binary)
        } else if (binary != settings$model$binary) {
          PEcAn.logger::logger.warn(
            "Specified binary [", settings$model$binary,
            "] does not match path in database [", binary, "]")
        }
      }
    } else {
      PEcAn.logger::logger.warn(
        "No model binary sepcified in database for model ", settings$model$type)
    }
  }

  return(settings)
}

#' @title Check Workflow Settings
#' @param settings settings file
#' @export check.workflow.settings
check.workflow.settings <- function(settings, dbcon = NULL) {
  # check for workflow defaults
  fixoutdir <- FALSE
  if (!is.null(dbcon)
      && settings$database$bety$write
      && ("model" %in% names(settings))) {
    if (!"workflow" %in% names(settings)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if (is.MultiSettings(settings)) {
        insert_result <- PEcAn.DB::db.query(
          paste0(
            "INSERT INTO workflows (",
              "folder, model_id, hostname, started_at) ",
            "values ('",
              settings$outdir, "','",
              settings$model$id, "', '",
              settings$host$name, "', '",
              now, "') RETURNING id"),
          con = dbcon)
      } else {
        insert_result <- PEcAn.DB::db.query(
          paste0(
            "INSERT INTO workflows (",
              "folder, site_id, model_id, hostname, start_date, end_date, ",
              "started_at) ",
            "values ('",
              settings$outdir, "','",
              settings$run$site$id, "','",
              settings$model$id, "', '",
              settings$host$name, "', '",
              settings$run$start.date, "', '",
              settings$run$end.date, "', '",
              now, "') RETURNING id"),
          con = dbcon)
      }
      settings$workflow$id <- insert_result[["id"]]
      fixoutdir <- TRUE
    }
  } else {
    settings$workflow$id  <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  }

  # check/create the pecan folder
  if (is.null(settings$outdir)) {
    settings$outdir <- "PEcAn_@WORKFLOW@"
  }
  # replace @WORKFLOW@ with the id of the workflow
  settings$outdir <- gsub(
    "@WORKFLOW@",
    format(settings$workflow$id, scientific = FALSE),
    settings$outdir)
  # create fully qualified pathname
  if (substr(settings$outdir, 1, 1) != "/") {
    settings$outdir <- file.path(getwd(), settings$outdir)
  }
  PEcAn.logger::logger.info("output folder =", settings$outdir)
  if (!file.exists(settings$outdir)
      && !dir.create(settings$outdir, recursive = TRUE)) {
    PEcAn.logger::logger.severe("Could not create folder", settings$outdir)
  }

  #update workflow
  if (fixoutdir) {
    PEcAn.DB::db.query(
      paste0(
        "UPDATE workflows SET folder='",
        PEcAn.utils::full.path(settings$outdir),
        "' WHERE id=", settings$workflow$id),
      con = dbcon)
  }

  return(settings)
}


#' @title Check Database Settings
#' @param settings settings file
#' @export check.database.settings
check.database.settings <- function(settings) {
  if (!is.null(settings$database)) {
    # check all databases
    for (name in names(settings$database)) {
      if (name != "dbfiles") {
        # 'dbfiles' is kept in <database>, but isn't actually a db settings block
        settings$database[[name]] <- check.database(settings$database[[name]])
      }
    }

    # check bety database
    if (!is.null(settings$database$bety)) {
      # should runs be written to database
      if (is.null(settings$database$bety$write)) {
        PEcAn.logger::logger.info(
          "Writing all runs/configurations to database.")
        settings$database$bety$write <- TRUE
      } else {
        settings$database$bety$write <- as.logical(settings$database$bety$write)
        if (settings$database$bety$write) {
          PEcAn.logger::logger.debug(
            "Writing all runs/configurations to database.")
        } else {
          PEcAn.logger::logger.warn(
            "Will not write runs/configurations to database.")
        }
      }

      # check if we can connect to the database with write permissions
      if (settings$database$bety$write
          && !PEcAn.DB::db.exists(
                params = settings$database$bety,
                TRUE,
                table = "users")) {
        PEcAn.logger::logger.severe(
          "Invalid Database Settings : ", unlist(settings$database))
      }

      # TODO check userid and userpassword

      # Connect to database
      dbcon <- PEcAn.DB::db.open(settings$database$bety)
      on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)

      # check database version
      check.bety.version(dbcon)
    } else {
      PEcAn.logger::logger.warn(
        "No BETY database information specified; not using database.")
    }
  } else {
    PEcAn.logger::logger.warn(
      "No BETY database information specified; not using database.")
  }
  return(settings)
}

#' @title Check ensemble Settings
#' @param settings settings file
#' @export check.ensemble.settings
check.ensemble.settings <- function(settings) {
  # check ensemble
  if (!is.null(settings$ensemble)) {
    if (is.null(settings$ensemble$variable)) {
      if (is.null(settings$sensitivity.analysis$variable)) {
        PEcAn.logger::logger.severe(
          "No variable specified to compute ensemble for.")
      }
      PEcAn.logger::logger.info(
        "Setting ensemble variable to the same as sensitivity analysis",
        "variable [", settings$sensitivity.analysis$variable, "]")
      settings$ensemble$variable <- settings$sensitivity.analysis$variable
    }

    if (is.null(settings$ensemble$size)) {
      PEcAn.logger::logger.info("Setting ensemble size to 1.")
      settings$ensemble$size <- 1
    }

    if (is.null(settings$ensemble$start.year)) {
      if (!is.null(settings$run$start.date)) {
        settings$ensemble$start.year <- lubridate::year(
          settings$run$start.date)
        PEcAn.logger::logger.info(
          "No start date passed to ensemble - using the run date (",
          settings$ensemble$start.year, ").")
      } else if (!is.null(settings$sensitivity.analysis$start.year)) {
        settings$ensemble$start.year <- settings$sensitivity.analysis$start.year
        PEcAn.logger::logger.info(
          "No start date passed to ensemble.",
          "Using the sensitivity.analysis date (",
          settings$ensemble$start.year, ").")
      } else {
        PEcAn.logger::logger.info(
          "No start date passed to ensemble, and no default available.")
      }
    }

    if (is.null(settings$ensemble$end.year)) {
      if (!is.null(settings$run$end.date)) {
        settings$ensemble$end.year <- lubridate::year(settings$run$end.date)
        PEcAn.logger::logger.info(
          "No end date passed to ensemble - using the run date (",
          settings$ensemble$end.year, ").")
      } else if (!is.null(settings$sensitivity.analysis$end.year)) {
        settings$ensemble$end.year <- settings$sensitivity.analysis$end.year
        PEcAn.logger::logger.info(
          "No end date passed to ensemble.",
          "Using the sensitivity.analysis date (",
          settings$ensemble$end.year, ").")
      } else {
        PEcAn.logger::logger.info(
          "No end date passed to ensemble, and no default available.")
      }
    }

    # check start and end dates
    if (exists("startdate") && !is.null(settings$ensemble$start.year) &&
        lubridate::year(startdate) > settings$ensemble$start.year) {
      PEcAn.logger::logger.severe(
        "Start year of ensemble should come after the start.date of the run")
    }
    if (exists("enddate") && !is.null(settings$ensemble$end.year) &&
        lubridate::year(enddate) < settings$ensemble$end.year) {
      PEcAn.logger::logger.severe(
        "End year of ensemble should come before the end.date of the run")
    }
    if (!is.null(settings$ensemble$start.year)
        && !is.null(settings$ensemble$end.year)
        && settings$ensemble$start.year > settings$ensemble$end.year) {
      PEcAn.logger::logger.severe(
       "Start year of ensemble should come before the end year of the ensemble")
    }
  }
  # Old version of pecan xml files which they don't have a sampling space
  # or it's just sampling space and nothing inside it.
  if (is.null(settings$ensemble$samplingspace)
      || !is.list(settings$ensemble$samplingspace)) {
    PEcAn.logger::logger.info(
      "We are updating the ensemble tag inside the xml file.")
    # I try to put ensemble method in older versions into the parameter space -
    # If I fail (when no method is defined) I just set it as uniform
    settings$ensemble$samplingspace$parameters$method <- settings$ensemble$method
    if (is.null(settings$ensemble$samplingspace$parameters$method)) {
      settings$ensemble$samplingspace$parameters$method <- "uniform"
    }
    #putting something simple in the met
    settings$ensemble$samplingspace$met$method <- "sampling"
  }
  return(settings)
}
