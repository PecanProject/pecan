.check.workflow.settings <- function(settings, dbcon = NULL) {
  # check for workflow defaults
  fixoutdir <- FALSE
  if(!is.null(dbcon) && settings$database$bety$write && ("model" %in% names(settings))) {
    if (!'workflow' %in% names(settings)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if(is.MultiSettings(settings)) {
        db.query(paste0("INSERT INTO workflows (folder, model_id, hostname, started_at, created_at) values ('",
                        settings$outdir, "','" , settings$model$id, "', '", settings$host$name, "', '",
                        now, "', '", now, "')"), con=dbcon)
      } else {      
        db.query(paste0("INSERT INTO workflows (folder, site_id, model_id, hostname, start_date, end_date, started_at, created_at) values ('",
                        settings$outdir, "','" , settings$run$site$id, "','", settings$model$id, "', '", settings$host$name, "', '",
                        settings$run$start.date, "', '", settings$run$end.date, "', '", now, "', '", now, "')"), con=dbcon)
      }
      settings$workflow$id <- db.query(paste0("SELECT id FROM workflows WHERE created_at='", now,
                                              "' ORDER BY id DESC LIMIT 1;"), con=dbcon)[['id']]
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
  settings$outdir <- gsub("@WORKFLOW@", format(settings$workflow$id, scientific = FALSE), settings$outdir)
  # create fully qualified pathname
  if (substr(settings$outdir, 1, 1) != '/') {
    settings$outdir <- file.path(getwd(), settings$outdir)
  }
  logger.info("output folder =", settings$outdir)
  if (!file.exists(settings$outdir) && !dir.create(settings$outdir, recursive = TRUE)) {
    logger.severe("Could not create folder", settings$outdir)
  }
  
  #update workflow
  if (fixoutdir) {
    db.query(paste0("UPDATE workflows SET folder='", full.path(settings$outdir), "' WHERE id=", 
                    settings$workflow$id), con = dbcon)
  }
  
  return(settings)
}

