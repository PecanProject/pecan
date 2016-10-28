.check.model.settings <- function(settings, dbcon = NULL) {
  # check modelid with values
  if(!is.null(settings$model)) {
    if(!is.null(dbcon)) {
      if(!is.null(settings$model$id)) {
        if(as.numeric(settings$model$id) >= 0) {
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
        if(nrow(model) > 1) {
          logger.warn("multiple records for", settings$model$name, "returned; using the latest")
          row <- which.max(model$updated_at)
          if (length(row) == 0) {
            row <- nrow(model)
          }
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
      settings$model$delete.raw <- FALSE
    }
    
    # check on binary for given host
    if (!is.null(settings$model$id) && (settings$model$id >= 0)) {
      binary <- dbfile.file("Model", settings$model$id, dbcon, settings$host$name)
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
  
  return(settings)
}
