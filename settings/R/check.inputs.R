# check to see if inputs are specified
# this should be part of the model code

#' @import PEcAn.DB
#' @import PEcAn.utils
.check.inputs <- function(settings) {
  if (is.null(settings$model$type)) {
    return(settings)
  }
  
  # don't know how to check inputs
  if (is.null(settings$database$bety)) {
    logger.info("No databasse connection, can't check inputs.")
    return(settings)
  }
  
  # get list of inputs associated with model type
  dbcon <- db.open(settings$database$bety)
  on.exit(db.close(dbcon))
  
  inputs <- db.query(paste0("SELECT tag, format_id, required FROM modeltypes, modeltypes_formats WHERE modeltypes_formats.modeltype_id = modeltypes.id and modeltypes.name='", settings$model$type, "' AND modeltypes_formats.input;"), con=dbcon)
  
  # check list of inputs  
  allinputs <- names(settings$run$inputs)
  for(i in seq_len(nrow(inputs))) {
    tag <- inputs$tag[i]
    hostname <- settings$host$name
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
  
  if (length(allinputs) > 0) {
    logger.info("Unused inputs found :", paste(allinputs, collapse=" "))
  }
  
  return(settings)
} 