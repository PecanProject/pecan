#' Updates a pecan.xml file to match new layout. This will take care of the
#' conversion to the latest pecan.xml file.
#'
#' @title Update Settings
#' @name update.settings
#' @param settings settings file
#' @param force Logical: update even if settings have previously been updated?.
#' @return will return the updated settings values
#' @export update.settings
#' @author Rob Kooper

update.settings <- function(settings, force = FALSE) {
  if (!force && !is.null(settings$settings.info$settings.updated)
      && settings$settings.info$settings.updated == TRUE) {
    PEcAn.logger::logger.info(
      "Deprecated settings have been fixed already. Skipping.")
    return(invisible(settings))
  } else {
    PEcAn.logger::logger.info("Fixing deprecated settings...")
  }

  if (is.MultiSettings(settings)) {
    return(invisible(papply(settings, update.settings, force = force)))
  }

  # update database section, now have different database definitions
  # under database section, e.g. fia and bety
  if (!is.null(settings$database)) {
    # simple check to make sure the database tag is updated
    if (!is.null(settings$database$dbname)) {
      if (!is.null(settings$database$bety)) {
        PEcAn.logger::logger.severe(
          "Please remove dbname etc from database configuration.")
      }

      PEcAn.logger::logger.info(
        "Database tag has changed, please use <database><bety> to store",
        "information about accessing the BETY database. See also",
        "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")

      bety <- list()
      for (name in names(settings$database)) {
        bety[[name]] <- settings$database[[name]]
      }
      settings$database <- list(bety = bety)
    }

    # warn user about change and update settings
    if (!is.null(settings$bety$write)) {
      PEcAn.logger::logger.warn(
        "<bety><write> is now part of the database settings. For more",
        "information about the database settings see",
        "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
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
        PEcAn.logger::logger.severe(
          "Please remove model_type from model configuration.")
      } else {
        PEcAn.logger::logger.info(
          "Please remove model_type from model configuration.")
      }
    }

    PEcAn.logger::logger.info(
      "Model tag has changed, please use <model><type> to specify",
      "type of model. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$model$type <- settings$model$model_type
    settings$model$model_type <- NULL
  }
  if (!is.null(settings$model$name)) {
    if (!is.null(settings$model$type)) {
      if (settings$model$name != settings$model$type) {
        PEcAn.logger::logger.severe(
          "Please remove name from model configuration.")
      } else {
        PEcAn.logger::logger.info(
          "Please remove name from model configuration.")
      }
    }

    PEcAn.logger::logger.info(
      "Model tag has changed, please use <model><type> to specify",
      "type of model. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$model$type <- settings$model$name
    settings$model$name <- NULL
  }

  # run$site$met is now run$inputs$met$path
  if (!is.null(settings$run$site$met)) {
    if (!is.null(settings$run$inputs$met)) {
      if (settings$run$site$met != settings$run$inputs$met) {
        PEcAn.logger::logger.severe(
          "Please remove met from model configuration.")
      } else {
        PEcAn.logger::logger.info(
          "Please remove met from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    PEcAn.logger::logger.info(
      "Model tag has changed, please use <inputs><met> to specify",
      "met file for a run. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$run$inputs$met$path <- settings$run$site$met
    settings$run$site$met <- NULL
  }

  # inputs now have path and id under tag
  for (tag in names(settings$run$inputs)) {
    if (grepl(".id$", tag)) {
      tagid <- tag
      tag <- substr(tagid, 1, nchar(tagid) - 3)
      if (tag %in% names(settings$run$inputs)) {
        next
      } else {
        settings$run$inputs[[tag]]["id"] <- settings$run$inputs[[tagid]]
        settings$run$inputs[[tagid]] <- NULL
      }
    } else {
      if (!is.list(settings$run$inputs[[tag]])) {
        path <- settings$run$inputs[[tag]]
        settings$run$inputs[[tag]] <- list("path" = path)
      }

      tagid <- paste0(tag, ".id")
      if (tagid %in% names(settings$run$inputs)) {
        if ("id" %in% names(settings$run$inputs[[tag]])) {
          if (settings$run$inputs[[tagid]]
              != settings$run$inputs[[tag]][["id"]]) {
            PEcAn.logger::logger.severe(
              "Please remove", tagid, "from inputs configuration.")
          } else {
            PEcAn.logger::logger.info(
              "Please remove", tagid, "from inputs configuration.")
          }
          settings$run$inputs[[tagid]] <- NULL
        } else {
          settings$run$inputs[[tag]][["id"]] <- settings$run$inputs[[tagid]]
          settings$run$inputs[[tagid]] <- NULL
        }
      }
    }
  }

  # some specific ED changes
  if (!is.null(settings$model$veg)) {
    if (!is.null(settings$run$inputs$veg)) {
      if (settings$model$veg != settings$run$inputs$veg) {
        PEcAn.logger::logger.severe(
          "Please remove veg from model configuration.")
      } else {
        PEcAn.logger::logger.info(
          "Please remove veg from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    PEcAn.logger::logger.info(
      "Model tag has changed, please use <inputs><veg> to specify",
      "veg file for a run. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$run$inputs$veg <- settings$model$veg
    settings$model$veg <- NULL
  }
  if (!is.null(settings$model$soil)) {
    if (!is.null(settings$run$inputs$soil)) {
      if (settings$model$soil != settings$run$inputs$soil) {
        PEcAn.logger::logger.severe(
          "Please remove soil from model configuration.")
      } else {
        PEcAn.logger::logger.info(
          "Please remove soil from model configuration.")
      }
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    PEcAn.logger::logger.info(
      "Model tag has changed, please use <inputs><soil> to specify",
      "soil file for a run. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$run$inputs$soil <- settings$model$soil
    settings$model$soil <- NULL
  }
  if (!is.null(settings$model$psscss)) {
    if (!is.null(settings$run$inputs$pss)) {
      PEcAn.logger::logger.info(
        "Please remove psscss from model configuration.")
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    PEcAn.logger::logger.info(
      "Model tag has changed, please use <inputs><pss/css/site> to specify",
      "pss/css/site file for a run. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$run$inputs$pss <- file.path(settings$model$psscss, "foo.pss")
    settings$run$inputs$css <- file.path(settings$model$psscss, "foo.css")
    settings$run$inputs$site <- file.path(settings$model$psscss, "foo.site")
    settings$model$psscss <- NULL
  }
  if (!is.null(settings$model$inputs)) {
    if (!is.null(settings$run$inputs$inputs)) {
      PEcAn.logger::logger.info(
        "Please remove inputs from model configuration.")
    }
    if (is.null(settings$run$inputs)) {
      settings$run$inputs <- list()
    }
    PEcAn.logger::logger.info(
      "Model tag has changed, please use <inputs><lu/thsums> to specify",
      "lu/thsums file for a run. See also",
      "https://pecanproject.github.io/pecan-documentation/develop/pecanXML.html.")
    settings$run$inputs$lu <- file.path(settings$model$inputs, "glu")
    settings$run$inputs$thsums <- settings$model$inputs
    settings$model$soil <- NULL
  }

  # Set 'checked' flag so update.settings will be skipped in the future
  # (unless force=TRUE)
  settings$settings.info$settings.updated <- TRUE

  return(invisible(settings))
}
