##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
##' Checks for and attempts to fix deprecated settings structure
##'
##' @title Fix Deprecated Settings
##' @param settings settings list
##' @return updated settings list
##' @author Ryan Kelly
##' @export fix.deprecated.settings
fix.deprecated.settings <- function(settings, force=FALSE) {
  if(!force && !is.null(settings$settings.info$deprecated.settings.fixed) && 
     settings$settings.info$deprecated.settings.fixed==TRUE) {
    PEcAn.logger::logger.info("Deprecated settings have been fixed already. Skipping.")
    return(invisible(settings))
  } else {
    PEcAn.logger::logger.info("Fixing deprecated settings...")
  }
  
  if(is.MultiSettings(settings)) {
    return(invisible(papply(settings, fix.deprecated.settings, force=force)))
  }
  
  # settings$model$jobtemplate
  if(!is.null(settings$run$jobtemplate)) {
    if(!is.null(settings$model$jobtemplate)) {
      PEcAn.logger::logger.severe("You have both deprecated settings$run$jobtemplate and settings$model$jobtemplate. Use latter only.")
    }
    PEcAn.logger::logger.info("settings$run$jobtemplate is deprecated. uwe settings$model$jobtemplate instead")
    settings$model$jobtemplate <- settings$run$jobtemplate
    settings$run$jobtemplate <- NULL
  }
  
  # settings$database$dbfiles
  if(!is.null(settings$run$dbfiles)) {
    if(!is.null(settings$database$dbfiles)) {
      PEcAn.logger::logger.severe("You have both deprecated settings$run$dbfiles and settings$database$dbfiles. Use latter only.")
    }
    PEcAn.logger::logger.info("settings$run$dbfiles is deprecated. uwe settings$database$dbfiles instead")
    settings$database$dbfiles <- settings$run$dbfiles
    settings$run$dbfiles <- NULL
  }
  
  # settings$host
  if(!is.null(settings$run$host)) {
    if(!is.null(settings$host)) {
      PEcAn.logger::logger.severe("You have both deprecated settings$run$host and settings$host. Use latter only.")
    }
    PEcAn.logger::logger.info("settings$run$host is deprecated. uwe settings$host instead")
    settings$host <- settings$run$host
    settings$run$host <- NULL
  }
  
  # Set 'checked' flag so fix.deprecated.settings will be skipped in the future (unless force=TRUE)
  settings$settings.info$deprecated.settings.fixed <- TRUE
  
  return(settings)
}
