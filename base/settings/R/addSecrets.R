##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
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
##' @export addSecrets
addSecrets <- function(settings, force=FALSE) {
  if (!file.exists("~/.pecan.xml")) {
    return(invisible(settings))
  }
  
  if(!force && !is.null(settings$settings.info$secrets.added) && 
     settings$settings.info$secrets.added==TRUE) {
    PEcAn.logger::logger.info("Secret settings have been added already. Skipping.")
    return(invisible(settings))
  } else {
    PEcAn.logger::logger.info("Adding secret settings...")
  }
  
  if(is.MultiSettings(settings)) {
    return(invisible(papply(settings, addSecrets, force=force)))
  }
  
  pecan <- xmlToList(xmlParse("~/.pecan.xml"))
  
  # always copy following sections
  for(key in c('database')) {
    for(section in names(pecan[[key]])) {
      if (section %in% names(settings[section])) {
        PEcAn.logger::logger.info("Already have a section for", section)
      } else {
        PEcAn.logger::logger.info("Imported section for", section)
        settings[[key]][section] <- pecan[[key]][section]
      }
    }
  }
  
  # only copy these sections if tag exists
  for(key in c('browndog')) {
    if (! key %in% names(settings)) next
    
    for(section in names(pecan[[key]])) {
      if (section %in% names(settings[section])) {
        PEcAn.logger::logger.info("Already have a section for", section)
      } else {
        PEcAn.logger::logger.info("Imported section for", section)
        settings[[key]][section] <- pecan[[key]][section]
      }
    }
  }  
  
  return(invisible(settings))
}
