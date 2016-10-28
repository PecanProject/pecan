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
.add.secret.settings <- function(settings) {
  if (!file.exists("~/.pecan.xml")) {
    return(settings)
  }
  pecan <- XML::xmlToList(XML::xmlParse("~/.pecan.xml"))
  
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
    if (! key %in% names(settings)) {
      next
    }
    
    for(section in names(pecan[[key]])) {
      if (section %in% names(settings[section])) {
        logger.info("Already have a section for", section)
      } else {
        logger.info("Imported section for", section)
        settings[[key]][section] <- pecan[[key]][section]
      }
    }
  }  
  
  return(invisible(settings))
}
