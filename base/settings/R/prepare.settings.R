##' Update, set defaults for, and otherwise prepare a PEcAn Settings object
##'
##'  Performs various checks, fixes deprecated contructs, and assigns missing values where possible.
##'
##' @title Prepare Settings
##' @param settings settings list
##' @param force Whether to force the function to run even if it determines it has been run on 
##'        these settings already. 
##' @author Ryan Kelly
##' @author Betsy Cowdery
##' @export prepare.settings

prepare.settings <- function(settings, force=FALSE) {
  if(is.MultiSettings(settings)) {
    return(invisible(papply(settings, prepare.settings, force=force)))
  }
  
  settings <- fix.deprecated.settings(settings, force=force)
  settings <- addSecrets(settings, force=force)
  settings <- update.settings(settings, force=force)
  settings <- check.settings(settings, force=force)
  settings <- site.pft.link.settings (settings)
  return(settings)
}
