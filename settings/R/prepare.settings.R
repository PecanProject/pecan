prepare.settings <- function(settings, force=FALSE) {
  if(is.MultiSettings(settings)) {
    return(invisible(papply(settings, prepare.settings, force=force)))
  }
  
  settings <- fix.deprecated.settings(settings, force=force)
  settings <- addSecrets(settings, force=force)
  settings <- update.settings(settings, force=force)
  settings <- check.settings(settings, force=force)  
  return(settings)
}
