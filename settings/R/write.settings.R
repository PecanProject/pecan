write.settings <- function(settings, outputfile = "pecan.CHECKED.xml"){
  
  settings <- papply(settings, fix.deprecated.settings)
  settings <- papply(settings, addSecrets)
  settings <- papply(settings, update.settings)
  settings <- check.settings(settings)
  
  ## save the checked/fixed pecan.xml
  if (!is.null(outputfile)) {
    pecanfile <- file.path(settings$outdir, outputfile)
    if (file.exists(pecanfile)) {
      logger.warn(paste("File already exists [", pecanfile, "] file will be overwritten"))
    }
    saveXML(listToXml(settings, "pecan"), file=pecanfile)
  }
  
  ## setup Rlib from settings
  if(!is.null(settings$Rlib)){
    .libPaths(settings$Rlib)
  }
  
}
