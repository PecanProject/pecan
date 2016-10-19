##' Takes in a settings object, performs a series of checks, fixes & updates settings and produces pecan.CHECKED.xml
##'
##' @title Fix Deprecated Settings
##' @param settings settings list
##' @return updated settings list
##' @author Ryan Kelly


write.settings <- function(settings, outputfile = "pecan.CHECKED.xml"){
  library(XML)
  library(PEcAn.DB)
  library(PEcAn.utils)
  
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
  invisible(settings)
}
