##' Takes in a settings object, performs a series of checks, fixes & updates settings and produces pecan.CHECKED.xml
##'
##' @title Write settings
##' @param settings settings list
##' @param outputfile the file name to write to
##' @param outputdir the directory to write to
##' @author Ryan Kelly
##' @author Betsy Cowdery
##' @export write.settings


write.settings <- function(settings, outputfile, outputdir=settings$outdir){
  pecanfile <- file.path(outputdir, outputfile)
  if (file.exists(pecanfile)) {
    PEcAn.logger::logger.warn(paste("File already exists [", pecanfile, "] file will be overwritten"))
  }
  saveXML(listToXml(settings, "pecan"), file=pecanfile)
}
