#' Write a settings object as XML
#'
#' If `outputdir` is NULL, the whole path is taken from `outputfile`.
#' If both `outputfile` and `outputdir` are NULL, no file is written
#' and it returns the formatted XML as character.
#' This can be handy for debugging.
#'
#' @param settings settings list
#' @param outputfile the file name to write to
#' @param outputdir the directory to write to
#'
#' @return path to written XML file,
#'  or settings as an XML string if outputfile is NULL
#' @author Ryan Kelly
#' @author Betsy Cowdery
#' @export
write.settings <- function(
  settings,
  outputfile,
  outputdir = settings$outdir) {
  if (!is.null(outputdir)) {
    outputfile <- file.path(outputdir, outputfile)
  }
  if (!is.null(outputfile) && file.exists(outputfile)) {
    PEcAn.logger::logger.warn(
      paste(
        "File already exists [", outputfile,
        "] file will be overwritten"))
  }
  XML::saveXML(listToXml(settings, "pecan"), file = outputfile)
}
