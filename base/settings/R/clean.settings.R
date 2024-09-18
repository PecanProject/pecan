#' Cleans PEcAn settings file
#'
#' This will try and clean the settings file so it is ready for
#' a new run. This will remove all run specific information and
#' set the outdir to be 'pecan' for the next run.
#' @param inputfile the PEcAn settings file to be used.
#' @param outputfile the name of file to which the settings will be
#'        written inside the outputdir.
#' @param write Indicates whether to write the modified settings to a file.
#' @return list of all settings as saved to the XML file(s)
#' @export clean.settings
#' @author Rob Kooper
#' @examples
#' \dontrun{
#' clean.settings('output/PEcAn_1/pecan.xml', 'pecan.xml')
#' }
clean.settings <- function(
    inputfile = "pecan.xml",
    outputfile = "pecan.xml",
    write = TRUE) {
  if (is.null(inputfile) || !file.exists(inputfile)) {
    PEcAn.logger::logger.severe("Could not find input file.")
  }
  settings <- XML::xmlToList(XML::xmlParse(inputfile))

  # 1) change outdir
  settings$outdir <- "pecan"

  # 2) remove rundir/modeloutdir
  settings$rundir <- NULL
  settings$modeloutdir <- NULL

  # 3) remove all outdir under pft and remove poteriorid
  for (i in seq_along(settings$pfts)) {
    settings$pfts[i]$pft$outdir <- NULL
    settings$pfts[i]$pft$posteriorid <- NULL
  }

  # 4) remove rundir/outdir under host if localhost
  if (!is.null(settings$run$host)) {
    settings$host <- settings$run$host
    settings$run$host <- NULL
  }
  if (settings$host$name == "localhost") {
    settings$host$rundir <- NULL
    settings$host$outdir <- NULL
  }

  # 5) remove explicit location of storage
  settings$database$dbfiles <- NULL

  # 5) remove workflow completely (including id)
  settings$workflow <- NULL

  # save and done
  if (write) XML::saveXML(listToXml(settings, "pecan"), file = outputfile)

  ## Return settings file as a list
  return(invisible(settings))
} # clean.settings
