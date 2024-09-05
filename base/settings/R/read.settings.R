#' Loads PEcAn settings file
#'
#' This will try and find the PEcAn settings file in the following order:
#' \enumerate{
#'   \item `--settings <file>` passed as command line argument using `--settings`
#'   \item `inputfile` passed as argument to function
#'   \item `PECAN_SETTINGS` environment variable `PECAN_SETTINGS` pointing to a specific file
#'   \item `./pecan.xml` `pecan.xml` in the current folder
#' }
#' 
#' Once the function finds a valid file, it will not look further.
#' Thus, if \code{inputfile} is supplied, \code{PECAN_SETTINGS} will be
#'   ignored.
#' Even if a \code{file} argument is passed, it will be ignored if a file
#'   is passed through a higher priority method.
#'
#' @param inputfile the PEcAn settings file to be used.
#' @return list of all settings as loaded from the XML file(s)
#' @export read.settings
#' @import XML
#' @author Shawn Serbin
#' @author Rob Kooper
#' @author David LeBauer
#' @author Ryan Kelly
#' @author Betsy Cowdery
#' @examples
#' \dontrun{
#' ## bash shell:
#' ## example workflow.R and pecan.xml files in pecan/tests
#' R --vanilla -- --settings path/to/mypecan.xml < workflow.R
#'
#' ## R:
#'
#' settings <- read.settings()
#' settings <- read.settings(file="willowcreek.xml")
#' test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
#' settings <- read.settings(test.settings.file)
#' }
read.settings <- function(inputfile = "pecan.xml") {
  if (inputfile == "") {
    PEcAn.logger::logger.warn(
      "settings files specified as empty string;",
      "\n\t\tthis may be caused by an incorrect argument to system.file.")
  }

  loc <- which(commandArgs() == "--settings")
  ## If settings file passed at cmd line
  if (length(loc) != 0) {
    # 1 filename is passed as argument to R
    for (idx in loc) {
      if (!is.null(commandArgs()[idx + 1])
          && file.exists(commandArgs()[idx + 1])) {
        PEcAn.logger::logger.info("Loading --settings=", commandArgs()[idx + 1])
        xml <- XML::xmlParse(commandArgs()[idx + 1])
        break
      }
    }
    ## if settings file on $PATH
  } else if (file.exists(Sys.getenv("PECAN_SETTINGS"))) {
    # 2 load from PECAN_SETTINGS
    PEcAn.logger::logger.info(
      "Loading PECAN_SETTINGS=",
      Sys.getenv("PECAN_SETTINGS"))
    xml <- XML::xmlParse(Sys.getenv("PECAN_SETTINGS"))
    ## if settings file passed to read.settings function
  } else if (!is.null(inputfile) && file.exists(inputfile)) {
    # 3 filename passed into function
    PEcAn.logger::logger.info("Loading inpufile=", inputfile)
    xml <- XML::xmlParse(inputfile)
    ## use pecan.xml in cwd only if none exists
  } else if (file.exists("pecan.xml")) {
    # 4 load ./pecan.xml
    PEcAn.logger::logger.warn(inputfile, " not found!")
    PEcAn.logger::logger.info("Loading ./pecan.xml")
    xml <- XML::xmlParse("pecan.xml")
  } else {
    # file not found
    PEcAn.logger::logger.severe("Could not find a pecan.xml file")
  }

  ## convert the xml to a list
  settings <- XML::xmlToList(xml)
  settings <- strip_comments(settings)
  settings <- as.Settings(settings)
  settings <- expandMultiSettings(settings)

  ## setup Rlib from settings
  if (!is.null(settings$Rlib)) {
    .libPaths(settings$Rlib)
  }

  return(invisible(settings))
}



#' Strip comments from parsed pecan.xml
#'
#' Allows use of HTML style comments (`<!-- a comment -->`) in pecan.xml files
#' by removing them after converted to a list. Inspired by
#' https://stackoverflow.com/questions/37853679/removing-elements-in-a-nested-r-list-by-name
#'
#' @param x a settings list
#'
#' @return list
#' @noRd
#'
strip_comments <- function(x) {
  # function to find depth of a list element
  # see http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
  depth <- function(this, thisdepth = 0) {
    if (!is.list(this)) {
      return(thisdepth)
    } else{
      return(max(unlist(
        lapply(this, depth, thisdepth = thisdepth + 1)
      )))
    }
  }
  
  thisdepth <- depth(x)
  nameIndex <- which(names(x) == "comment")
  if (thisdepth == 0) {
    return(x)
  } else if (length(nameIndex)) {
    x <- x[-nameIndex]
  }
  return(lapply(x, strip_comments))
  
}
