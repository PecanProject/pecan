##' Loads PEcAn settings XML file
##' 
##' This will try and find the PEcAn settings file in the following order:
##' \enumerate{
##' \item {--settings <file>}{passed as command line argument using --settings}
##' \item {inputfile}{passed as argument to function}
##' \item {PECAN_SETTINGS}{environment variable PECAN_SETTINGS pointing to a specific file}
##' \item {./pecan.xml}{pecan.xml in the current folder}
##' }
##' Once the function finds a valid file, it will not look further. 
##' Thus, if \code{inputfile} is supplied, \code{PECAN_SETTINGS} will be ignored. 
##' Even if a \code{file} argument is passed, it will be ignored if a file is passed through
##' a higher priority method.  
##' @param inputfile the PEcAn settings file to be used.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @author David LeBauer
##' @author Ryan Kelly
##' @author Betsy Cowdery
##' @examples
##' \dontrun{
##' ## bash shell:
##' ## example workflow.R and pecan.xml files in pecan/tests
##' R --vanilla -- --settings path/to/mypecan.xml < workflow.R 
##' 
##' ## R:
##' 
##' settings <- read.settings()
##' settings <- read.settings(file="willowcreek.xml")
##' test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
##' settings <- read.settings(test.settings.file)
##' }
read.settings.XML <- function(inputfile = "pecan.xml"){
  if(inputfile == ""){
    logger.warn("settings files specified as empty string; \n\t\tthis may be caused by an incorrect argument to system.file.")
  }
  
  loc <- which(commandArgs() == "--settings")
  ## If settings file passed at cmd line
  if (length(loc) != 0) {  
    # 1 filename is passed as argument to R
    for(idx in loc) {
      if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
        logger.info("Loading --settings=", commandArgs()[idx+1])
        xml <- xmlParse(commandArgs()[idx+1])
        break
      }
    }
    ## if settings file on $PATH
  } else if (file.exists(Sys.getenv("PECAN_SETTINGS"))) { 
    # 2 load from PECAN_SETTINGS
    logger.info("Loading PECAN_SETTINGS=", Sys.getenv("PECAN_SETTINGS"))
    xml <- xmlParse(Sys.getenv("PECAN_SETTINGS"))
    ## if settings file passed to read.settings function
  } else if(!is.null(inputfile) && file.exists(inputfile)) {
    # 3 filename passed into function
    logger.info("Loading inpufile=", inputfile)
    xml <- xmlParse(inputfile)
    ## use pecan.xml in cwd only if none exists
  } else if (file.exists("pecan.xml")) {
    # 4 load ./pecan.xml
    logger.info("Loading ./pecan.xml")
    xml <- xmlParse("pecan.xml")
  } else {
    # file not found
    logger.severe("Could not find a pecan.xml file")
  }
  
  ## convert the xml to a list
  settings <- xmlToList(xml)
  invisible(settings)
}
