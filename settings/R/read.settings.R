##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
library(XML)
library(lubridate)
library(PEcAn.DB)
library(PEcAn.utils)

##' Loads PEcAn settings file
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
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir. If set to null nothing is saved.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @author David LeBauer
##' @author Ryan Kelly
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
##' 
##' 
##' Read settings now needs to take in three different things:
##' 1) the settings type: XML, RR
##' 2) the argument needed for loading: inputfile, list of RR id's 
##' 3) Additional settings XML that needs to be added in, such as benchmarking settings
##' HOW to do all of this cleanly with a function that is called from the command line?

read.settings <- function(type = "XML", input = "pecan.xml", supplementXML= "moresettings.xml",
                          outputfile = "pecan.CHECKED.xml"){
  if(inputfile == ""){
    logger.warn("settings files specified as empty string; \n\t\tthis may be caused by an incorrect argument to system.file.")
  }
  
  fcn <- match.fun(sprintf("read.settings.%s",type))
  settings <- do.call(fcn,list(input))
  
  settings <- expandMultiSettings(settings)
  
  settings <- papply(settings, fix.deprecated.settings)
  settings <- papply(settings, addSecrets)
  settings <- papply(settings, update.settings)
  
  #still not sure where this should go. Will a multi run involve different benchmarks for each run?
  if(!is.null(supplementXML)){ 
    settings <- papply(settings, function(x) supplement.settings(x,supplementXML))
  }
  
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
##=================================================================================================#

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
