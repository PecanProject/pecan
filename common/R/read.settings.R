#--------------------------------------------------------------------------------------------------#
##' 
##' @name read.settings.R
##' @title Generic function to open and parse PEcAn XML config/settings file 
##'
##' NEED MORE HERE. WORKING DRAFT OF SCRIPT TO OPEN PECAN SETTINGS
##'
##'
##' @return list object with information from input PEcAn XML settings file 
##'
##' @author Shawn Serbin
#==================================================================================================#


#---------------- Load requirements for function. -------------------------------------------------#
# Info: Load required libraries for running this function inside the PEcAn workflow.
ok = require(XML); if (! ok) stop("Package XML is not available...")      # R XML library
#--------------------------------------------------------------------------------------------------#


#---------------- Define function. ----------------------------------------------------------------#
# Info: Function to read in and parse settings file. Returns XML info as R list object.
#       This function is called by other modules to open and parse XML config file.

read.settings <- function(pecan.settings.file){
  settings.xml <- xmlParse(pecan.settings.file)
  settings <- xmlToList(settings.xml)
  if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)}
  
  # Create main output directory if it doesn't already exist
  if (! file.exists(settings$outdir)) dir.create(settings$outdir)
  
  # Move settings file to main output directory
  if (! file.exists(file.path(pecan.settings.file,settings$outdir))){
    file.copy(from = pecan.settings.file, to = settings$outdir)
  }
  
  # Re-open settings.xml in output directory
  out.path = list.files(path = settings$outdir, pattern = "*.xml",full.names=TRUE)
  settings.xml <- xmlParse(out.path)
  settings <- xmlToList(settings.xml)
  if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)}
  
  # Return settings file as a list
  return(settings)
}
#--------------------------------------------------------------------------------------------------#


####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################