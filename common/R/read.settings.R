#--------------------------------------------------------------------------------------------------#
##' Read settings file
##'
##' Function to read in and parse settings file.
##' Returns XML info as R list object.
##' This function is called by other modules to open and parse XML config file.
##' @name read.settings.R
##' @title Generic function to open and parse PEcAn XML config/settings file 
##' @return list object with information from input PEcAn XML settings file 
##' @author Shawn Serbin
##' @examples
##' ## not run
##' ## read.settings('common/inst/tests/test.settings.xml')
#--------------------------------------------------------------------------------------------------#
read.settings <- function(pecan.settings.file){
  settings.xml <- xmlParse(pecan.settings.file)
  settings <- xmlToList(settings.xml)
  if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)}
  
  # Create main output directory if it doesn't already exist
  if (! file.exists(settings$outdir)) dir.create(settings$outdir, recursive=TRUE)
  
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
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################
