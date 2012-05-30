##' Generic function to open and parse PEcAn XML config/settings file 
##'
##' NEED MORE HERE. WORKING DRAFT OF SCRIPT TO OPEN PECAN SETTINGS
##'
##'
##'
##'
#==================================================================================================#


#---------------- Load requirements for function. -------------------------------------------------#
# Info: Load required libraries for running this function inside the PEcAn workflow.
ok = require(XML); if (! ok) stop("Package XML is not available...")      # R XML library
#require(db)      # PEcAn db library
#--------------------------------------------------------------------------------------------------#


#---------------- Import command line arguments. --------------------------------------------------#
args <- commandArgs(trailingOnly = TRUE)
#print(args)       # echo arguments
#--------------------------------------------------------------------------------------------------#


#---------------- Define function. ----------------------------------------------------------------#
# Info: This function is called by other modules to open and parse XML config file.

# Define settings file - debug hack. REMOVE?
#pecan.settings.file = Sys.getenv("PECANSETTINGS")
#pecan.settings.file = '/Users/serbin/DATA/pecan_in/US-WCr.settings.xml'
pecan.settings.file = args[1]     # Define path to settings file

#message(paste("---- Path to settings file: ",pecan.settings.file))

# Function to read in and parse settings file. Returns XML info as R list object.
read.settings = function(pecan.settings.file){
  settings.xml <- xmlParse(pecan.settings.file)
  settings <- xmlToList(settings.xml)
  if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)}
  return(settings)
}
#--------------------------------------------------------------------------------------------------#


####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################