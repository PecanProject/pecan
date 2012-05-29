##' Generic function to open and parse PEcAn XML config/settings file 
##'
##' NEED MORE HERE. WORKING DRAFT OF SCRIPT TO OPEN PECAN SETTINGS
##'
##'
##'
##'

#----------------------------------------------------------------------#
# Load requirements
#
library(XML)
require(db)


#
# Define settings file
pecan.settings.file = Sys.getenv("PECANSETTINGS")

# Parse settings file
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
