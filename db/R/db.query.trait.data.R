##' 
##'
##'
##'
##'
##'
##'
##'
##'
#==================================================================================================#
# Info:  lots of hacks for now.  Needs to be updated once full workflow is ready.


#---------------- Load requirements for function. -------------------------------------------------#
# Info: Current set as hacks.  Need to replace once the packages compile

setwd('/Users/serbin/DATA/pecan_trunk/')

#ok = require(db); if (! ok) stop("Package db is not available...")      # R XML library
source('db/R/query.bety.db.R')
source('db/R/query.bety.pft.R')
source('db/R/query.bety.prior.R')
source('db/R/query.bety.trait.data.R')
source('db/R/query.bety.traits.R')

# Utils
source('utils/R/utils.R')


# HACK: Just a hack for now.
source('common/R/read.settings.R')

ok = require(RMySQL); if (! ok) stop("Package RMySQL is not available...")      # R MySQL library
#--------------------------------------------------------------------------------------------------#


#---------------- Load PEcAn settings file. -------------------------------------------------------#
pecan.settings.file = '/Users/serbin/DATA/pecan_in/US-WCr.settings.xml'
settings = read.settings(pecan.settings.file)
#--------------------------------------------------------------------------------------------------#


#---------------- Clean up output directory. ------------------------------------------------------#
# Info: May not want to keep this code block?

file.remove(list.files(path=settings$outdir,full.names=TRUE)
            [which(file.info(list.files(path=settings$outdir,full.names=TRUE))$isdir==FALSE)])

# Create output directories if they don't already exist.  Clean up old files.
num = length(settings$pfts)
for (i in 1:num){
  out.dir = settings$pfts[i]$pft$outdir
  if (! file.exists(out.dir)) dir.create(out.dir)
  
  # Remove old files.  Clean up.
  file.remove(list.files(path=settings$pfts[i]$pft$outdir,full.names=TRUE)
              [which(file.info(list.files(path=settings$pfts[i]$pft$outdir,
                                          full.names=TRUE))$isdir==FALSE)])
  
  rm(out.dir)
}
#--------------------------------------------------------------------------------------------------#


#---------------- Load trait dictionary. ----------------------------------------------------------#
# Info:  A hack. need to remove this dependency.
trait.names <- trait.dictionary()$id
#--------------------------------------------------------------------------------------------------#


#---------------- Load trait dictionary. ----------------------------------------------------------#
newconfn <- function() query.bety.con(dbname   = settings$database$name,
                                      password = settings$database$passwd,
                                      username = settings$database$userid,
                                      host     = settings$database$host)

newcon <- newconfn()
#--------------------------------------------------------------------------------------------------#



















####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################