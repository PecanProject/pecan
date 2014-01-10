## code to run Allom stand alone
##
## load pecan settings
if(interactive()){
  settings.file = "/home/mdietze/pecan/tests/settings.bartlett.xml"
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}
library(XML)
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
#require(PECAn)

## libraries & database connection
library(mvtnorm)
library(MCMCpack)
haveMPI <- require(Rmpi)
library(RMySQL)
dvr <- dbDriver("MySQL")
con <- query.base.con(dbname   = settings$database$name,
                      password = settings$database$passwd,
                      username = settings$database$userid,
                      host     = settings$database$host)

## mcmc settings
ngibbs = nu(settings$meta.analysis$iter)