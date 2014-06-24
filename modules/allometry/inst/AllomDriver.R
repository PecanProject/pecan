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
con <- query.base.con(dbname   = settings$database$bety$name,
                      password = settings$database$bety$passwd,
                      username = settings$database$bety$userid,
                      host     = settings$database$bety$host)

## mcmc settings
ngibbs = nu(settings$meta.analysis$iter)


pfts = list(FAGR = data.frame(spcd=531,name="FAgr"))
outdir = "~/Downloads/"
components = 6
allom.stats = AllomAve(pfts,components,outdir,parm="~/git/pecan/modules/allometry/data/Table3_GTR-NE-319.v2.csv",ngibbs=500,nchain=3)

pfts = list(FAGR = data.frame(spcd=531,name="FAgr"))
allom.stats = AllomAve(pfts,ngibbs=500)

