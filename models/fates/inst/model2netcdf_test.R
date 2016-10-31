#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
#--------------------------------------------------------------------------------------------------#

outdir <- "/data/sserbin/Modeling/FATES/output/"
start_date <- '2004-01-01 00:00:00'
end_date <- '2005-12-31 00:00:00'
sitelat <- 42.56341
sitelon <- 289.1853
year <- "2004"

library(PEcAn.FATES)
model2netcdf.FATES(outdir="/data/sserbin/Modeling/FATES/output/",sitelat=42.56341,sitelon=289.1853,start_date="2004-01-01 00:00:00",end_date="2005-12-31 00:00:00")