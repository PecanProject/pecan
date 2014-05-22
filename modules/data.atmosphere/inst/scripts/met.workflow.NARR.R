#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
require(RPostgreSQL)
require(ncdf4)
#--------------------------------------------------------------------------------------------------#

# For initial NARR setup, run download.NARR.R on geo
# Update NARR from the internet
outfolder <- "/projectnb/cheas/pecan.data/input/NARR/"
start_year <- 1979
end_year   <- 2013

download.NARR(outfolder,start_year,end_year) 

#--------------------------------------------------------------------------------------------------#
# Update NARR_CF
input.id  <-  285
outfolder <- "/projectnb/cheas/pecan.data/input/NARR_CF/"
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "met2cf.NARR"
write     <-  FALSE
username  <- ""

convert.input(input.id,outfolder,pkg,fcn,write,username)

#--------------------------------------------------------------------------------------------------#
# Extract for location
input.id <-  288
newsite  <-  768

outfolder <-  paste0("/projectnb/cheas/pecan.data/input/NARR_CF_site_",newsite,"/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "extract.NARR"
write     <-  FALSE
username  <- ""

convert.input (input.id,outfolder,pkg,fcn,write,username,newsite)





