#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
require(RPostgreSQL)
require(ncdf4)
#--------------------------------------------------------------------------------------------------#

# For initial NARR setup, run precan/modules/data.atmosphere/download.NARR.R on geo
outfolder <- "/projectnb/cheas/pecan.data/input/NARR/"
start_year <- 1979
end_year   <- 2013

download.NARR(outfolder,start_year,end_year) 

# We already know that the input id for raw NARR is 285
NARR_raw.id <- 285

#--------------------------------------------------------------------------------------------------#
# Update NARR_CF
input.id  <-  NARR_raw.id
outfolder <- "/projectnb/cheas/pecan.data/input/NARR_CF/"
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "met2cf.NARR"
write     <-  FALSE
username  <- ""

NARR_cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username) # doesn't update existing record

#--------------------------------------------------------------------------------------------------#
# Extract for location
input.id <-  NARR_cf.id
ns       <-  768

outfolder <-  paste0("/projectnb/cheas/pecan.data/input/NARR_CF_site_",ns,"/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "extract.NARR"
write     <-  FALSE
username  <- ""

NARR_extract.id <- convert.input (input.id,outfolder,pkg,fcn,write,username,newsite = ns)





