#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
require(RPostgreSQL)
require(ncdf4)
#--------------------------------------------------------------------------------------------------#

# Update NARR from the internet
outfolder <- "/projectnb/cheas/pecan.data/input/NARR/"
start_year <- 1979
end_year   <- 2013
met2cf.NARR(outfolder,start_year,end_year) 

# Update NARR_CF
system(paste("pecan/modules/data.atmosphere/inst/scripts/nc_formatting.sh")) ## How to have it find the path?

# Extract for location

input.id = 288

newsite = 768
year = TRUE

outfolder = paste0("/projectnb/cheas/pecan.data/input/NARR_CF_site_",newsite,"/")
pkg = "PEcAn.data.atmosphere"
fcn = "extract.NARR"
write = FALSE
username = "ecowdery"

convert.input (input.id,outfolder,pkg,fcn,write,username,...)









if point == TRUE{
	# one coordinate
	pnt.input <- function(input.id,outfolder,pkg,fcn,write,username,newsite,year=TRUE)
	}else{
		# deal with region here
	}





