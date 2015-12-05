download.CRUNCEP <- function(outfolder, start_year, end_year, lat, lon, overwrite=FALSE, verbose=FALSE, ...){
  
  require(PEcAn.utils)
  require(lubridate)
#  start_date <- as.POSIXlt(start_date, tz = "GMT")
#  end_date <- as.POSIXlt(end_date, tz = "GMT")
#  start_year <- year(start_date)
#   end_year   <- year(end_date)
  
# start_year = 2001
#  end_year = 2005
#  lat = 45.0
#  lon = -90.2
  ylist <- seq(start_year,end_year,by=1)
    for (year in ylist) 
      {
      system(paste('/usr/local/anaconda2/bin/python ~/pecan/modules/data.atmosphere/python/download_Global_MsTMIP_CRUNCEP.py ',lat,lon,year))
    }
}

