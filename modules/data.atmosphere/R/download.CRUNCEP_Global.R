download.CRUNCEP <- function(outfolder, start_year, end_year, lat, lon, overwrite=FALSE, verbose=FALSE, ...){
  
  require(PEcAn.utils)
  require(lubridate)

  ylist <- seq(start_year,end_year,by=1)
    for (year in ylist) 
      {
      system2('python',system.file('python/download_Global_MsTMIP_CRUNCEP.py',package = "PEcAn.data.atmosphere"),lat,lon,year)    }
}
