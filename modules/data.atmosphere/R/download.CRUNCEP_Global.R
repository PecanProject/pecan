##' Download and conver to CF CRUNCEP single grid point from MSTIMIP server using OPENDAP interface
##' @name download.CRUNCEP
##' @title download.CRUNCEP
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##'
##' @author James Simkins
download.CRUNCEP <- function(outfolder, start_date, end_date, lat, lon, overwrite=FALSE, verbose=FALSE, ...){  
  require(PEcAn.utils)
  require(lubridate)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
    for (year in ylist) 
      {
      results <- system2(system.file('scripts/download_Global_MsTMIP_CRUNCEP.sh',package = "PEcAn.data.atmosphere"),args = c(system.file('python/download_Global_MsTMIP_CRUNCEP.py',package = "PEcAn.data.atmosphere"),lat,lon,year,outfolder))
#      results <- system2('python',args = c(system.file('python/download_Global_MsTMIP_CRUNCEP.py',package = "PEcAn.data.atmosphere"),lat,lon,year,outfolder))  
    }
  
  invisible(results[length(results)])
}
