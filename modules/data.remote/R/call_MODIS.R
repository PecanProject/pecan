##' Get MODIS data by date and location
##'
##' @name call_MODIS
##' @title call_MODIS
##' @export
##' @param outfolder where the output file will be stored
##' @param fname  name of netcdf file to output
##' @param start_date  beginning of date range for LAI download in unambiguous date format
##' @param end_date    end of date range for LAI download in unambiguous date format
##' @param lat    Latitude of the pixel
##' @param lon    Longitude of the pixel
##' @param size   NS and WE distance in km to be included
##' @param product MODIS product number
##' @param band   which measurement to extract
##' @param qc_band which quality control band (optional)
##' @param sd_band which standard deviation band (optional)
##' @param verbose tell python whether or not to print debug statements (all or nothing)
##' 
##' depends on a number of Python libraries. sudo -H pip install numpy suds netCDF4
##' 
##' @examples 
##' \dontrun{
##' test <- call_MODIS(start="2001001",end="2016366",lat=44.0646,lon=-71.28808,size=3,qc_band = "FparLai_QC",sd_band = "LaiStdDev_1km")
##' }
##' 
call_MODIS <- function(outfolder = ".", fname = "m_data.nc", start_date, end_date, lat, lon, size = 0, 
                       product = "MOD15A2", band = "Lai_1km", qc_band = NA, sd_band = NA, verbose = TRUE) {
  
  start = strftime(as.Date(start_date),'%Y%j')
  end = strftime(as.Date(end_date),'%Y%j')
  # library(MODISTools)
  # 
  # dat <- MODISTools::GetSubset(Lat=lat, Long=lon, Product=product, Band=band, 
  #                  StartDate=as.integer(start), EndDate=as.integer(end), KmAboveBelow=size, KmLeftRight=size)
  # if(!is.na(qc_band)){
  # qc <- MODISTools::GetSubset(Lat=lat, Long=lon, Product=product, Band=qc_band, 
  #                              StartDate=as.integer(start), EndDate=as.integer(end), KmAboveBelow=size, KmLeftRight=size)
  # } else {
  #   qc <- NULL
  # }
  # if(!is.na(sd_band)){
  #   sd <- MODISTools::GetSubset(Lat=lat, Long=lon, Product=product, Band=sd_band, 
  #                               StartDate=as.integer(start), EndDate=as.integer(end), KmAboveBelow=size, KmLeftRight=size)
  # } else {
  #   sd <- NULL
  # }
  # 
  # return(list(dat,qc,sd))
  
  library(rPython)
  
  # The name of the netCDF file. 
  fname <- paste0(outfolder, "/", fname)
  
  # Distance of the are both east-west and north-south from the center of the pixel.
  kmNS <- as.integer(size)
  kmWE <- as.integer(size)
  
  # Here it assigns the run directory and given variables values within python
  rPython::python.assign("cwd", getwd())
  
  rPython::python.assign("start", as.integer(start))
  if ( rPython::python.get("start") != start) {
    stop("call_MODIS start date sent incorrectly")
  } 
  
  rPython::python.assign("end", as.integer(end))
  if (rPython::python.get("end") != end) {
    stop("call_MODIS end date sent incorrectly")
  }
  
  rPython::python.assign("lat", lat)
  rPython::python.assign("lon", lon)
  rPython::python.assign("kmNS", kmNS)
  rPython::python.assign("kmWE", kmWE)
  rPython::python.assign("fn", fname)
  rPython::python.assign("product", product)
  rPython::python.assign("band", band)
  rPython::python.assign("qcband", qc_band)
  rPython::python.assign("sdband", sd_band)
  rPython::python.assign("debug", verbose)
  
  # Here we import the MODIS python script as a module for the python. That way we can
  # run the routines within the script as independent commands.
  script.path <- dirname(system.file("modisWSDL.py", package = "PEcAn.data.remote"))
  rPython::python.exec(paste0("import sys; sys.path.append(\"", script.path, "\")"))
  rPython::python.exec("import modisWSDL")
  
  # This is overkill if you are not editing modisWSDL, but if you are developing this
  # will refresh the definition of the module
  rPython::python.exec("reload(modisWSDL)")
  
  # And here we execute the main MODIS run. Although it should be noted that while we get
  # values of the run here, the script also does write a netCDF output file.
  rPython::python.exec("m, k, date = modisWSDL.run_main(start_date=start, end_date=end,la=lat,lo=lon,kmAB=kmNS,kmLR=kmWE,fname=fn,product=product,band=band,qcband=qcband,sdband=sdband,debug=debug)")
  
  # m = The MODIS observed LAI for the given pixel k = The standard deviation of the
  # MODIS LAI. Be careful with this as it is at times very low date = Year and
  # day-of-year of the observation
  m <- rPython::python.get("[ map(float, x) for x in m.data ]")
  if (!is.na(sd_band)) {
    k <- rPython::python.get("[ map(float, x) for x in k.data ]")
  } else {
    k <- NA
  }
  date <- rPython::python.get("date")
  #date = strptime(date, format='%Y%j',tz = 'UTC') %>% as.POSIXct()
  
  return(invisible(list(m = m, k = k, date = date)))
} # call_MODIS
