##' Get MODIS data by date and location
##' 
##' @name call_MODIS
##' @title call_MODIS
##' @export
##' @param outfolder where the output file will be stored
##' @param fname  name of netcdf file to output
##' @param start_date  string value for beginning of date range for download in unambiguous date format (YYYYJJJ)
##' @param end_date    string value for end of date range for download in unambiguous date format (YYYYJJJ)
##' @param lat    Latitude of the pixel
##' @param lon    Longitude of the pixel
##' @param size   kmAboveBelow and kmLeftRight distance in km to be included
##' @param product string value for MODIS product number
##' @param band   string value for which measurement to extract
##' @param band_qc string value for which quality control band, or use "NA" if you do not know or do not need QC information (optional)
##' @param band_sd string value for which standard deviation band, or use "NA" if you do not know or do not need StdDev information (optional)
##' @param package_method string value to inform function of which package method to use to download modis data. Either "MODISTools" or "reticulate" (optional)
##' 
##' depends on a number of Python libraries. sudo -H pip install numpy suds netCDF4 json
##' 
##' @examples
##' \dontrun{
##' test_modistools <- call_MODIS(product = "MCD15A2H", band = "Lai_500m", start_date="2004300",end_date="2004365",lat=38,lon=-123,size=0,band_qc = "FparLai_QC",band_sd = "LaiStdDev_500m", package_method = "MODISTools")
##' plot(lubridate::yday(test_modistools$calendar_date), test_modistools$data, type = 'l', xlab = "day of year", ylab = test_modistools$band[1])
##' test_reticulate <- call_MODIS(product = "MCD15A2H", band = "Lai_500m", start_date="2004300",end_date="2004365",lat=38,lon=-123,size=0,band_qc = "FparLai_QC",band_sd = "LaiStdDev_500m", package_method = "reticulate")
##' }
##' 
##' @author Bailey Morrison
##'  
call_MODIS <- function(outfolder = ".", fname = "m_data.nc", start_date, end_date, lat, lon, size = 0, product = "MCD15A2H", band = "Lai_500m", band_qc = "FparLai_QC", band_sd = "LaiStdDev_500m", package_method = "MODISTools") {
  
  # makes the query search for 1 pixel and not for rasters for now. Will be changed when we provide raster output support.
  size = 0
  
  # set start and end dates to correct format
  if (package_method == "MODISTools"){
    start = as.Date(start_date, "%Y%j")
    end = as.Date(end_date, "%Y%j")
    
    # extract main band data from api
    dat <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band,
                                  start=start, end=end, km_ab=size, km_lr=size)
    # extract QC data
    if(!is.na(band_qc)){
      qc <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band_qc,
                                  start=start, end=end, km_ab=size, km_lr=size)
    } else {
      qc <- NULL
    }
    
    # extract stdev data
    if(!is.na(band_sd)){
      sd <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band_sd,
                                  start=start, end=end, km_ab=size, km_lr=size)
    } else {
      sd <- NULL
    }
    
    # reformat output from list/factors to simple str+numeric dataframe
    data = data.frame(lapply(dat$data, as.character), stringsAsFactors = F)
    
    headers = (dat$header)
    scale = as.numeric(headers$scale)
    # change lat and lon once we start outputing rasters, the lat and lon will not be the same for all data based on input lat and lon
    latitude = as.character(rep(headers$latitude, nrow(data)))
    longitude = as.character(rep(headers$longitude, nrow(data)))

    QC = as.numeric(qc$data$data)
    SD = as.numeric(sd$data$data) #formatC(sd$data$data*scale, digits = 2, format = 'f')
    
    output = as.data.frame(cbind(data$modis_date, data$calendar_date, data$band, data$tile, latitude, longitude, data$pixel, data$data, QC, SD), stringsAsFactors = F)
    names(output) = c("modis_date", "calendar_date", "band", "tile", "lat", "lon", "pixels", "data", "qc", "sd")
    
    output[,5:10] = lapply(output[,5:10], as.numeric)
    
    # scale the data + stdev to proper units
    output$data = output$data*scale
    output$sd = output$sd*scale
    output$lat = round(output$lat, 4)
    output$lon = round(output$lon, 4)
    
    return(output)}

  
  if (package_method == "reticulate"){
    # just to check that the right python is being used (needs to be >2.7.10 or pandas module in extract_modis_data.py will not work). If using Rstudio to run code, python installed from online download will result in the incorreect python location for this function to work.
    #Sys.getenv("RETICULATE_PYTHON")
    
    # load in python script
    script.path <- file.path(system.file("extract_modis_data.py", package = "PEcAn.data.remote"))
    reticulate::source_python(script.path)
    
    # extract the data
    output = extract_modis_data(product = product, band = band, lat = lat, lon = lon, start_date = start_date, end_date = end_date, size = size, band_qc = band_qc, band_sd = band_sd)
    output[,5:10] = lapply(output[,5:10], as.numeric)
    output$lat = round(output$lat, 4)
    output$lon = round(output$lon, 4)
    
    return(output)}
}
