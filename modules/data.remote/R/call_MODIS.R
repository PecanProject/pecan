##' Get MODIS data by date and location  
##' 
##' @name call_MODIS
##' @title call_MODIS
##' @export
##' @param outfolder where the output file will be stored
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
##' depends on the MODISTools package version 1.1.0
##' 
##' @examples
##' \dontrun{
##' test_modistools <- call_MODIS(product = "MOD15A2H", band = "Lai_500m", start_date = "2004300", end_date = "2004365", lat = 38, lon = -123, size = 0, band_qc = "FparLai_QC", band_sd = "LaiStdDev_500m", package_method = "MODISTools")
##' plot(lubridate::yday(test_modistools$calendar_date), test_modistools$data, type = 'l', xlab = "day of year", ylab = test_modistools$band[1])
##' test_reticulate <- call_MODIS(product = "MOD15A2H", band = "Lai_500m", start_date = "2004300", end_date = "2004365", lat = 38, lon = -123, size = 0, band_qc = "",band_sd = "", package_method = "reticulate")
##' }
##' 
##' @author Bailey Morrison
##'  
call_MODIS <- function(outfolder = ".", start_date, end_date, lat, lon, size = 0, product, band, band_qc = "", band_sd = "", package_method = "MODISTools") {
  
  # makes the query search for 1 pixel and not for rasters for now. Will be changed when we provide raster output support.
  size <- 0
  
  # set start and end dates to correct format
  if (package_method == "MODISTools"){
    
    products = MODISTools::mt_products()
    if (!(product %in% products$product))
    {
      print(products)
      stop("Product not available for MODIS API. Please chose a product from the list above.")
    } else {
      print("Check #1: Product exists!")
    }
    

    dates <- MODISTools::mt_dates(product = product, lat = lat, lon = lon)$modis_date
    dates <- as.numeric(substr(dates, 2, nchar(dates)))
    if (as.numeric(start_date) <= dates[1] | as.numeric(end_date) >= dates[length(dates)])
    {
      print(paste("Range of dates for product are ", dates[1], " - ", dates[length(dates)], sep = ""))
      stop("Please choose dates between the date range listed above.")
    } else {
      print("Check #2: Dates are available!")
    }
    
    bands <- MODISTools::mt_bands(product = product)
    if (!(band %in% bands$band))
    {
      print(bands$band)
      stop("Band selected is not avialable. Please selected from the bands listed above that correspond with the data product.")
    } else {
      print("Check #3: Band Exists!")
    }
  
    
    print("Extracting data")
    
    start <- as.Date(start_date, "%Y%j")
    end <- as.Date(end_date, "%Y%j")
    
    # extract main band data from api
    dat <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band,
                                  start=start, end=end, km_ab=size, km_lr=size)
    # extract QC data
    if(band_qc != ""){
      qc <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band_qc,
                                  start=start, end=end, km_ab=size, km_lr=size)
    }
   
    # extract stdev data
    if(band_sd != ""){
      sd <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band_sd,
                                  start=start, end=end, km_ab=size, km_lr=size)
    }
    
    
    if (band_qc == "")
    {
      QC <- rep("nan", nrow(dat))
    } else {
      QC <- as.numeric(qc$value)
    }
    
    if (band_sd == "")
    {
      SD <- rep("nan", nrow(dat))
    } else {
      SD <- as.numeric(sd$value) * as.numeric(sd$scale) #formatC(sd$data$data*scale, digits = 2, format = 'f')
    }
    
    output <- as.data.frame(cbind(dat$modis_date, dat$calendar_date, dat$band, dat$tile, dat$latitude, dat$longitude, dat$pixel, dat$value, QC, SD), stringsAsFactors = F)
    names(output) <- c("modis_date", "calendar_date", "band", "tile", "lat", "lon", "pixels", "data", "qc", "sd")
    
    output[,5:10] <- lapply(output[,5:10], as.numeric)
    
    # scale the data + stdev to proper units
    output$data <- output$data * (as.numeric(dat$scale))
    output$sd <- output$sd * (as.numeric(dat$scale))
    output$lat <- round(output$lat, 4)
    output$lon <- round(output$lon, 4)
    
    fname <- paste(product, "_", band, "output_", start_date, "_", end_date, "_", lat, "_", lon, ".csv", sep = "")
    fname <- paste0(outfolder, "/", fname)
    write.csv(output, fname)
    return(output)}

  
  if (package_method == "reticulate"){
    # load in python script
    script.path <- file.path(system.file("extract_modis_data.py", package = "PEcAn.data.remote"))
    #script.path = file.path('/Users/bmorrison/pecan/modules/data.remote/inst/extract_modis_data.py')
    reticulate::source_python(script.path)
    
    # extract the data
    output <- extract_modis_data(product = product, band = band, lat = lat, lon = lon, start_date = start_date, end_date = end_date, size = size, band_qc = band_qc, band_sd = band_sd)
    output[,5:10] <- lapply(output[,5:10], as.numeric)
    output$lat <- round(output$lat, 4)
    output$lon <- round(output$lon, 4)
    
    fname <- paste(product, "_", band, "_", start_date, "_", end_date, "_", lat, "_", lon, ".csv", sep = "")
    fname <- paste0(outfolder, "/", fname)
    write.csv(output, fname)
    return(output)}
}
