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
##' @param siteID string value of a PEcAn site ID. Currently only used for output filename.
##' @param QC_filter Converts QC values of band and keeps only data values that are excellent or good (as described by MODIS documentation), and removes all bad values. qc_band must be supplied for this parameter to work. Default is False.
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
call_MODIS <- function(outfolder = NULL, start_date, end_date, lat, lon, size = 0, product, band, band_qc = "", band_sd = "", siteID = NULL, package_method = "MODISTools", QC_filter = F) {
  
  # makes the query search for 1 pixel and not for rasters for now. Will be changed when we provide raster output support.
  size <- 0
# reformat start and end date if they are in YYYY/MM/DD format instead of YYYYJJJ
  if (grepl("/", start_date) == T)
  {
    start_date = as.Date(paste0(lubridate::year(start_date), spatial.tools::add_leading_zeroes(lubridate::yday(start_date), 3)), format = "%Y%j")
  }
  
  if (grepl("/", end_date) == T)
  {
    end_date <- as.Date(paste0(lubridate::year(end_date), spatial.tools::add_leading_zeroes(lubridate::yday(end_date), 3)), format = "%Y%j")
  }
  start_date <- as.Date(start_date, format = "%Y%j")
  end_date <- as.Date(end_date, format = "%Y%j")


  #################### if package_method == MODISTools option ####################
  if (package_method == "MODISTools")
  {
    #################### FUNCTION PARAMETER PRECHECKS #################### 
    #1. check that modis product is available
    products = MODISTools::mt_products()
    if (!(product %in% products$product))
    {
      print(products)
      stop("Product not available for MODIS API. Please chose a product from the list above.")
    } 
    
    #2. check that modis produdct band is available
    bands <- MODISTools::mt_bands(product = product)
    if (!(band %in% bands$band))
    {
      print(bands$band)
      stop("Band selected is not avialable. Please selected from the bands listed above that correspond with the data product.")
    } 
    
    #3. check that dates asked for in function parameters are fall within dates available for modis product/bands.
    dates <- MODISTools::mt_dates(product = product, lat = lat, lon = lon)$modis_date
    dates <- as.Date(as.character(substr(dates, 2, nchar(dates))), format = "%Y%j")
    
    ########## Date case 1:  user only wants one date ##########
    if (start_date == end_date)
    {
      if (as.numeric(start_date) >= dates[1] & as.numeric(end_date) <= dates[length(dates)])
      {
        print("Extracting data")
        
        start <- as.Date(start_date, "%Y%j")
        end <- as.Date(end_date, "%Y%j")
      }
      ########## For Date case 1: if only one date is asked for, but date is not within modis data prodate date range ##########
      if (as.numeric(start_date)< dates[1] || as.numeric(end_date)> dates[length(dates)])
      {
        print(start)
        stop("start or end date are not within MODIS data product date range. Please choose another date.")
      }
    } else { 
      ########## Date case 2: user want a range of dates ##########
      # Best case scenario: Start and end date asked for fall with available date range of modis data product.
      if (as.numeric(start_date)>=dates[1] & as.numeric(end_date)<=dates[length(dates)])
      {
        print("Check #2: All dates are available!")
      }
      
      # Okay scenario: Some MODIS data is available for parameter start_date and end_date range, but either the start_date or end_date falls outside the range of availble 
      # MODIS data dates
      if (as.numeric(start_date)<=dates[1] & as.numeric(end_date)>=dates[1])
      {
        start_date = dates[1]
        print("WARNING: Dates are only partially available. Start date before modis data product is available.")
      } 
      if (as.numeric(end_date)>=dates[length(dates)] & as.numeric(start_date) <= dates[length(dates)])
      {
        end_date = dates[length(dates)]
        print("WARNING: Dates are only partially available. End date befire modis data product is available.")
      } 
      
      # Unacceptable scenario: start_date and end_date does not fall within the availa MODIS data product date range. There is no data to extract in this scenario.
      if ((as.numeric(start_date)<dates[1] & as.numeric(end_date <dates[1])) | (as.numeric(start_date)>dates[length(dates)] & as.numeric(end_date)>dates[length(dates)]))
      {
        stop("No MODIS data available start_date and end_date parameterized.")
      }
      
      start <- as.Date(start_date, "%Y%j")
      end <- as.Date(end_date, "%Y%j")
    }
    
    print("Extracting data")
    cat(paste("Product =", product, "\n", "Band =", band, "\n", "Date Range =", start, "-", end, "\n", "Latitude =", lat, "\n", "Longitude =", lon, sep = " "))
    
    # extract main band data from api
    dat <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band,
                                 start=start_date, end=end_date, km_ab=size, km_lr=size)
    
    # extract QC data
    if(band_qc != "")
    {
      qc <- MODISTools::mt_subset(lat=lat, lon=lon, product=product, band=band_qc,
                                  start=start, end=end, km_ab=size, km_lr=size)
    }
    
    # extract stdev data
    if(band_sd != "")
    {
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
    
    if (QC_filter)
    {
      output$qc == as.character(output$qc)
      for (i in seq_len(nrow(output)))
      {
        convert = paste(binaryLogic::as.binary(as.integer(output$qc[i]), n = 8), collapse = "")
        output$qc[i] = substr(convert, nchar(convert)-2, nchar(convert))
      }
      good <- which(output$qc == "000" | output$qc == "001")
      if (length(good) > 0 | !(is.null(good)))
      {
        output = output[good,]
      } else {
        print("All QC values are bad. No data to output with QC filter == TRUE.")
      }
    }
    
    if (!(outfolder == ""))
    {
      fname <- paste(product, "_", band, "_", siteID, "_output_", start_date, "_", end_date, ".csv", sep = "")
      fname <- paste0(outfolder, "/", fname)
      write.csv(output, fname, row.names = F)
    }
    
    return(output)
  }
  
  
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
    
    if (!(is.null(outfolder)))
    {
      fname <- paste(product, "_", band, "_", start_date, "_", end_date, "_", lat, "_", lon, ".csv", sep = "")
      fname <- file.path(outfolder, fname)
      write.csv(output, fname)
    }
    
    return(output)}
}
