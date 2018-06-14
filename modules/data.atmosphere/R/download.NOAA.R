###Comments prefixed with three #s are code statements for debugging

##' Helper function
##'@param d a POSIX Date
##'@return A representation of the date as a string with no separators
##' rnoaa::gefs requires a nate in the format YYYYMMDD instead of a POSIX date
##' This function requires that the lubridate package be installed.
POSIX.to.GEFSDate <- function(d) {
  mon = lubridate::month(d)  #Extract parts of the date
  day = lubridate::day(d)
  if (mon < 10) { mon = paste0("0", mon) } #Add leading zeros, if necessary
  if (day < 10) { day = paste0("0", day) }
  return(paste0(lubridate::year(d), mon, day)) #concantanate parts of the date and return
}

##' Helper function
##' @param d1 a POSIX Date
##' @param d2 a POSIX Date
##' This function returns a value greater than one if the first date is greater than the second, zero if the 
##' dates are the same, and a negative value if the first date is less than the second.
##' This function requires that the lubridate package be installed.
compare.POSIX.dates <- function(d1, d2) {
  d1_year = lubridate::year(d1)
  d1_month = lubridate::month(d1)
  d1_day = lubridate::day(d1)
  
  d2_year = lubridate::year(d2)
  d2_month = lubridate::month(d2)
  d2_day = lubridate::day(d2)
  
  if (d1_year != d2_year) {
    return(d1_year - d2_year)
  } else if (d1_month != d2_month) {
    return(d1_month - d2_month)
  } else {
    return(d1_day - d2_day)
  }
}





##' Download NOAA Weather Data
##' 
##' Information on NOAA weather units can be found here:
##' https://www.ncdc.noaa.gov/crn/measurements.html
##' Note that the temperature is measured in degrees C, but is converted at the station and downlaoded
##' in Kelvin.
##'
##' Download and convert to CF NOAA weather data
##' @param outfolder Directory where results should be written
##' @param start_date,end_date Range of dates/times to be downloaded
##' @param site_id numeric. Currently ignored
##' @param lat site latitude in decimal degrees
##' @param lon site longitude in decimal degrees
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param verbose logical.  Print additional debug information.
##' @param include_forecast logical.  If TRUE, 16 day forecast data is tacked on to the end of the data set
##' @param ... Other arguments, currently ignored
##' @export
##' 
##' @author Luke Dramko
##' 
download.NOAA <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in,
                             overwrite = FALSE, verbose = FALSE, include_forecast = FALSE, ...) {
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  
  #Date error checking - Checks to see if the start date is before the end date
  if (compare.POSIX.dates(end_date, start_date) < 0) {
    PEcAn.logger::logger.severe("Invalid dates: end date occurs before start date")
  }
  
  #Bounds date checking
  #NOAA's GEFS database started recording data January 1, 2018.
  NOAA_GEFS_Start_Date = as.POSIXlt("2008-01-01")
  
  if (compare.POSIX.dates(Sys.Date(), end_date) < 0 || compare.POSIX.dates(start_date, NOAA_GEFS_Start_Date) < 0) {
    PEcAn.logger::logger.severe(sprintf('Input year range (%d:%d) exceeds the NOAA range (%d:%d), or date provided is beyond current date.',
                                        lubridate::year(start_date), lubridate::year(end_date),
                                        lubridate::year(NOAA_GEFS_Start_Date), lubridate::year(Sys.Date())))
  }
  #End date error checking
  
  #Set up the data frame with file information.  This data frame will be returned and stored in the BETY database to 
  #locate the files later.
  #file_info <- data.frame(
   # file = character(rows),
  #  host = character(rows),
  #  mimetype = character(rows),
  #  formatname = character(rows),
   # startdate = character(rows),
  #  enddate = character(rows),
    #dbfile.name =                   #paste("GFDL", model, scenario, ensemble_member, sep = "."),   # 'GFDL',
   # stringsAsFactors = FALSE
  #)
  
  
  #NOAA variable downloading
  
  #We want data for each of the following variables. Here, we're just getting the raw data; later, we will convert it to the 
  #cf standard format when relevant.
  noaa_var_names = c("Temperature_height_above_ground_ens", "Pressure_surface_ens", "Relative_humidity_height_above_ground_ens", "Downward_Long-Wave_Radp_Flux_surface_6_Hour_Average_ens", 
                   "Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average_ens", "Total_precipitation_surface_6_Hour_Accumulation_ens",
                   "u-component_of_wind_height_above_ground_ens", "v-component_of_wind_height_above_ground_ens")
  
  #These are the cf standard names
  cf_var_names = c("air_temperature", "air_pressure", "specific_humidity", "surface_downwelling_longwave_flux_in_air", 
                      "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "eastward_wind", "northward_wind")
  
  #Creating a data frame to store all of the information about the variables:
  var_info = data.frame(
    noaa_var_names,
    cf_var_names
  )
  
  
  # This debugging loop allows you to check if the cf variables are correctly mapped to the equivalent
  # NOAA variable names.  This is very important, as much of the processing below will be erroneous if 
  # these fail to match up.
  #for (i in 1:length(cf_var_names)) {
  #  print(sprintf("cf / noaa   :   %s / %s", cf_var_names[[i]], noaa_var_names[[i]]))
  #}
  
  noaa_data = list()
  
  #Setting up the loops.  num_days represents the length of time in days before the start and end of the data being collected.
  num_days = lubridate::time_length(lubridate::interval(start_date, end_date), unit="day")
  request_time = Sys.time()
  
  
  ###NOTE: THE FUNCTION BELOW MAY FAIL IF THE DATA DOESN'T EXIST ON THE MOST CURRENT AVALIABLE DAY (FOR EXAMPLE,
  ###YOU REQUEST 6.00 P.M. DATA AT 1.00 PM)
  
  #We're gathering data for each variable we want in the NOAA data set
  for (i in 1:length(cf_var_names)) {
    #Some code...
    current_date = start_date #Incremented in the loop to fetch the proper day's data for the
    gefs_date = POSIX.to.GEFSDate(current_date)
    
    var_dat = matrix(rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0000", date=gefs_date)$data,
                     nrow=21, ncol=1)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1200", date=gefs_date)$data)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1800", date=gefs_date)$data)
    j = 1
    while (j < num_days) {
      current_date = current_date + lubridate::days(1)  #increments the date by one
      gefs_date = POSIX.to.GEFSDate(current_date)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1200", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1800", date=gefs_date)$data)
      j = j + 1   
    }
    noaa_data[[i]] = var_dat
    #some code...
  }
  
  print(noaa_data[[1]])
  
  quit(save = "no", status = 0)  ###Currently here to keep the function from running past this point.  For debugging only.
  
  #This code actually does the downloading of the information using the rnoaa gefs function.  It is stored
  #as a list of lists (the data itself is also stored in a list).
  for (i in 1:length(cf_var_names)) {
    temp_var = rnoaa::gefs(noaa_var_names[[i]], lat, lon, raw=TRUE, time_idx = 1, forecast_time = "0000", date= get_date)
    noaa_data[[i]] = temp_var
  }
  
  save(noaa_data, file="/home/ldramko/Working/noaa_only.data")
  
  ###load(noaa_data, file="noaa_only.data")
  
  ###print(noaa_data)
  
  # Not all NOAA data units match the cF data standard.  In this next section, data are processed to
  # confirm with the standard when necessary.
  # The following is a list of variables which need to be processed:
  # 1. NOAA's relative humidity must be converted to specific humidity
  # 2. NOAA's measure of precipitation is the accumulation over 6 hours; cf's standard is precipitation per second
  
  #Convert NOAA's relative humidity to specific humidity
  humid_index = cf_var_names == "specific_humidity"
  
  humid_ = noaa_data[humid_index]
  temperature_ = noaa_data[cf_var_names == "air_temperature"]
  pressure_ = noaa_data[cf_var_names == "air_pressure"]
  
  #The extraction of each necessary element from the list noaa_data is done in two steps for simplicity.
  humid_data = (humid_[[1]])$data
  temperature_data = (temperature_[[1]])$data
  pressure_data = (pressure_[[1]])$data
  
  ###print("***************** relative humidity ***********************")
  ###print(humid_data)
  ### print("****************************************")
  ### print(temperature_data)
  ### print("****************************************")
  ### print(pressure_data)
   
  #Depending on the data you download, sometimes R stores it as a vector and sometimes as a matrix; the
  #different cases must be processed with different loops.
  if (as.logical(nrow(humid_data))) {
     for (i in 1:length(humid_data)) {
       humid_data[i] = PEcAn.data.atmosphere::rh2qair(humid_data[i], temperature_data[i], pressure_data[i])
     }
  } else if (!as.logical(nrow(humid_data))) {
     for (i in 1:nrow(humid_data)) {
       for (j in 1:ncol(humid_data)) {
          humid_data[i,j] = PEcAn.data.atmosphere::rh2qair(humid_data[i,j], temperature_data[i,j], pressure_data[i,j])
        }
      }
    } else {     PEcAn.logger::logger.severe("Error: Humidity data saved in an invalid format!")
  }
  #Update the noaa_data list with the correct data
  noaa_data[humid_index][[1]]$data <- humid_data
  
  ###print(noaa_data)
  
  # Convert NOAA's total precipitation (kg m-2) to precipitation flux (kg m-2 s-1)
  #There are 21600 seconds in 6 hours.  NOAA precipitation data is an accumulation over 6 hours.
  precip_index = cf_var_names == "precipitation_flux"
  
  noaa_data[precip_index][[1]]$data = noaa_data[precip_index][[1]]$data / 21600
  
  #############################################
  # Done with data processing.  Now writing the data to the specified directory.
  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  setwd(outfolder)
  
  for (i in 1:length(noaa_data)) {
    v = noaa_data[[i]]
    flname = paste(v[[1]], v[[2]], cf_var_names[i] ,"NOAA", "Rdata", sep= ".")
    if (overwrite | !file.exists(flname)) {
      save(v, file = flname)
    } else {
      PEcAn.logger::logger.info(paste0("File ", flname, " already exists, and was not overwritten."))
      if (verbose) {
         PEcAn.logger::logger.info(paste0("The file represents ", cf_var_names[i] ," data from ", substr(v[[1]],1,4) , "-",
                                       substr(v[[1]],5,6) , "-", substr(v[[1]],8,9), ", at ", v[[2]], " hrs."))
      }
    }
  }
}