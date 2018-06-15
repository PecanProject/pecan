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
##' This function returns a positive value if the first date is greater than the second, zero if the 
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
  NOAA_GEFS_Start_Date = as.POSIXlt(Sys.Date()) - lubridate::days(12)
  
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
  ###cf_var_names = c("air_temperature")


  ###May want to delete this
  #Creating a data frame to store all of the information about the variables:
  #var_info = data.frame(
  #  noaa_var_names,
  #  cf_var_names
  #)
  
  
  # This debugging loop allows you to check if the cf variables are correctly mapped to the equivalent
  # NOAA variable names.  This is very important, as much of the processing below will be erroneous if 
  # these fail to match up.
  #for (i in 1:length(cf_var_names)) {
  #  print(sprintf("cf / noaa   :   %s / %s", cf_var_names[[i]], noaa_var_names[[i]]))
  #}
  
  noaa_data = list()
  
  #Setting up the loops.  num_days represents the length of time in days betwen the start and end of the data being collected.
  num_days = lubridate::time_length(lubridate::interval(start_date, end_date), unit="day")
  request_time = Sys.time()
  
  #If the requested forecast data ends on the current day, we have to be careful about the time of day we ask
  #for data.  Data is posted every 6 hours.  If this function is called at 9:00 a.m., it wouldn't make sense
  #to request data for 12 a.m. or 6:00 p.m. - the data doesn't yet exist.
  ends_today = FALSE
  if (as.POSIXlt(Sys.Date()) == end_date) {
    print(sprintf("Entered: first if statment.  Dates are %s and %s.", as.POSIXlt(Sys.Date()), end_date))
    ends_today = TRUE
    #The last day will be handled in its own special block of code...
  } else {
    num_days = num_days + 1 # ...otherwise, we need to do it in the main loop.
  }
  
  print(sprintf("num_days = %d", num_days))
  
  #This is where the actual download of the data occurs.
  i = 1
  max = length(cf_var_names)
  while (i <= max && as.logical(num_days)) {
    ###Debugging
    print(sprintf("while loop: i = %d", i))
    print(sprintf("date = %s", start_date))
    current_date = start_date #Incremented in the loop to fetch the proper day's data
    
    gefs_date = POSIX.to.GEFSDate(current_date)
    
    var_dat = matrix(rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0000", date=gefs_date)$data,
                     nrow=21, ncol=1)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1200", date=gefs_date)$data)
    var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1800", date=gefs_date)$data)
    j = 1
    while (j < num_days) {
      current_date = current_date + lubridate::days(1)  #increments the date by one
      
      ###Debugging
      print(sprintf("    while loop: j = %d; date = %s", j, current_date))
      
      gefs_date = POSIX.to.GEFSDate(current_date)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0000", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1200", date=gefs_date)$data)
      var_dat = cbind(var_dat, rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1800", date=gefs_date)$data)
      j = j + 1   
    }
    noaa_data[[i]] = var_dat
    i = i+1
  }
  
  print("Finished main loop.  Moving on to the special cases.")
  
  last_data_set_at = "1800"
  
  if (ends_today) {
    print("Entered ends_today")
    for (i in 1:length(cf_var_names)) {
      gefs_date = POSIX.to.GEFSDate(end_date)
      last_data_set_at = "0000"
      hour = lubridate::hour(as.POSIXlt(Sys.time()))
      continue = TRUE
      
      print(sprintf("hour = %d", hour))
      
      if (length(noaa_data) != 0) { #The data matrix already exists (it was created in the loop above), and we just have to add to it.
        noaa_data[[i]] = cbind(noaa_data[[i]], rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0000", date=gefs_date)$data)
      } else { #we have to make the data matrix and put it in the array.
        noaa_data[[i]] = matrix(rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0000", date=gefs_date)$data,
                           nrow=21, ncol=1)
      }
      
      print(sprintf("variable = %s", cf_var_names[i]))
      print("Got through hour == 0")
      
      #The "hour > 6" statement is an attempt to reconcile the current time with the GEFS data should become avaliable.  However, this
      #is not assured, and so the tryCatch block exists to handle cases when data are not yet avaliable.
      if (hour > 6 && continue) {
        noaa_data[[i]] = tryCatch({
          last_data_set_at = "0600"
           cbind(noaa_data[[i]], rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "0600", date=gefs_date)$data)
        }, error=function(e){
          continue=FALSE
          PEcAn.logger::logger.info(paste0("Last data avaliable for ", Sys.Date(), " is the 12:00 midnight data point (0:00)."))
          noaa_data
          })
      } 
      
      print("Got through hour == 6")
      
      if (hour > 12 && continue) {
        noaa_data[[i]] = tryCatch({
          last_data_set_at = "1200"
          cbind(noaa_data[[i]], rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1200", date=gefs_date)$data)
        }, error=function(e){
          continue=FALSE
          PEcAn.logger::logger.info(paste0("Last data avaliable for ", Sys.Date(), " is the 6:00 a.m. data point."))
          noaa_data
          })
      }
      
      print("Got through hour == 12")
      
      if (hour > 18 && continue) {
        noaa_data[[i]] = tryCatch({
          last_data_set_at = "1800"
          cbind(noaa_data[[i]], rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1, forecast_time = "1800", date=gefs_date)$data)
        }, error=function(e){
          continue=FALSE
          PEcAn.logger::logger.info(paste0("Last data avaliable for ", Sys.Date(), " is the 12:00 noon data point."))
          noaa_data
          })
      }
      
      print("Got through hour == 18")
    }
  }
  
  print(noaa_data)
  
  
  if (include_forecast) {
    ###debugging
    print("Entered include_forecast")
    
    for (i in 1:length(cf_var_names)) {
      print(sprintf("Line 259: i = %d, var = %s", i, cf_var_names[i]))
      noaa_data[[i]] = cbind(noaa_data[[i]], rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 2:64, forecast_time = last_data_set_at, date=gefs_date)$data)
    }
  }
  
  print("Done.")
  
  print(noaa_data)
  print("***************************************************************")
  
  ###################################################
  # Not all NOAA data units match the cf data standard.  In this next section, data are processed to
  # confirm with the standard when necessary.
  # The following is a list of variables which need to be processed:
  # 1. NOAA's relative humidity must be converted to specific humidity
  # 2. NOAA's measure of precipitation is the accumulation over 6 hours; cf's standard is precipitation per second
  
  #Convert NOAA's relative humidity to specific humidity
  humid_index = cf_var_names == "specific_humidity"
  
  humid_data = noaa_data[humid_index][[1]]
  temperature_data = noaa_data[cf_var_names == "air_temperature"][[1]]
  pressure_data = noaa_data[cf_var_names == "air_pressure"][[1]]
  
  ###print("***************** relative humidity ***********************")
  ###print(humid_data)
  ### print("****************************************")
  ### print(temperature_data)
  ### print("****************************************")
  ### print(pressure_data)
   
  #Depending on the volume and dimensions of data you download, sometimes R stores it as a vector and sometimes
  #as a matrix; the different cases must be processed with different loops.
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
  } else {  PEcAn.logger::logger.severe("Error: Humidity data saved in an invalid format!") }
  
  #Update the noaa_data list with the correct data
  noaa_data[humid_index][[1]] <- humid_data
  
  # Convert NOAA's total precipitation (kg m-2) to precipitation flux (kg m-2 s-1)
  #NOAA precipitation data is an accumulation over 6 hours.
  precip_index = cf_var_names == "precipitation_flux"
  
  noaa_data[precip_index][[1]] = noaa_data[precip_index][[1]] / 21600  #There are 21600 seconds in 6 hours
  
  #############################################
  # Done with data processing.  Now writing the data to the specified directory.
  if (!dir.exists(outfolder)) {
    dir.create(outfolder, recursive=TRUE)
  }
  
  setwd(outfolder)
  
  #Generate file name
  styr = lubridate::year(start_year)
  edyr = lubridate::year(end_year)
  yrcomponent = styr
  if (styr != endyr) {
    yrcomponent = styr, 
  }
  
  
  quit(save = "no", status = 0)  ###Currently here to keep the function from running past this point.  For debugging only.
  
  for (i in 1:length(noaa_data)) {
    v = noaa_data[[i]]
    flname = paste(NOAA.GE, v[[2]], cf_var_names[i] ,"NOAA", "Rdata", sep= ".")
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