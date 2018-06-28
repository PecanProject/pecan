##' @title Download NOAA GEFS Weather Data
##' 
##' @section Information on Units
##' Information on NOAA weather units can be found here:
##' @references https://www.ncdc.noaa.gov/crn/measurements.html
##' Note that the temperature is measured in degrees C, but is converted at the station and downlaoded
##' in Kelvin.
##' 
##' @section NOAA_GEFS General Information
##' This function downloads NOAA GEFS weather data.  GEFS is an ensemble of 21 different weather forecast models.  A 16 day forecast is avaliable
##' every 6 hours.  Each forecast includes information on a total of 8 variables.  These are transformed from the NOAA standard to the internal PEcAn
##' standard.
##' 
##' @section Data Avaliability
##' NOAA GEFS weather data is avaliable on a rolling 12 day basis; dates provided in "start_date" must be within this range. The end date can be any point after
##' that, but if the end date is beyond 16 days, only 16 days worth of forecast are recorded.  Times are rounded down to the previous 6 hour forecast.  NOAA
##' GEFS weather data isn't always posted immediately, and to compensate, this function adjusts requests made in the last two hours
##' back two hours (approximately the amount of time it takes to post the data) to make sure the most current forecast is used.
##' 
##' @section Data Save Format
##' Data is saved in the netcdf format to the specified directory.  File names reflect the precision of the data to the given range of days.
##' NOAA.GEFS.willow creek.3.2018-06-08T06:00.to.2018-06-24T06:00.nc specifies the forecast, using ensemble nubmer 3 at willow creek on
##' June 6th, 2018 at 6:00 a.m. to June 24th, 2018 at 6:00 a.m.
##' 
##' @return A list of data frames is returned containing information about the data file that can be used to locate it later.  Each
##' data frame contains information about one file.
##'
##' Download and convert to CF NOAA weather data
##' @param outfolder Directory where results should be written
##' @param start_date, end_date Range of dates/times to be downloaded (default assumed time of day is 0:00, midnight)
##' @param lat site latitude in decimal degrees
##' @param lon site longitude in decimal degrees
##' @param sitename The unique ID given to each site. This is used as part of the file name.
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param verbose logical.  Print additional debug information.  Passed on to functions in the netcdf4 package to provide debugging info.
##' @param ... Other arguments, currently ignored
##' @export
##' 
##' @examples 
##' \dontrun{
##'  download.NOAA_GEFS(outfolder="~/Working/results", lat.in= 45.805925, lon.in = -90.07961, sitename="US-WCr")
##' }
##' 
##' @author Luke Dramko
##' 
download.NOAA_GEFS <- function(outfolder, lat.in, lon.in, sitename, start_date = Sys.time(), end_date = (as.POSIXct(start_date, tz="UTC") + lubridate::days(16)),
                             overwrite = FALSE, verbose = FALSE, ...) {
  
  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date <- as.POSIXct(end_date, tz = "UTC")
  
  #It takes about 2 hours for NOAA GEFS weather data to be posted.  Therefore, if a request is made within that 2 hour window,
  #we instead want to adjust the start time to the previous forecast, which is the most recent one avaliable.  (For example, if
  #there's a request at 7:00 a.m., the data isn't up yet, so the function grabs the data at midnight instead.)
  if (abs(as.numeric(Sys.time() - start_date, units="hours")) <= 2) {
    start_date = start_date - lubridate::hours(2)
    end_date = end_date - lubridate::hours(2)
  }
  
  #Date/time error checking - Checks to see if the start date is before the end date
  if (start_date > end_date) {
    PEcAn.logger::logger.severe("Invalid dates: end date occurs before start date")
  } else if (as.numeric(end_date - start_date, units="hours") < 6) { #Done separately to produce a more helpful error message.
    PEcAn.logger::logger.severe("Times not far enough appart for a forecast to fall between them.  Forecasts occur every six hours; make sure start 
                                and end dates are at least 6 hours appart.")
  }
  
  #Set the end forecast date (default is the full 16 days)
  if (end_date > start_date + lubridate::days(16)) {
    end_date = start_date + lubridate::days(16)
  }
  
  #Round the starting date/time down to the previous block of 6 hours.  Adjust the time frame to match.
  forecast_hour = (lubridate::hour(start_date) %/% 6) * 6 #Integer division by 6 followed by re-multiplication acts like a "floor function" for multiples of 6
  increments = as.integer(as.numeric(end_date - start_date, units = "hours") / 6) #Calculating the number of forecasts between start and end dates.
  increments = increments + ((lubridate::hour(end_date) - lubridate::hour(start_date)) %/% 6) #These calculations are required to use the rnoaa package.
  
  end_hour = sprintf("%04d", ((forecast_hour + (increments * 6)) %% 24) * 100)  #Calculating the starting hour as a string, which is required type to access the 
                                                                        #data via the rnoaa package
  forecast_hour = sprintf("%04d", forecast_hour * 100)  #Having the end date as a string is useful later, too.
  
  #Recreate the adjusted start and end dates.
  start_date = as.POSIXct(paste0(lubridate::year(start_date), "-", lubridate::month(start_date), "-", lubridate::day(start_date), " ", 
                                 substring(forecast_hour, 1,2), ":00:00"), tz="UTC")
  end_date = start_date + lubridate::hours(increments * 6)
  
  #Bounds date checking
  #NOAA's GEFS database maintains a rolling 12 days of forecast data for access through this function.
  #We do want Sys.Date() here - NOAA makes data unavaliable days at a time, not forecasts at a time.
  NOAA_GEFS_Start_Date = as.POSIXct(Sys.Date(), tz="UTC") - lubridate::days(11)  #Subtracting 11 days is correct, not 12.
  
  #Check to see if start_date is valid. This must be done after date adjustment.
  if (as.POSIXct(Sys.time(), tz="UTC") < start_date || start_date < NOAA_GEFS_Start_Date) {
    PEcAn.logger::logger.severe(sprintf('Start date (%s) exceeds the NOAA GEFS range (%s to %s).',
                                        start_date,
                                        NOAA_GEFS_Start_Date, Sys.Date()))
  }
  
  if (lubridate::hour(start_date) > 23) {
    PEcAn.logger::logger.severe(sprintf("Start time %s is not a valid time", lubridate::hour(start_date)))
  }
  
  if (lubridate::hour(end_date) > 23) {  #Done separately from the previous if statement in order to have more specific error messages.
    PEcAn.logger::logger.severe(sprintf("End time %s is not a valid time", lubridate::hour(end_date)))
  }
  #End date/time error checking
  
  #################################################
  #NOAA variable downloading
  #Uses the rnoaa package to download data
  
  #We want data for each of the following variables. Here, we're just getting the raw data; later, we will convert it to the 
  #cf standard format when relevant.
  noaa_var_names = c("Temperature_height_above_ground_ens", "Pressure_surface_ens", "Relative_humidity_height_above_ground_ens", "Downward_Long-Wave_Radp_Flux_surface_6_Hour_Average_ens", 
                   "Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average_ens", "Total_precipitation_surface_6_Hour_Accumulation_ens",
                   "u-component_of_wind_height_above_ground_ens", "v-component_of_wind_height_above_ground_ens")
  
  #These are the cf standard names
  cf_var_names = c("air_temperature", "air_pressure", "specific_humidity", "surface_downwelling_longwave_flux_in_air", 
                      "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "eastward_wind", "northward_wind")
  cf_var_units = c("K", "Pa", "1", "Wm-2", "Wm-2", "kgm-2s-1", "ms-1", "ms-1")  #Negative numbers indicate negative exponents
  
  # This debugging loop allows you to check if the cf variables are correctly mapped to the equivalent
  # NOAA variable names.  This is very important, as much of the processing below will be erroneous if 
  # these fail to match up.
  # for (i in 1:length(cf_var_names)) {
  #  print(sprintf("cf / noaa   :   %s / %s", cf_var_names[[i]], noaa_var_names[[i]]))
  #}
  
  noaa_data = list()
  
  #Downloading the data here.  It is stored in a matrix, where columns represent time in intervals of 6 hours, and rows represent
  #each ensemble member.  Each variable gets its own matrix, which is stored in the list noaa_data.
  for (i in 1:length(noaa_var_names)) {
    noaa_data[[i]] = rnoaa::gefs(noaa_var_names[i], lat.in, lon.in, raw=TRUE, time_idx = 1:increments, forecast_time = forecast_hour, date=format(start_date, "%Y%m%d"))$data
  }
  
  ###################################################
  # Not all NOAA data units match the cf data standard.  In this next section, data are processed to
  # confirm with the standard when necessary.
  # The following is a list of variables which need to be processed:
  # 1. NOAA's relative humidity must be converted to specific humidity
  # 2. NOAA's measure of precipitation is the accumulation over 6 hours; cf's standard is precipitation per second

  #Convert NOAA's relative humidity to specific humidity
  humid_index = which(cf_var_names == "specific_humidity")
  
  #Temperature, pressure, and relative humidity are required to calculate specific humidity.
  humid_data = noaa_data[[humid_index]]
  temperature_data = noaa_data[[which(cf_var_names == "air_temperature")]]
  pressure_data = noaa_data[[which(cf_var_names == "air_pressure")]]
   
  #Depending on the volume and dimensions of data you download, sometimes R stores it as a vector and sometimes
  #as a matrix; the different cases must be processed with different loops.
  #(The specific corner case in which a vector would be generated is if only one hour is requested; for example, 
  #only the data at time_idx 1, for example).
  if (as.logical(nrow(humid_data))) {
     for (i in 1:length(humid_data)) {
       humid_data[i] = PEcAn.data.atmosphere::rh2qair(humid_data[i], temperature_data[i], pressure_data[i])
     }
  } else {
     for (i in 1:nrow(humid_data)) {
       for (j in 1:ncol(humid_data)) {
          humid_data[i,j] = PEcAn.data.atmosphere::rh2qair(humid_data[i,j], temperature_data[i,j], pressure_data[i,j])
        }
      }
  }
  
  #Update the noaa_data list with the correct data
  noaa_data[[humid_index]] <- humid_data
  
  # Convert NOAA's total precipitation (kg m-2) to precipitation flux (kg m-2 s-1)
  #NOAA precipitation data is an accumulation over 6 hours.
  precip_index = which(cf_var_names == "precipitation_flux")
  
  #The statement udunits2::ud.convert(1, "kg m-2 6 hr-1", "kg m-2 s-1") is equivalent to udunits2::ud.convert(1, "kg m-2 hr-1", "kg m-2 s-1") * 6,
  #which is a little unintuitive. What will do the conversion we want is what's below:
  noaa_data[[precip_index]] = udunits2::ud.convert(noaa_data[[precip_index]], "kg m-2 hr-1", "kg m-2 6 s-1")  #There are 21600 seconds in 6 hours
  
  #############################################
  # Done with data processing.  Now writing the data to the specified directory. Each ensemble member is written to its own file, for a total
  # of 21 files.  
  if (!dir.exists(outfolder)) {
    dir.create(outfolder, recursive=TRUE, showWarnings = FALSE)
  }
  
  # Create a data frame with information about the file.  This data frame's format is an internal PEcAn standard, and is stored in the BETY database to
  # locate the data file.  The data file is stored on the local machine where the download occured.  Because NOAA GEFS is an 
  # ensemble of 21 different forecast models, each model gets its own data frame.  All of the information is the same for 
  # each file except for the file name.
  results = data.frame(
    file = "",                            #Path to the file (added in loop below).
    host = PEcAn.remote::fqdn(),          #Name of the server where the file is stored
    mimetype = "application/x-netcdf",    #Format the data is saved in
    formatname = "CF Meteorology",        #Type of data
    startdate = paste0(format(start_date, "%Y-%m-%dT%H:%M:00")),    #starting date and time, down to the second
    enddate = paste0(format(end_date, "%Y-%m-%dT%H:%M:00")),        #ending date and time, down to the second
    dbfile.name = "NOAA_GEFS",            #Source of data
    stringsAsFactors = FALSE
  )
  
  results_list = list()
  
  #Each ensemble gets its own file.
  #These dimensions will be used for all 21 ncdf4 file members, so they're all declared once here.
  #The data is really one-dimensional for each file (though we include lattitude and longitude dimensions
  #to comply with the PEcAn standard).
  time_dim = ncdf4::ncdim_def("Time", "6 Hours", 1:ncol(noaa_data[[1]]))
  lat_dim = ncdf4::ncdim_def("latitude", "Degrees North", lat.in)
  lon_dim = ncdf4::ncdim_def("longitude", "Degrees East", lon.in)
  
  dimensions_list = list(time_dim, lat_dim, lon_dim)
  
  nc_var_list = list()
  for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
    nc_var_list[[i]] = ncdf4::ncvar_def(cf_var_names[i], cf_var_units[i], dimensions_list, missval=NaN)
  }
  
  #For each ensemble
  for (i in 1:21) { # i is the ensemble number
    #Generating a unique identifier string that characterizes a particular data set.
    identifier = paste("NOAA_GEFS", sitename, i, format(start_date, "%Y-%m-%dT%H:%M"), 
          format(end_date, "%Y-%m-%dT%H:%M"), sep=".")
    
    ensemble_folder = file.path(outfolder, identifier)
    
    #Each file will go in its own folder.
    if (!dir.exists(ensemble_folder)) {
      dir.create(ensemble_folder, recursive=TRUE, showWarnings = FALSE)
    }
    
    flname = file.path(ensemble_folder, paste(identifier, "nc", sep = "."))
    
    #Each ensemble member gets its own unique data frame, which is stored in results_list
    #Object references in R work differently than in other languages. When adding an item to a list, R creates a copy of it
    #for you instead of just inserting the object reference, so this works.
    results$file = flname
    results_list[[i]] = results
    
    if (!file.exists(flname) | overwrite) {
      nc_flptr = ncdf4::nc_create(flname, nc_var_list, verbose=verbose)
      
      #For each variable associated with that ensemble
      for (j in 1:length(cf_var_names)) {
        # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
        ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], noaa_data[[j]][i,])
      }
      
      ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
    } else {
      PEcAn.logger::logger.info(paste0("The file ", flname, " already exists.  It was not overwritten."))
    }
    
  }
  
  return(results_list)
} #download.NOAA_GEFS
