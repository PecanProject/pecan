###Comments prefixed with three #s are code statements for debugging

##' Helper function
##'@param d a POSIX Date
##'@return A representation of the date as a string with no separators
##' rnoaa::gefs requires a nate in the format YYYYMMDD instead of a POSIX date
POSIXtoGEFSDate <- function(d) {
  mon = lubridate::month(start_date)  #Extract parts of the date
  day = lubridate::month(start_date)
  if (mon < 10) { mon = paste0("0", mon) } #Add leading zeros, if necessary
  if (day < 10) { day = paste0("0", day) }
  return(paste0(lubridate::year(start_date), mon, day)) #concantanate parts of the date and return
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
##' @param include_forecast logical.  If TRUE, forecast data is tacked on to the end of the data set
##' @param ... Other arguments, currently ignored
##' @export
##' 
##' @author Luke Dramko
##' 
download.NOAA <- function(outfolder, start_date, end_date, site_id, lat, lon,
                             overwrite = FALSE, verbose = FALSE, include_forecast = FALSE, ...) {
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  
  #Exracting Date Components
  start_year <- lubridate::year(start_date)
  start_month <- lubridate::month(start_date)
  start_day <- lubridate::day(start_date)
  
  end_year <- lubridate::year(end_date)
  end_month <- lubridate::month(end_date)
  end_day <- lubridate::day(end_date)
  
  ###print(c(start_year, start_month, start_day))
  ###print(c(end_year, end_month, end_day))
  
  #Date error checking - Checks to see if the start date is before the end date
  if (start_year > end_year) {
    PEcAn.logger::logger.severe("Invalid dates: end year occurs before start year.")
  } else if (start_year == end_year) {
    if (start_month > end_month) {
      PEcAn.logger::logger.severe("Invalid dates: end month occurs before start month.")
    } else if (start_month == end_month) {
      if (start_day > end_day) {
        PEcAn.logger::logger.severe("Invalid dates: end day occurs before start day.")
      }
    }
  }
  
  #Bounds date checking
  #NOAA's GEFS database started recording data January 1, 2018.
  NOAA_GEFS_Startyr = 2008
  Current_Date = Sys.Date()
  Current_Year = lubridate::year(Current_Date)
  Current_Month = lubridate::month(Current_Date)
  Current_Day = lubridate::day(Current_Date)
  
  ##This error checking needs A LOT more work.**********************************************************************
  if (start_year < NOAA_GEFS_Startyr | end_year > Current_Year) {
    PEcAn.logger::logger.severe(sprintf('Input year range (%d:%d) exceeds the NOAA range (%d:%d)',
                                        start_year, end_year,
                                        NOAA+GEFS_Startyr, Current_Year))
  }
  
  #Date error checking complete.
  
  #Set up the data frame with file information.  This data frame will be returned and stored in the BETY database to 
  #locate the files later.
  file_info <- data.frame(
    file = character(rows),
    host = character(rows),
    mimetype = character(rows),
    formatname = character(rows),
    startdate = character(rows),
    enddate = character(rows),
    dbfile.name =                   #paste("GFDL", model, scenario, ensemble_member, sep = "."),   # 'GFDL',
    stringsAsFactors = FALSE
  )
  
  
  #NOAA variable downloading
  
  #We want data for each of the following variables. Here, we're just getting the raw data; later, we will convert it to the 
  #cf standard format when relevant.
  noaa_var_names = list("Temperature_height_above_ground_ens", "Pressure_surface_ens", "Relative_humidity_height_above_ground_ens", "Downward_Long-Wave_Radp_Flux_surface_6_Hour_Average_ens", 
                   "Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average_ens", "Total_precipitation_surface_6_Hour_Accumulation_ens",
                   "u-component_of_wind_height_above_ground_ens", "v-component_of_wind_height_above_ground_ens")
  
  #These are the cf standard names
  cf_var_names = list("air_temperature", "air_pressure", "specific_humidity", "surface_downwelling_longwave_flux_in_air", 
                      "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "eastward_wind", "northward_wind")
  
  #cf_var_names and noaa_var_names are parelell arrays
  
  # This debugging loop allows you to check if the cf variables are correctly mapped to the equivalent
  # NOAA variable names.  This is very important, as much of the processing below will be erroneous if 
  # these fail to match up.
  #for (i in 1:length(cf_var_names)) {
  #  print(sprintf("cf / noaa   :   %s / %s", cf_var_names[[i]], noaa_var_names[[i]]))
  #}
  
  get_date = POSIXtoGEFS(start_date)
  
  noaa_data = list()
  
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
   
  print("Done.")
  
}