#' Download gridded forecast in the box bounded by the latitude and longitude list
#'
#' @param lat_list lat for site
#' @param lon_list long for site
#' @param forecast_time start hour of forecast
#' @param forecast_date date for forecast
#' @param model_name_raw model name for directory creation
#' @param end_hr end hr to determine how many hours to download
#' @param output_directory output directory
#' 
#' @export
#'
#' @return NA
#'

noaa_grid_download <- function(lat_list, lon_list, forecast_time, forecast_date, model_name_raw, output_directory, end_hr) {
  
  
  download_grid <- function(ens_index, location, directory, hours_char, cycle, base_filename1, vars,working_directory){
    #for(j in 1:31){
    if(ens_index == 1){
      base_filename2 <- paste0("gec00",".t",cycle,"z.pgrb2a.0p50.f")
      curr_hours <- hours_char[hours <= 384]
    }else{
      if((ens_index-1) < 10){
        ens_name <- paste0("0",ens_index - 1)
      }else{
        ens_name <- as.character(ens_index -1)
      }
      base_filename2 <- paste0("gep",ens_name,".t",cycle,"z.pgrb2a.0p50.f")
      curr_hours <- hours_char
    }
    
    
    for(i in 1:length(curr_hours)){
      file_name <- paste0(base_filename2, curr_hours[i])
      
      destfile <- paste0(working_directory,"/", file_name,".grib")
      
      if(file.exists(destfile)){
        
        fsz <- file.info(destfile)$size
        gribf <- file(destfile, "rb")
        fsz4 <- fsz-4
        seek(gribf,where = fsz4,origin = "start")
        last4 <- readBin(gribf,"raw",4)
        if(as.integer(last4[1])==55 & as.integer(last4[2])==55 & as.integer(last4[3])==55 & as.integer(last4[4])==55) {
          download_file <- FALSE
        } else {
          download_file <- TRUE
        }
        close(gribf)
        
      }else{
        download_file <- TRUE
      }
      
      if(download_file){
        
        out <- tryCatch(utils::download.file(paste0(base_filename1, file_name, vars, location, directory),
                                             destfile = destfile, quiet = TRUE),
                        error = function(e){
                          warning(paste(e$message, "skipping", file_name),
                                  call. = FALSE)
                          return(NA)
                        },
                        finally = NULL)
        
        if(is.na(out)) next
      }
    }
  }
  
  model_dir <- file.path(output_directory, model_name_raw)
  
  curr_time <- lubridate::with_tz(Sys.time(), tzone = "UTC")
  curr_date <- lubridate::as_date(curr_time)
  
  noaa_page <- readLines('https://nomads.ncep.noaa.gov/pub/data/nccf/com/gens/prod/')
  
  potential_dates <- NULL
  for(i in 1:length(noaa_page)){
    if(stringr::str_detect(noaa_page[i], ">gefs.")){
      end <- stringr::str_locate(noaa_page[i], ">gefs.")[2]
      dates <- stringr::str_sub(noaa_page[i], start = end+1, end = end+8)
      potential_dates <- c(potential_dates, dates)
    }
  }
  
  
  last_cycle_page <- readLines(paste0('https://nomads.ncep.noaa.gov/pub/data/nccf/com/gens/prod/gefs.', dplyr::last(potential_dates)))
  
  potential_cycle <- NULL
  for(i in 1:length(last_cycle_page)){
    if(stringr::str_detect(last_cycle_page[i], 'href=\"')){
      end <- stringr::str_locate(last_cycle_page[i], 'href=\"')[2]
      cycles <- stringr::str_sub(last_cycle_page[i], start = end+1, end = end+2)
      if(cycles %in% c("00","06", "12", "18")){
        potential_cycle <- c(potential_cycle, cycles)
      }
    }
  }
  
  potential_dates <- lubridate::as_date(potential_dates)
  
  potential_dates = potential_dates[which(potential_dates == forecast_date)]
  
  if(length(potential_dates) == 0){PEcAn.logger::logger.error("Forecast Date not available")}
  
  
  location <- paste0("&subregion=&leftlon=",
                     floor(min(lon_list)),
                     "&rightlon=",
                     ceiling(max(lon_list)),
                     "&toplat=",
                     ceiling(max(lat_list)),
                     "&bottomlat=",
                     floor(min(lat_list)))
  
  base_filename1 <- "https://nomads.ncep.noaa.gov/cgi-bin/filter_gefs_atmos_0p50a.pl?file="
  vars <- "&lev_10_m_above_ground=on&lev_2_m_above_ground=on&lev_surface=on&lev_entire_atmosphere=on&var_APCP=on&var_DLWRF=on&var_DSWRF=on&var_PRES=on&var_RH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_TCDC=on"
  
  for(i in 1:length(potential_dates)){
    
    forecast_date <- lubridate::as_date(potential_dates[i])
    forecast_hours = as.numeric(forecast_time) 
    
    
    for(j in 1:length(forecast_hours)){
      cycle <- forecast_hours[j]
      
      if(cycle < 10) cycle <- paste0("0",cycle)
      
      model_date_hour_dir <- file.path(model_dir,forecast_date,cycle)
      if(!dir.exists(model_date_hour_dir)){
        dir.create(model_date_hour_dir, recursive=TRUE, showWarnings = FALSE)
      }
      
      new_download <- TRUE
      
      if(new_download){
        
        print(paste("Downloading", forecast_date, cycle))
        
        if(cycle == "00"){
          hours <- c(seq(0, 240, 3),seq(246, 384, 6))
          hours <- hours[hours<=end_hr]
        }else{
          hours <- c(seq(0, 240, 3),seq(246, min(end_hr, 840) , 6))
        }
        hours_char <- hours
        hours_char[which(hours < 100)] <- paste0("0",hours[which(hours < 100)])
        hours_char[which(hours < 10)] <- paste0("0",hours_char[which(hours < 10)])
        curr_year <- lubridate::year(forecast_date)
        curr_month <- lubridate::month(forecast_date)
        if(curr_month < 10) curr_month <- paste0("0",curr_month)
        curr_day <- lubridate::day(forecast_date)
        if(curr_day < 10) curr_day <- paste0("0",curr_day)
        curr_date <- paste0(curr_year,curr_month,curr_day)
        directory <- paste0("&dir=%2Fgefs.",curr_date,"%2F",cycle,"%2Fatmos%2Fpgrb2ap5")
        
        ens_index <- 1:31
        
        parallel::mclapply(X = ens_index,
                           FUN = download_grid,
                           location,
                           directory,
                           hours_char,
                           cycle,
                           base_filename1,
                           vars,
                           working_directory = model_date_hour_dir,
                           mc.cores = 1)
      }else{
        print(paste("Existing", forecast_date, cycle))
      }
    }
  }
}
#' Extract and temporally downscale points from downloaded grid files
#'
#' @param lat_list lat for site
#' @param lon_list lon for site
#' @param site_id Unique site_id for file creation
#' @param downscale Logical. Default is TRUE. Downscales from 6hr to hourly
#' @param overwrite Logical. Default is FALSE. Should exisiting files be overwritten
#' @param forecast_date Date for download
#' @param forecast_time Time (0,6,12,18) for start of download 
#' @param model_name Name of model for file name
#' @param model_name_ds Name of downscale file name
#' @param model_name_raw Name of raw file name
#' @param output_directory Output directory 
#' @importFrom rlang .data 
#' 
#' @export
#' @return List
#'
#'
process_gridded_noaa_download <- function(lat_list,
                                          lon_list,
                                          site_id,
                                          downscale,
                                          overwrite,
                                          forecast_date,
                                          forecast_time,
                                          model_name,
                                          model_name_ds,
                                          model_name_raw,
                                          output_directory){
  #binding variables 
  NOAA.member <- NULL
  extract_sites <- function(ens_index, hours_char, hours, cycle, site_id, lat_list, lon_list, working_directory){
    
    site_length <- length(site_id)
    tmp2m <- array(NA, dim = c(site_length, length(hours_char)))
    rh2m <- array(NA, dim = c(site_length, length(hours_char)))
    ugrd10m <- array(NA, dim = c(site_length,length(hours_char)))
    vgrd10m <- array(NA, dim = c(site_length, length(hours_char)))
    pressfc <- array(NA, dim = c(site_length, length(hours_char)))
    apcpsfc <- array(NA, dim = c(site_length, length(hours_char)))
    tcdcclm <- array(NA, dim = c(site_length, length(hours_char)))
    dlwrfsfc <- array(NA, dim = c(site_length, length(hours_char)))
    dswrfsfc <- array(NA, dim = c(site_length, length(hours_char)))
    
    if(ens_index == 1){
      base_filename2 <- paste0("gec00",".t",cycle,"z.pgrb2a.0p50.f")
    }else{
      if(ens_index-1 < 10){
        ens_name <- paste0("0",ens_index-1)
      }else{
        ens_name <- as.character(ens_index-1)
      }
      base_filename2 <- paste0("gep",ens_name,".t",cycle,"z.pgrb2a.0p50.f")
    }
    
    lats <- round(lat_list/.5)*.5
    lons <- round(lon_list/.5)*.5
    
    if(lons < 0){
      lons <- 360 + lons
    }
    curr_hours <- hours_char
    
    for(hr in 1:length(curr_hours)){
      file_name <- paste0(base_filename2, curr_hours[hr])
      grib_file_name <- paste0(working_directory,"/", file_name,".grib")
      
      if(file.exists(grib_file_name)){
        grib_data <- terra::rast(grib_file_name)
        
        ## Convert to data frame
        grib_data_df <- terra::as.data.frame(grib_data, xy=TRUE)
        lat_lon <- grib_data_df[, c("x", "y")]
        
        for(s in 1:length(site_id)){
          
          index <- which(lat_lon[,2] == lats[s] & lat_lon[,1] == lons[s])
          
          pressfc[s, hr]  <- grib_data_df$`SFC=Ground or water surface; Pressure [Pa]`[index]
          tmp2m[s, hr]    <- grib_data_df$`2[m] HTGL=Specified height level above ground; Temperature [C]`[index]
          rh2m[s, hr]     <- grib_data_df$`2[m] HTGL=Specified height level above ground; Relative humidity [%]`[index]
          ugrd10m[s, hr]  <- grib_data_df$`10[m] HTGL=Specified height level above ground; u-component of wind [m/s]`[index]
          vgrd10m[s, hr]  <- grib_data_df$`10[m] HTGL=Specified height level above ground; v-component of wind [m/s]`[index]
          
          if(curr_hours[hr] != "000"){
            apcpsfc[s, hr]  <- grib_data_df$`SFC=Ground or water surface; 03 hr Total precipitation [kg/(m^2)]`[index]
            tcdcclm[s, hr]  <- grib_data_df$`RESERVED(10) (Reserved); Total cloud cover [%]`[index]
            dswrfsfc[s, hr] <- grib_data_df$`SFC=Ground or water surface; Downward Short-Wave Rad. Flux [W/(m^2)]`[index]
            dlwrfsfc[s, hr] <- grib_data_df$`SFC=Ground or water surface; Downward Long-Wave Rad. Flux [W/(m^2)]`[index]
          }
        }
      }
    }
    
    return(list(tmp2m = tmp2m,
                pressfc = pressfc,
                rh2m = rh2m,
                dlwrfsfc = dlwrfsfc,
                dswrfsfc = dswrfsfc,
                ugrd10m = ugrd10m,
                vgrd10m = vgrd10m,
                apcpsfc = apcpsfc,
                tcdcclm = tcdcclm))
  }
  
  noaa_var_names <- c("tmp2m", "pressfc", "rh2m", "dlwrfsfc",
                      "dswrfsfc", "apcpsfc",
                      "ugrd10m", "vgrd10m", "tcdcclm")
  
  
  model_dir <- file.path(output_directory)
  model_name_raw_dir <- file.path(output_directory, model_name_raw)
  
  curr_time <- lubridate::with_tz(Sys.time(), tzone = "UTC")
  curr_date <- lubridate::as_date(curr_time)
  potential_dates <- seq(curr_date - lubridate::days(6), curr_date, by = "1 day")
  
  #Remove dates before the new GEFS system
  potential_dates <- potential_dates[which(potential_dates > lubridate::as_date("2020-09-23"))]
  
  
  
  
  cycle <-forecast_time
  curr_forecast_time <- forecast_date + lubridate::hours(cycle)
  if(cycle < 10) cycle <- paste0("0",cycle)
  if(cycle == "00"){
    hours <- c(seq(0, 240, 3),seq(246, 840 , 6))
  }else{
    hours <- c(seq(0, 240, 3),seq(246, 384 , 6))
  }
  hours_char <- hours
  hours_char[which(hours < 100)] <- paste0("0",hours[which(hours < 100)])
  hours_char[which(hours < 10)] <- paste0("0",hours_char[which(hours < 10)])
  
  raw_files <- list.files(file.path(model_name_raw_dir,forecast_date,cycle))
  hours_present <- as.numeric(stringr::str_sub(raw_files, start = 25, end = 27))
  
  all_downloaded <- TRUE
  # if(cycle == "00"){
  #   #Sometime the 16-35 day forecast is not competed for some of the forecasts.  If over 24 hrs has passed then they won't show up.
  #   #Go ahead and create the netcdf files
  #   if(length(which(hours_present == 840)) == 30 | (length(which(hours_present == 384)) == 30 & curr_forecast_time + lubridate::hours(24) < curr_time)){
  #     all_downloaded <- TRUE
  #   }
  # }else{
  #   if(length(which(hours_present == 384)) == 31 | (length(which(hours_present == 384)) == 31 & curr_forecast_time + lubridate::hours(24) < curr_time)){
  #     all_downloaded <- TRUE
  #   }
  # }
  
  
  
  
  
  if(all_downloaded){
    
    ens_index <- 1:31
    #Run download_downscale_site() over the site_index
    output <- parallel::mclapply(X = ens_index,
                                 FUN = extract_sites,
                                 hours_char = hours_char,
                                 hours = hours,
                                 cycle,
                                 site_id,
                                 lat_list,
                                 lon_list,
                                 working_directory = file.path(model_name_raw_dir,forecast_date,cycle),
                                 mc.cores = 1)
    
    
    forecast_times <- lubridate::as_datetime(forecast_date) + lubridate::hours(as.numeric(cycle)) + lubridate::hours(as.numeric(hours_char))
    
    
    
    #Convert negetive longitudes to degrees east
    if(lon_list < 0){
      lon_east <- 360 + lon_list
    }else{
      lon_east <- lon_list
    }
    
    model_site_date_hour_dir <- file.path(model_dir, site_id, forecast_date,cycle)
    
    if(!dir.exists(model_site_date_hour_dir)){
      dir.create(model_site_date_hour_dir, recursive=TRUE, showWarnings = FALSE)
    }else{
      unlink(list.files(model_site_date_hour_dir, full.names = TRUE))
    }
    
    if(downscale){
      modelds_site_date_hour_dir <- file.path(output_directory,model_name_ds,site_id, forecast_date,cycle)
      if(!dir.exists(modelds_site_date_hour_dir)){
        dir.create(modelds_site_date_hour_dir, recursive=TRUE, showWarnings = FALSE)
      }else{
        unlink(list.files(modelds_site_date_hour_dir, full.names = TRUE))
      }
    }
    
    noaa_data <- list()
    
    for(v in 1:length(noaa_var_names)){
      
      value <- NULL
      ensembles <- NULL
      forecast.date <- NULL
      
      noaa_data[v] <- NULL
      
      for(ens in 1:31){
        curr_ens <- output[[ens]]
        value <- c(value, curr_ens[[noaa_var_names[v]]][1, ])
        ensembles <- c(ensembles, rep(ens, length(curr_ens[[noaa_var_names[v]]][1, ])))
        forecast.date <- c(forecast.date, forecast_times)
      }
      noaa_data[[v]] <- list(value = value,
                             ensembles = ensembles,
                             forecast.date = lubridate::as_datetime(forecast.date))
      
    }
    
    #These are the cf standard names
    cf_var_names <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air",
                      "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "eastward_wind", "northward_wind","cloud_area_fraction")
    
    #Replace "eastward_wind" and "northward_wind" with "wind_speed"
    cf_var_names1 <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air",
                       "surface_downwelling_shortwave_flux_in_air", "precipitation_flux","specific_humidity", "cloud_area_fraction","wind_speed")
    
    cf_var_units1 <- c("K", "Pa", "1", "Wm-2", "Wm-2", "kgm-2s-1", "1", "1", "ms-1")  #Negative numbers indicate negative exponents
    
    names(noaa_data) <- cf_var_names
    
    specific_humidity <- rep(NA, length(noaa_data$relative_humidity$value))
    
    noaa_data$relative_humidity$value <- noaa_data$relative_humidity$value / 100
    
    noaa_data$air_temperature$value <- noaa_data$air_temperature$value + 273.15
    
    specific_humidity[which(!is.na(noaa_data$relative_humidity$value))] <- PEcAn.data.atmosphere::rh2qair(rh = noaa_data$relative_humidity$value[which(!is.na(noaa_data$relative_humidity$value))],
                                                                                                          T = noaa_data$air_temperature$value[which(!is.na(noaa_data$relative_humidity$value))],
                                                                                                          press = noaa_data$air_pressure$value[which(!is.na(noaa_data$relative_humidity$value))])
    
    
    #Calculate wind speed from east and north components
    wind_speed <- sqrt(noaa_data$eastward_wind$value^2 + noaa_data$northward_wind$value^2)
    
    forecast_noaa <- tibble::tibble(time = noaa_data$air_temperature$forecast.date,
                                    NOAA.member = noaa_data$air_temperature$ensembles,
                                    air_temperature = noaa_data$air_temperature$value,
                                    air_pressure= noaa_data$air_pressure$value,
                                    relative_humidity = noaa_data$relative_humidity$value,
                                    surface_downwelling_longwave_flux_in_air = noaa_data$surface_downwelling_longwave_flux_in_air$value,
                                    surface_downwelling_shortwave_flux_in_air = noaa_data$surface_downwelling_shortwave_flux_in_air$value,
                                    precipitation_flux = noaa_data$precipitation_flux$value,
                                    specific_humidity = specific_humidity,
                                    cloud_area_fraction = noaa_data$cloud_area_fraction$value,
                                    wind_speed = wind_speed)
    
    forecast_noaa$cloud_area_fraction <- forecast_noaa$cloud_area_fraction / 100 #Convert from % to proportion
    
    # Convert the 3 hr precip rate to per second.
    forecast_noaa$precipitation_flux <- forecast_noaa$precipitation_flux / (60 * 60 * 3)
    
    
    
    # Create a data frame with information about the file.  This data frame's format is an internal PEcAn standard, and is stored in the BETY database to
    # locate the data file.  The data file is stored on the local machine where the download occured.  Because NOAA GEFS is an 
    # ensemble of 21 different forecast models, each model gets its own data frame.  All of the information is the same for 
    # each file except for the file name.
    
    results_list = list()
    
    
    for (ens in 1:31) { # i is the ensemble number
      
      #Turn the ensemble number into a string
      if(ens-1< 10){
        ens_name <- paste0("0",ens-1)
      }else{
        ens_name <- ens - 1
      }
      
      forecast_noaa_ens <- forecast_noaa %>%
        dplyr::filter(NOAA.member == ens) %>%
        dplyr::filter(!is.na(.data$air_temperature))
      
      end_date <- forecast_noaa_ens %>%
        dplyr::summarise(max_time = max(.data$time))
      
      results = data.frame(
        file = "",                            #Path to the file (added in loop below).
        host = PEcAn.remote::fqdn(),          #Name of the server where the file is stored
        mimetype = "application/x-netcdf",    #Format the data is saved in
        formatname = "CF Meteorology",        #Type of data
        startdate = paste0(format(forecast_date, "%Y-%m-%dT%H:%M:00")),    #starting date and time, down to the second
        enddate = paste0(format(end_date$max_time, "%Y-%m-%dT%H:%M:00")),        #ending date and time, down to the second
        dbfile.name = "NOAA_GEFS_downscale",            #Source of data (ensemble number will be added later)
        stringsAsFactors = FALSE
      )
      
      identifier = paste("NOAA_GEFS", site_id, ens_name, format(forecast_date, "%Y-%m-%dT%H:%M"), 
                         format(end_date$max_time, "%Y-%m-%dT%H:%M"), sep="_")     
      
      fname <- paste0(identifier, ".nc")
      ensemble_folder = file.path(output_directory, identifier)
      output_file <- file.path(ensemble_folder,fname)
      
      if (!dir.exists(ensemble_folder)) {
        dir.create(ensemble_folder, recursive=TRUE, showWarnings = FALSE)} 
      
      
      #Write netCDF
      if(!nrow(forecast_noaa_ens) == 0){      
        write_noaa_gefs_netcdf(df = forecast_noaa_ens,ens, lat = lat_list[1], lon = lon_east, cf_units = cf_var_units1, output_file = output_file, overwrite = TRUE)
      }else {results_list[[ens]] <- NULL 
      next}
      
      if(downscale){
        #Downscale the forecast from 6hr to 1hr
        
        
        identifier_ds = paste("NOAA_GEFS_downscale", site_id, ens_name, format(forecast_date, "%Y-%m-%dT%H:%M"), 
                              format(end_date$max_time, "%Y-%m-%dT%H:%M"), sep="_")
        
        fname_ds <- paste0(identifier_ds, ".nc")
        ensemble_folder_ds = file.path(output_directory, identifier_ds)
        output_file_ds <- file.path(ensemble_folder_ds,fname_ds)
        
        if (!dir.exists(ensemble_folder_ds)) {
          dir.create(ensemble_folder_ds, recursive=TRUE, showWarnings = FALSE)} 
        
        results$file = output_file_ds
        results$dbfile.name = fname_ds
        results_list[[ens]] <- results
        
        #Run downscaling
        temporal_downscale_half_hour(input_file = output_file, output_file = output_file_ds, overwrite = TRUE, hr = 1)
      }
      
      
    }
  }
  results_list <- results_list[!sapply(results_list, is.null)]
  return(results_list)
} #process_gridded_noaa_download

#' @title Downscale NOAA GEFS from 6hr to 1hr
#' @return None
#'
#' @param input_file, full path to 6hr file
#' @param output_file, full path to 1hr file that will be generated
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param hr time step in hours of temporal downscaling (default = 1)
#' @importFrom rlang .data 
#' 
#' @author Quinn Thomas
#'
#'

temporal_downscale <- function(input_file, output_file, overwrite = TRUE, hr = 1){
  
  # open netcdf
  nc <- ncdf4::nc_open(input_file)
  
  if(stringr::str_detect(input_file, "ens")){
    ens_postion <- stringr::str_locate(input_file, "ens")
    ens_name <- stringr::str_sub(input_file, start = ens_postion[1], end = ens_postion[2] + 2)
    ens <- as.numeric(stringr::str_sub(input_file, start = ens_postion[2] + 1, end = ens_postion[2] + 2))
  }else{
    ens <- 0
    ens_name <- "ens00"
  }
  
  # retrive variable names
  cf_var_names <- names(nc$var)
  
  # generate time vector
  time <- ncdf4::ncvar_get(nc, "time")
  begining_time <- lubridate::ymd_hm(ncdf4::ncatt_get(nc, "time",
                                                      attname = "units")$value)
  time <- begining_time + lubridate::hours(time)
  
  # retrive lat and lon
  lat.in <- ncdf4::ncvar_get(nc, "latitude")
  lon.in <- ncdf4::ncvar_get(nc, "longitude")
  
  # generate data frame from netcdf variables and retrive units
  noaa_data <- tibble::tibble(time = time)
  var_units <- rep(NA, length(cf_var_names))
  for(i in 1:length(cf_var_names)){
    curr_data <- ncdf4::ncvar_get(nc, cf_var_names[i])
    noaa_data <- cbind(noaa_data, curr_data)
    var_units[i] <- ncdf4::ncatt_get(nc, cf_var_names[i], attname = "units")$value
  }
  
  ncdf4::nc_close(nc)
  
  names(noaa_data) <- c("time",cf_var_names)
  
  # spline-based downscaling
  if(length(which(c("air_temperature", "wind_speed","specific_humidity", "air_pressure") %in% cf_var_names) == 4)){
    forecast_noaa_ds <- downscale_spline_to_hrly(df = noaa_data, VarNames = c("air_temperature", "wind_speed","specific_humidity", "air_pressure"))
  }else{
    #Add error message
  }
  
  # Convert splined SH, temperature, and presssure to RH
  forecast_noaa_ds <- forecast_noaa_ds %>%
    dplyr::mutate(relative_humidity = qair2rh(qair = forecast_noaa_ds$specific_humidity,
                                              temp = forecast_noaa_ds$air_temperature,
                                              press = forecast_noaa_ds$air_pressure)) %>%
    dplyr::mutate(relative_humidity = .data$relative_humidity,
                  relative_humidity = ifelse(.data$relative_humidity > 1, 0, .data$relative_humidity))
  
  # convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  if("surface_downwelling_longwave_flux_in_air" %in% cf_var_names){
    LW.flux.hrly <- downscale_repeat_6hr_to_hrly(df = noaa_data, varName = "surface_downwelling_longwave_flux_in_air")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, LW.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # convert precipitation to hourly (just copy 6 hourly values over past 6-hour time period)
  if("surface_downwelling_longwave_flux_in_air" %in% cf_var_names){
    Precip.flux.hrly <- downscale_repeat_6hr_to_hrly(df = noaa_data, varName = "precipitation_flux")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, Precip.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # convert cloud_area_fraction to hourly (just copy 6 hourly values over past 6-hour time period)
  if("cloud_area_fraction" %in% cf_var_names){
    cloud_area_fraction.flux.hrly <- downscale_repeat_6hr_to_hrly(df = noaa_data, varName = "cloud_area_fraction")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, cloud_area_fraction.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # use solar geometry to convert shortwave from 6 hr to 1 hr
  if("surface_downwelling_shortwave_flux_in_air" %in% cf_var_names){
    ShortWave.hrly <- downscale_ShortWave_to_hrly(df = noaa_data, lat = lat.in, lon = lon.in)
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, ShortWave.hrly, by = "time")
  }else{
    #Add error message
  }
  
  #Add dummy ensemble number to work with write_noaa_gefs_netcdf()
  forecast_noaa_ds$NOAA.member <- ens
  
  #Make sure var names are in correct order
  forecast_noaa_ds <- forecast_noaa_ds %>%
    dplyr::select("time", tidyselect::all_of(cf_var_names), "NOAA.member")
  
  #Write netCDF
  write_noaa_gefs_netcdf(df = forecast_noaa_ds,
                         ens = ens,
                         lat = lat.in,
                         lon = lon.in,
                         cf_units = var_units,
                         output_file = output_file,
                         overwrite = overwrite)
  
} #temporal_downscale



##' @title Write NOAA GEFS netCDF
##' @name write_noaa_gefs_netcdf
##' @param df data frame of meterological variables to be written to netcdf.  Columns
##' must start with time with the following columns in the order of `cf_units`
##' @param ens ensemble index used for subsetting df
##' @param lat latitude in degree north
##' @param lon longitude in degree east
##' @param cf_units vector of variable names in order they appear in df
##' @param output_file name, with full path, of the netcdf file that is generated
##' @param overwrite logical to overwrite existing netcdf file
##' 
##' @return NA
##' @export
##' 
##' @author Quinn Thomas
##'
##'

write_noaa_gefs_netcdf <- function(df, ens = NA, lat, lon, cf_units, output_file, overwrite){
  
  if(!is.na(ens)){
    data <- df
    max_index <- max(which(!is.na(data$air_temperature)))
    start_time <- min(data$time)
    end_time <- data$time[max_index]
    
    data <- data %>% dplyr::select(-c("time", "NOAA.member"))
  }else{
    data <- df
    max_index <- max(which(!is.na(data$air_temperature)))
    start_time <- min(data$time)
    end_time <- data$time[max_index]
    
    data <- df %>%
      dplyr::select(-c("time"))
  }
  
  diff_time <- as.numeric(difftime(df$time, df$time[1])) / (60 * 60)
  
  cf_var_names <- names(data)
  
  time_dim <- ncdf4::ncdim_def(name="time",
                               units = paste("hours since", format(start_time, "%Y-%m-%d %H:%M")),
                               diff_time, #GEFS forecast starts 6 hours from start time
                               create_dimvar = TRUE)
  lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
  lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", lon, create_dimvar = TRUE)
  
  dimensions_list <- list(time_dim, lat_dim, lon_dim)
  
  nc_var_list <- list()
  for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
    nc_var_list[[i]] <- ncdf4::ncvar_def(cf_var_names[i], cf_units[i], dimensions_list, missval=NaN)
  }
  
  if (!file.exists(output_file) | overwrite) {
    nc_flptr <- ncdf4::nc_create(output_file, nc_var_list, verbose = FALSE)
    
    #For each variable associated with that ensemble
    for (j in 1:ncol(data)) {
      # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
      ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], unlist(data[,j]))
    }
    
    ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
  }
}