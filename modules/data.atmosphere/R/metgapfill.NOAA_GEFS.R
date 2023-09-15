##'@title Gapfill NOAA_GEFS weather data
##'@section Purpose:
##'This function uses simple methods to gapfill NOAA GEFS met data
##'Temperature and Precipitation are gapfilled with spline; other data sources are gapfilled with
##'using linear models fitted to other fitted data.
##'
##'@param in.prefix the met file name
##'@param in.path The location of the file
##'@param outfolder The place to write the output file to
##'@param start_date The start date of the contents of the file
##'@param end_date The end date of the contents of the file
##'@param overwrite Whether or not to overwrite the output file if it exists or not
##'@param verbose Passed to nc writing functions for additional output
##'@export
##'
##'@author Luke Dramko
metgapfill.NOAA_GEFS <- function(in.prefix, in.path, outfolder, start_date, end_date,
                                 overwrite = FALSE, verbose = FALSE, ...) {
  
  PEcAn.logger::logger.info("Starting metgapfill.NOAA_GEFS")
  
  # These are the variables cf NOAA_GEFS uses
 # cf_var_names = c("air_temperature", "air_pressure", "specific_humidity", "surface_downwelling_longwave_flux_in_air", 
                   #"surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "eastward_wind", "northward_wind")
  
  # Variables whose gapfillings are not directly dependent on splines.
  #dependent_vars <- c("specific_humidity", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air",
                     # "air_pressure", "eastward_wind", "northward_wind")
  
  
  escaped <- gsub("(\\W)", "\\\\\\1", in.prefix) # The file name may contain special characters that could mess up the regular expression.
  matching_files <- grep(escaped, list.files(in.path), value=TRUE) 
  if (length(matching_files) == 0) {
    PEcAn.logger::logger.severe(paste0("No files found matching ", in.prefix, "; cannot process data."))
  }
  
  # This function is supposed to process netcdf files, so we'll search for files the the extension .nc and use those first.
  nc_file = grep("\\.nc$", matching_files)
  if (length(nc_file) > 0) {
    in.prefix <- matching_files[1]
  } else { # no .nc files found... it could be that the extension was left off, or some other problem
    PEcAn.logger::logger.warn("No files found with extension '.nc'.  Using the first file in the list below:")
    PEcAn.logger::logger.warn(matching_files)
    in.prefix <- matching_files[1]
  }
  
  # Attach the path.  The above procedure doesn't require the path, but acutally opening the file does.
  full.data.file <- file.path(in.path, in.prefix)
  
  if (!file.exists(full.data.file)) {
    PEcAn.logger::logger.warn(paste0("File ", full.data.file, " not found.  Unable to perform gapfilling."))
    return(data.frame())
  }
  
  flptr = ncdf4::nc_open(full.data.file)
  
  cf_var_names = names(flptr$var)   #to deal with wind speed vs eastward vs northward
  dependent_vars = cf_var_names[-which(cf_var_names == "air_temperature" | cf_var_names == "precipitation_flux")] #remove air temp and pres since they are gap filled differently
  # Put data into a matrix
  var <- ncdf4::ncvar_get(flptr, "air_temperature")
  allvars <- matrix(var, ncol=length(var), nrow=1)
  var <- ncdf4::ncvar_get(flptr, "precipitation_flux")
  allvars = rbind(allvars, var)
  
  for (i in 1:length(dependent_vars)) {
    allvars <- rbind(allvars, ncdf4::ncvar_get(flptr, dependent_vars[i]))
  }
  
  # Use this matrix to fill in missing days at the end of the forecast with data from the
  # same time of day.
  # First, count how far back needs going.
  k <- ncol(allvars)
  while (length(which(is.na(allvars[,k]))) > 0) {
    k = k - 1;
  }
  
  # i = column, row = j
  for (i in 1:nrow(allvars)) {
    for (j in k:ncol(allvars)) {
      if (is.na(allvars[i,j])) {
        allvars[i,j] = sample(stats::na.omit(allvars[i,seq(j, 1, by = -4)]), 1)
      }
    }
  }
  
  # Use a basic spline to fill in missing values for basic variables
  # Other variables will be fit to these for internal consistency in gapfilling
  air_temperature <- allvars[1,]
  precipitation_flux <- allvars[2,]
  
  air_temperature <- zoo::na.spline(air_temperature)
  precipitation_flux <- zoo::na.spline(precipitation_flux)
  
  fitted.data <- data.frame(air_temperature = air_temperature,
                            precipitation_flux = precipitation_flux)
  time <-flptr$dim$time$vals
  
  # This loop does the gapfilling of the other variables, based on air_temperature and precipitation_flux.
  # It does so in the following way:
  # A linear model for a variabe (e.g. specific humidity) is fitted to temperature and precipitation
  # A prediction is made using the predict function on what the values of the missing variables
  # should be.
  # The values that were missing in the original data are filled in with their corresponding 
  # values in the output of the prediciton function.
  # The new data is put into the data frame used to fit the next model
  for (i in 1:length(dependent_vars)) {
    var <- allvars[i+2,]
    if(is.na(var[1])) {
      var[1] <- mean(var, na.rm = TRUE)
    }
    
    fitted.data[[dependent_vars[i]]] = var
    
    # Unfortunately, R is picky, and the data.frame[['var_as_string']] notation doesn't work
    # for the lm function; only the $ notation does, hence this if/else if section.
    if (dependent_vars[i] == "specific_humidity") {
      reg <- stats::lm(fitted.data$specific_humidity ~.,fitted.data)
    } else if (dependent_vars[i] == "surface_downwelling_longwave_flux_in_air") {
      reg <- stats::lm(fitted.data$surface_downwelling_longwave_flux_in_air ~.,fitted.data)
    } else if (dependent_vars[i] == "surface_downwelling_shortwave_flux_in_air") {
      reg <- stats::lm(fitted.data$surface_downwelling_shortwave_flux_in_air ~.,fitted.data)
    } else if (dependent_vars[i] == "air_pressure") {
      reg <- stats::lm(fitted.data$air_pressure ~.,fitted.data)
    } else if (dependent_vars[i] == "eastward_wind") {
      reg <- stats::lm(fitted.data$eastward_wind ~.,fitted.data)
    } else if (dependent_vars[i] == "northward_wind") {
      reg <- stats::lm(fitted.data$northward_wind ~.,fitted.data)
    }else if (dependent_vars[i] == "wind_speed") {
      reg <- stats::lm(fitted.data$wind_speed ~.,fitted.data)
    }
  
    prediction <- stats::predict(reg, fitted.data)
    
    # Update the values in the data frame
    for (j in 1:length(prediction)) {
      if(is.na(fitted.data[[dependent_vars[i]]][j])) {
        fitted.data[[dependent_vars[i]]][j] <- prediction[j]
      }
    }
  }
  
  # Extract ensemble information from file name
  ensemble <- regmatches(in.prefix, regexpr("NOAA_GEFS\\.[^.]*\\.[0-9]*", in.prefix))
  ensemble <- regmatches(ensemble, regexpr("[0-9]+$", ensemble))
  
  # Each ensemble gets its own folder to keep things organized
  out.data.file <- file.path(outfolder, paste0("NOAA_GEFS.", ensemble))
  if (!dir.exists(out.data.file)) {
    dir.create(out.data.file, recursive=TRUE, showWarnings = FALSE)
  }
  
  # The file names are the same, but the data is in a different directory.
  out.data.file <- file.path(out.data.file, in.prefix)
  
  # Write new, gapfilled file
  if (!file.exists(out.data.file) || overwrite) {
    # Setup netcdf dimensions and variables
    # All variables should be of the same length.
    time_dim = ncdf4::ncdim_def(name="time", 
                                paste(units="hours since", start_date), 
                                time,
                                create_dimvar = TRUE)
    lat <- ncdf4::ncvar_get(nc = flptr, varid = "latitude")
    lon <- ncdf4::ncvar_get(nc = flptr, varid = "longitude")
    lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
    lon_dim = ncdf4::ncdim_def("longitude", "degree_east", lon, create_dimvar = TRUE)
    
    dimensions_list = list(time_dim, lat_dim, lon_dim)
    
    nc_var_list = list()
    for (i in 1:length(cf_var_names)) {
      units <- flptr$var[[cf_var_names[i]]]$units
      nc_var_list[[i]] = ncdf4::ncvar_def(cf_var_names[i], units, dimensions_list, missval=NaN)
    }
    
    # Open file
    nc_flptr = ncdf4::nc_create(out.data.file, nc_var_list, verbose=verbose)
    
    # Write data to file
    for (j in 1:length(cf_var_names)) {
      ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], fitted.data[[cf_var_names[j]]])
    }
    
    # Close file
    ncdf4::nc_close(nc_flptr)
  } else {
    PEcAn.logger::logger.info(paste0("File ", out.data.file, " already exists.  It was not overwritten."))
  }
  
  # We no longer need the original file
  ncdf4::nc_close(flptr)
  
  # This table of results is used to insert the record of the file into the database.
  results <- data.frame(file = out.data.file,                         # file name
                        host = PEcAn.remote::fqdn(),                  # machine where file is located
                        mimetype = "application/x-netcdf",            # type of file
                        formatname = "CF (gapfilled)",                # file format
                        startdate = start_date,                       # start date of file contents
                        enddate = end_date,                           # end date of file contents
                        dbfile.name = basename(out.data.file),        # output file name
                        stringsAsFactors = FALSE)
  
  return(results)
  
} # metgapfill.NOAA_GEFS
