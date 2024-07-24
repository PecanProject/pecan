##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.STICS
##' @title Write STICS met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param start_date start date of the simulation
##' @param end_date end date of the simulation
##' @param overwrite logical: replace output files if they already exist?
##' @param ... other arguments passed to function
##' @return results 
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
met2model.STICS <- function(in.path, in.prefix, outfolder, start_date, end_date,
                            overwrite = FALSE, ...) {
  
  PEcAn.logger::logger.info("START met2model.STICS")
  
  start_date  <- as.POSIXlt(start_date, tz = "UTC")
  end_date    <- as.POSIXlt(end_date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  
  # starting with the easiest case, full years
  # STICS looks for different input files for each year
  out.files      <- paste(in.prefix, seq(start_year, end_year), "climate", sep = ".")
  out.files.full <- file.path(outfolder, out.files)
  
  results <- data.frame(file = out.files.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/plain",
                        formatname = "climate",
                        startdate = start_date, # these need fixing,not same for all climate files
                        enddate = end_date,  # these need fixing
                        dbfile.name = out.files,
                        stringsAsFactors = FALSE)
  PEcAn.logger::logger.info("internal results")
  PEcAn.logger::logger.info(results)
  
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  ctr <- 1
  
  ## loop over files/years
  for (year in seq(start_year, end_year)) {
    
    if (file.exists(out.files.full[ctr]) && !overwrite) {
      PEcAn.logger::logger.debug("File '", out.files.full[ctr], "' already exists, skipping to next file.")
      ctr <- ctr + 1
      next
    }
    
    PEcAn.logger::logger.info(year)
    
    ## handle dates, also for partial year(s)
    if(year == start_year & year != end_year){
      # start year could be full or partial
      simdays <- seq(lubridate::yday(start_date), PEcAn.utils::days_in_year(year))
    }else if(year == end_year & year != start_year){
      # end year could be full or partial
      simdays <- seq(1, lubridate::yday(end_date))
    }else if(year == end_year & year == start_year){
      # we have one full or partial year
      simdays <- seq(lubridate::yday(start_date), lubridate::yday(end_date))
    }else{
      # a full year in between
      simdays <- seq(1, PEcAn.utils::days_in_year(year))
    }
    
    
    NDAYS      <- length(simdays)
    NWEATHER   <- as.integer(13)
    weather_df <- as.data.frame(matrix( -999.9, nrow = NDAYS, ncol = NWEATHER))
    
    # prepare data frame for STICS format, daily inputs, but doesn't have to be full year
    weather_df[ ,1] <- rep(gsub(".*_STICS_site_", "", outfolder), NDAYS) # column 1: name of weather file
    weather_df[ ,2] <- rep(year, NDAYS) # column 2: year
    start_month <- ifelse(year == start_year, paste0(start_date), paste0(year, "/01/01"))
    end_month   <- ifelse(year == end_year,   paste0(end_date),   paste0(year, "/12/31"))

    weather_df[ ,3] <- lubridate::month(seq(lubridate::as_date(start_month), 
                                            lubridate::as_date(end_month), by = "day")) # column 3: month
    
    weather_df[ ,4] <- lubridate::mday(seq(lubridate::as_date(start_month), 
                          lubridate::as_date(end_month), by = "day")) # column 4: day in month
    weather_df[ ,5] <- simdays # column 5: Julian day
    
    ## handle variables
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    if (file.exists(old.file)) {
      ## open netcdf
      nc <- ncdf4::nc_open(old.file) 
      on.exit(ncdf4::nc_close(nc), add = TRUE)
      
      ## convert time to seconds
      sec <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
      
      dt <- diff(sec)[1]
      tstep <- round(86400 / dt)
      dt <- 86400 / tstep
      
      ind <- rep(simdays, each = tstep)
      
      if(unlist(strsplit(nc$dim$time$units, " "))[1] %in% c("days", "day")){
        #this should always be the case, but just in case
        origin_dt <- as.POSIXct(unlist(strsplit(nc$dim$time$units, " "))[3], "%Y-%m-%d", tz="UTC")
        ydays <- lubridate::yday(origin_dt + sec)
        
      }else{
        PEcAn.logger::logger.error("Check units of time in the weather data.")
      }
      
      # column 6: minimum temperature (°C)
      Tair   <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
      Tair   <- Tair[ydays %in% simdays]
      Tair_C <- PEcAn.utils::ud_convert(Tair, "K", "degC")
      t_dmin <- round(tapply(Tair_C, ind, min, na.rm = TRUE), digits = 2) # maybe round these numbers 
      weather_df[ ,6] <- t_dmin
        
      # column 7: maximum temperature (°C)
      t_dmax <- round(tapply(Tair_C, ind, max, na.rm = TRUE), digits = 2) # maybe round these numbers 
      weather_df[ ,7] <- t_dmax
      
      # column 8: global radiation (MJ m-2. j-1)
      rad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
      gr  <- rad * 0.0864 # W m-2 to MJ m-2 d-1
      gr  <- gr[ydays %in% simdays] 
      weather_df[ ,8] <- round(tapply(gr, ind, mean, na.rm = TRUE), digits = 2) # irradiation (MJ m-2 d-1)
      
      # column 9: Penman PET (mm.j-1) OPTIONAL, leave it as -999.9 for now
      
      # column 10: rainfall (mm.j-1)
      Rain  <- ncdf4::ncvar_get(nc, "precipitation_flux") # kg m-2 s-1
      Rain  <- Rain[ydays %in% simdays]
      raini <- tapply(Rain * 86400, ind, mean, na.rm = TRUE) 
      weather_df[ ,10] <- round(raini, digits = 2) # precipitation (mm d-1)	
      
      # column 11: wind (m.s-1) 
      # OPTIONAL if you're not using the “Shuttleworth and Wallace” method or the “Penman calculate” method to calculate PET in the station file
      U <- try(ncdf4::ncvar_get(nc, "eastward_wind"))
      V <- try(ncdf4::ncvar_get(nc, "northward_wind"))
      if(is.numeric(U) & is.numeric(V) & !all(is.nan(U)) & !all(is.nan(V))){
        U  <- U[ydays %in% simdays]
        V  <- V[ydays %in% simdays]
        ws <- sqrt(U ^ 2 + V ^ 2)      
      }else{
        ws <- try(ncdf4::ncvar_get(nc, "wind_speed"))
        ws <- ws[ydays %in% simdays]
        if (is.numeric(ws)) {
          PEcAn.logger::logger.info("eastward_wind and northward_wind absent; using wind_speed")
        }else{
          PEcAn.logger::logger.severe("No variable found to calculate wind_speed")
        }
      }
      weather_df[ ,11] <- round(tapply(ws, ind, mean,  na.rm = TRUE), digits = 2) # mean wind speed (m s-1)
      
      # column 12: vapour pressure (mbars), leave it as -999.9 for now
      # OPTIONAL if you're not using the “Shuttleworth and Wallace” method or the “Penman calculate” method to calculate PET in the station file
      
      # column 13: CO2 content(ppm). 
      co2 <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))
      co2 <- co2[ydays %in% simdays]
      if(is.numeric(co2)){
        weather_df[ ,13] <- round(tapply(co2 * 1e6, ind, mean,  na.rm = TRUE), digits = 1)
      }else{
        # default : 330 ppm
        weather_df[ ,13] <- 330
        PEcAn.logger::logger.info("mole_fraction_of_carbon_dioxide_in_air absent; using default 330 ppm")
      }
      
    }else{
      PEcAn.logger::logger.severe(old.file, " does not exist.")
    }

    utils::write.table(weather_df, file = out.files.full[ctr], col.names = FALSE, row.names = FALSE)
    ctr <- ctr + 1
    
  } ## end-loop over files

  return(invisible(results))
  
} # met2model.STICS
