
## R Code to convert NetCDF CF met files into MAAT model met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

# leaf_user_met prefix
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"

##-------------------------------------------------------------------------------------------------#
##' met2model wrapper for MAAT
##'
##' @name met2model.MAAT
##' @title Create MAAT met driver files
##' @param in.path location on disk where inputs (CF met drivers) are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where MAAT met outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param leap_year Enforce Leap-years? If set to TRUE, will require leap years to have 366 days. 
##' If set to false, will require all years to have 365 days. Default = TRUE.
##' @param ... additional arguments, currently ignored
##' @export
##' @author Shawn P. Serbin
##'
met2model.MAAT <- function(in.path, in.prefix, outfolder, start_date, end_date,
                           overwrite = FALSE, verbose = FALSE, leap_year = TRUE, ...) {

  PEcAn.logger::logger.info("START met2model.MAAT")

  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")

  out.file <- paste(in.prefix,
                    strptime(start_date, "%Y-%m-%d"),
                    strptime(end_date, "%Y-%m-%d"),
                    "csv",
                    sep = ".")
  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/csv",
                        formatname = "MAAT meteorology",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)

  if (file.exists(out.file.full) && !overwrite) {
    PEcAn.logger::logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }

  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  out <- NULL

  # get start/end year since inputs are specified on year basis
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  day_secs <- PEcAn.utils::ud_convert(1, "day", "seconds")
  
  ## loop over files
  ## TODO need to filter out the data that is not inside start_date, end_date
  for (year in start_year:end_year) {

    PEcAn.logger::logger.info(paste0("Processing year: ",year))
    ncdf.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))

    if (file.exists(ncdf.file)) {
      ## open netcdf
      nc <- ncdf4::nc_open(ncdf.file)

      ## convert time to seconds
      sec <- nc$dim$time$vals
      frac.day <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")

      dt <- PEcAn.utils::seconds_in_year(year, leap_year) / length(sec)

      tstep <- round(day_secs / dt)
      dt    <- day_secs / tstep

      ### extract required MAAT driver variables names(nc$var)
      lat  <- ncdf4::ncvar_get(nc, "latitude")
      lon  <- ncdf4::ncvar_get(nc, "longitude")
      
      # Air temperature
      Tair <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
      Tair_C <- PEcAn.utils::ud_convert(Tair, "K", "degC") ## in degC
      
      # Precipitation
      Rain <- ncdf4::ncvar_get(nc, "precipitation_flux")  ## 'kg/m^2/s'
      
      # Get atmospheric pressure
      Atm_press <- ncdf4::ncvar_get(nc,"air_pressure") ## in Pa

      # get humidity vars
      RH_perc <- try(ncdf4::ncvar_get(nc, "relative_humidity"), silent = TRUE)  ## RH Percentage
      Qair <- try(ncdf4::ncvar_get(nc, "specific_humidity"), silent = TRUE)  #humidity (kg/kg)
      SVP <- PEcAn.utils::ud_convert(PEcAn.data.atmosphere::get.es(Tair_C), "millibar", "Pa")  ## Saturation vapor pressure
      VPD <- try(ncdf4::ncvar_get(nc, "water_vapor_saturation_deficit"), silent = TRUE)  ## in Pa
      if (!is.numeric(VPD)) {
        VPD <- SVP * (1 - PEcAn.data.atmosphere::qair2rh(Qair, Tair_C))
        PEcAn.logger::logger.info("water_vapor_saturation_deficit absent; VPD calculated from Qair, Tair, and SVP (saturation vapor pressure) ")
      }
      VPD_kPa <- PEcAn.utils::ud_convert(VPD, "Pa", "kPa")
      e_a <- SVP - VPD  # AirVP
      if (!is.numeric(RH_perc)) {
         RH_perc <- PEcAn.data.atmosphere::qair2rh(Qair, Tair_C,Atm_press)
      }

      # get windspeed
      ws <- try(ncdf4::ncvar_get(nc, "wind_speed"), silent = TRUE)
      if (!is.numeric(ws) | length(unique(ws)) == 1) {
        U <- ncdf4::ncvar_get(nc, "eastward_wind")
        V <- ncdf4::ncvar_get(nc, "northward_wind")
        ws <- sqrt(U ^ 2 + V ^ 2) ## m/s
        PEcAn.logger::logger.info("wind_speed absent; calculated from eastward_wind and northward_wind")
      }
      
      # get radiation
      SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## in W/m2
      PAR <- try(ncdf4::ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air") * 1e+06, silent = TRUE)  ## mol/m2/s to umols/m2/s
      if (!is.numeric(PAR)) {
        PAR <- SW * 2.114  #W/m2 TO umol/m2/s
      }

      # get CO2 (if exists)
      CO2 <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"), silent = TRUE)
      useCO2 <- is.numeric(CO2)
      if (useCO2) {
        CO2 <- CO2 * 1e+06  ## convert from mole fraction (kg/kg) to ppm
      }

      ncdf4::nc_close(nc)
      
    } else {
      print("Skipping to next year")
      next
    }

    ## build time variables (year, month, day of year)
    skip <- FALSE
    nyr  <- floor(length(sec) * dt / 86400 / 365)
    yr   <- NULL
    doy  <- NULL
    hr   <- NULL
    asec <- sec
    for (y in seq(year, year + nyr - 1)) {
      diy <- PEcAn.utils::days_in_year(y, leap_year)
      ytmp <- rep(y, PEcAn.utils::ud_convert(diy / dt, "days", "seconds"))
      dtmp <- rep(seq_len(diy), each = day_secs / dt)
      if (is.null(yr)) {
        yr  <- ytmp
        doy <- dtmp
        hr  <- rep(NA, length(dtmp))
      } else {
        yr  <- c(yr, ytmp)
        doy <- c(doy, dtmp)
        hr  <- c(hr, rep(NA, length(dtmp)))
      }
      rng <- length(doy) - length(ytmp):1 + 1
      if (!all(rng >= 0)) {
        skip <- TRUE
        PEcAn.logger::logger.warn(paste(year, "is not a complete year and will not be included"))
        break
      }
      asec[rng] <- asec[rng] - asec[rng[1]]
      hr[rng]   <- (asec[rng] - (dtmp - 1) * day_secs) / day_secs * 24
    }

    # Time output variable
    time <- as.POSIXct(asec, tz = "UTC", origin = paste0(year,"-01-01"))

    # output matrix
    n <- length(Tair)
    if (useCO2) {
      tmp <- cbind.data.frame(Time = time[1:n],
                              Year = yr[1:n],
                              DOY = doy[1:n],
                              Hour = hr[1:n],
                              Frac_day = frac.day[1:n],
                              Timestep_frac = rep(dt/day_secs, n),
                              CO2 = CO2,
                              Tair_degC = Tair_C,
                              Prec_mm = Rain * dt,  # converts from mm/s to mm umols/m2/s
                              Atm_press_Pa = Atm_press,
                              RH_perc = RH_perc,
                              VPD_kPa = VPD_kPa,
                              PAR_umols_m2_s = PAR,
                              Windspeed_m_s = ws)
    } else {
      tmp <- cbind.data.frame(Time = time[1:n],
                              Year = yr[1:n],
                              DOY = doy[1:n],
                              Hour = hr[1:n],
                              Frac_day = frac.day[1:n],
                              Timestep_frac = rep(dt/day_secs, n),
                              Tair_degC = Tair_C,
                              Prec_mm = Rain * dt,  # converts from mm/s to mm umols/m2/s
                              Atm_press_Pa = Atm_press,
                              RH_perc = RH_perc,
                              VPD_kPa = VPD_kPa,
                              PAR_umols_m2_s = PAR,
                              Windspeed_m_s = ws)
    }

    ## quick error check, sometimes get a NA in the last hr ?? NEEDED?
    hr.na <- which(is.na(tmp[, 3]))
    if (length(hr.na) > 0) {
      tmp[hr.na, 3] <- tmp[hr.na - 1, 3] + dt/day_secs * 24
    }

    if (is.null(out)) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
  }  ## end loop over years

  if (!is.null(out)) {

    ## write met csv output
    # write.table(out,out.file.full,quote = FALSE,sep='\t',row.names=FALSE,col.names=FALSE)
    utils::write.csv(out, out.file.full, row.names = FALSE)

    # write out leaf_user_met.xml - example
    #<met_data_translator>
    #<leaf>
    #<env>
    #<par>'PAR_umols_m2_s'</par>
    #<temp>'Tair_degC'</temp>
    #<vpd>'VPD_kPa'</vpd>
    #<atm_press>'Atm_press_Pa'</atm_press>
    #</env>
    #</leaf>
    #</met_data_translator>

    # Create leaf_user_met.xml
    if (useCO2) {
      leaf_user_met_list <- list(leaf = list(env = list(time = "'Time'", temp = "'Tair_degC'", par = "'PAR_umols_m2_s'",vpd="'VPD_kPa'",
                                                        atm_press="'Atm_press_Pa'",ca_conc="'CO2'",wind="'Windspeed_m_s'")))
    } else {
      leaf_user_met_list <- list(leaf = list(env = list(time = "'Time'", temp = "'Tair_degC'", par = "'PAR_umols_m2_s'",vpd="'VPD_kPa'",
                                                        atm_press="'Atm_press_Pa'",wind="'Windspeed_m_s'")))
    }
    leaf_user_met_xml <- PEcAn.settings::listToXml(leaf_user_met_list, "met_data_translator")

    # output XML file
    XML::saveXML(leaf_user_met_xml,
            file = file.path(outfolder, "leaf_user_met.xml"),
            indent = TRUE,
            prefix = PREFIX_XML)

    return(invisible(results))

  } else {
    print("NO MET TO OUTPUT")
    return(invisible(NULL))
  }
} # met2model.MAAT
