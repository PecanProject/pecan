#' Process ncdf file, run PRELES model, and convert output .nc file in CF standard
#'
#' @param met.file base name for yearly nc files containing met data.
#'   Example: `met.file="somefile"` matches somefile.2004.nc, somefile.2005.nc, etc.
#' @param outdir Location of PRELES model output
#' @param parameters An R data file containing parameter values.
#'  Must be an Rda file written via `save()`, and must define an object named
#'  `trait.values`
#' @param sitelat,sitelon Latitude and longitude of site in decimal degrees
#' @param start.date,end.date Start and end time of the simulation
#'
#' @export
#' @author Tony Gardella, Michael Dietze
runPRELES.jobsh <- function(met.file, outdir, parameters, sitelat, sitelon, start.date, end.date) {
  
  if (!requireNamespace("Rpreles", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "The Rpreles package is not installed.
      Please execute- devtools::install_github('MikkoPeltoniemi/Rpreles')")
  }
  
  # Process start and end dates
  start_date <- as.POSIXlt(start.date, tz = "UTC")
  end_date <- as.POSIXlt(end.date, tz = "UTC")
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  timestep.s <- PEcAn.utils::ud_convert(1, "day", "seconds")  # Number of seconds in a day
  
  ## Build met
  met <- NULL
  for (year in start_year:end_year) {
    
    met.file.y <- paste(met.file, year, "nc", sep = ".")
    
    if (file.exists(met.file.y)) {
      
      ## Open netcdf file
      nc <- ncdf4::nc_open(met.file.y)
      
      ## convert time to seconds
      sec <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
      
      ## build day and year
      
      dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
      tstep <- round(timestep.s / dt)  #time steps per day
      
      diy <- PEcAn.utils::days_in_year(year)
      doy <- rep(seq_len(diy), each = tstep)[seq_along(sec)]
      
      ## Get variables from netcdf file
      SW     <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  # SW in W/m2
      Tair   <- ncdf4::ncvar_get(nc, "air_temperature")  # air temperature in K
      Precip <- ncdf4::ncvar_get(nc, "precipitation_flux")  # precipitation in kg/m2s1
      CO2    <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))  # mol/mol
      SH     <- ncdf4::ncvar_get(nc, "specific_humidity")
      lat    <- ncdf4::ncvar_get(nc, "latitude")
      lon    <- ncdf4::ncvar_get(nc, "longitude")
      
      ncdf4::nc_close(nc)
      
      ## Check for CO2 and PAR
      if (!is.numeric(CO2)) {
        PEcAn.logger::logger.warn("CO2 not found. Setting to default: 4.0e+8 mol/mol")  # using rough estimate of atmospheric CO2 levels
        CO2 <- rep(4e+08, length(Precip))
      }
      
      ## GET VPD from Saturated humidity and Air Temperature
      RH <- PEcAn.data.atmosphere::qair2rh(SH, Tair)
      VPD <- PEcAn.data.atmosphere::get.vpd(RH, Tair)
      
      VPD <- VPD * 0.01  # convert to Pascal
      
      ## Get PPFD from SW
      PPFD <- PEcAn.data.atmosphere::sw2ppfd(SW)  # PPFD in umol/m2/s
      PPFD <- PEcAn.utils::ud_convert(PPFD, "umol m-2 s-1", "mol m-2 s-1")
      
      ## Format/convert inputs
      ppfd   <- tapply(PPFD, doy, mean, na.rm = TRUE)  # Find the mean for the day
      tair   <- PEcAn.utils::ud_convert(tapply(Tair, doy, mean, na.rm = TRUE), "kelvin", "celsius")  # Convert Kelvin to Celcius
      vpd    <- PEcAn.utils::ud_convert(tapply(VPD, doy, mean, na.rm = TRUE), "Pa", "kPa")  # pascal to kila pascal
      precip <- tapply(Precip, doy, sum, na.rm = TRUE)  # Sum to daily precipitation
      co2    <- tapply(CO2, doy, mean)  # need daily average, so sum up day
      co2    <- co2 * 1e+06  # convert to ppm
      doy    <- tapply(doy, doy, mean)  # day of year
      fapar  <- rep(0.6, length = length(doy))  # For now set to 0.6. Needs to be between 0-1
      
      ## Bind inputs
      tmp <- cbind(ppfd, tair, vpd, precip, co2, fapar)
      tmp[is.na(tmp)] <- 0
      met <- rbind(met, tmp)
    }  ## end file exists
  }  ## end met process
  
  param.def <- rep(NA, 30)
  
  #PARAMETER DEFAULT LIST
  ##GPP_MODEL_PARAMETERS
  #1.soildepth 413.0    |2.ThetaFC 0.450      | 3.ThetaPWP 0.118        |4.tauDrainage 3
  #5.betaGPP 0.748018   |6.tauGPP 13.23383    |7.S0GPP -3.9657867       |8.SmaxGPP 18.76696
  #9.kappaGPP -0.130473 |10.gammaGPP 0.034459 |11.soilthresGPP 0.450828 |12.cmCO2 2000
  #13.ckappaCO2 0.4
  ##EVAPOTRANSPIRATION_PARAMETERS
  #14.betaET  0.324463  |15.kappaET 0.874151  |16.chiET 0.075601        |17.soilthresE 0.541605
  #18.nu ET 0.273584
  ##SNOW_RAIN_PARAMETERS
  #19.Meltcoef 1.2      |20.I_0 0.33          |21.CWmax 4.970496        |22.SnowThreshold 0
  #23.T_0 0
  ##START INITIALISATION PARAMETERS
  #24.SWinit 200        |25.CWinit 0          |26.SOGinit 0             |27.Sinit 20
  #28.t0 fPheno_start_date_Tsum_accumulation; conif -999, for birch 57
  #29.tcrit -999 fPheno_start_date_Tsum_Tthreshold, 1.5 birch
  #30.tsumcrit -999 fPheno_budburst_Tsum, 134 birch
  
  ## Replace default with sampled parameters
  param_objs <- new.env()
  load(parameters, envir = param_objs)
  params <- data.frame(param_objs$trait.values)
  colnames(params) <- names(param_objs$trait.values[[1]])
  
  param.def[5] <- as.numeric(params["bGPP"])
  param.def[9] <- as.numeric(params["kGPP"])
  
  ## Run PRELES
  PRELES.output <- as.data.frame(Rpreles::PRELES(PAR = tmp[, "ppfd"],
                                                 TAir = tmp[, "tair"],
                                                 VPD = tmp[, "vpd"],
                                                 Precip = tmp[, "precip"],
                                                 CO2 = tmp[, "co2"],
                                                 fAPAR = tmp[, "fapar"],
                                                 p = param.def))
  PRELES.output.dims <- dim(PRELES.output)
  
  days <- as.Date(start_date):as.Date(end_date)
  year <- strftime(as.Date(days, origin = "1970-01-01"), "%Y")
  years <- unique(year)
  num.years <- length(years)
  
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y))))
      next
    print(paste("----Processing year: ", y))
    
    sub.PRELES.output <- subset(PRELES.output, years == y)
    sub.PRELES.output.dims <- dim(sub.PRELES.output)
    
    output <- list()
    output[[1]] <- PEcAn.utils::ud_convert(sub.PRELES.output[, 1], 'g m-2 day-1', 'kg m-2 sec-1')  #GPP - gC/m2day to kgC/m2s1
    output[[2]] <- (sub.PRELES.output[, 2])/timestep.s  #Evapotranspiration - mm =kg/m2
    output[[3]] <- (sub.PRELES.output[, 3])/timestep.s  #Soilmoisture - mm = kg/m2
    output[[6]] <- (sub.PRELES.output[, 6])/timestep.s  #Evaporation - mm = kg/m2
    output[[7]] <- (sub.PRELES.output[, 7])/timestep.s  #transpiration - mm = kg/m2
    
    t <- ncdf4::ncdim_def(name = "time",
                          units = paste0("days since", y, "-01-01 00:00:00"),
                          vals = 1:nrow(sub.PRELES.output),
                          calendar = "standard",
                          unlim = TRUE)
    
    lat <- ncdf4::ncdim_def("lat", "degrees_east", vals = as.numeric(sitelat), longname = "station_longitude")
    lon <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelon), longname = "station_longitude")
    
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0)
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var      <- list()
    nc_var[[1]] <- PEcAn.utils::to_ncvar("GPP",dims)
    nc_var[[2]] <- ncdf4::ncvar_def("Evapotranspiration", "kg/m2s1", list(lon, lat, t), -999)
    nc_var[[3]] <- PEcAn.utils::to_ncvar("SoilMoist", dims)
    nc_var[[4]] <- PEcAn.utils::to_ncvar("Evap", dims)
    nc_var[[5]] <- PEcAn.utils::to_ncvar("TVeg", dims)
    
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  }
} # runPRELES.jobsh
