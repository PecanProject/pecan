# R Code to convert NetCDF CF met files into DALEC met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for DALEC
##'
##' @title met2model.DALEC
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param spin_nyear,spin_nsample,spin_resample passed on to
##'  `PEcAn.data.atmosphere::spin.met()`
##' @param ... additional arguments, currently ignored
met2model.DALEC <- function(in.path, in.prefix, outfolder, start_date, end_date,
                            overwrite = FALSE, verbose = FALSE, spin_nyear=NULL,spin_nsample=NULL,spin_resample=NULL, ...) {

  ## DALEC 1 driver format (.csv): Runday, Min temp (°C), Max temp (°C), Radiation (MJ d-1),
  ## Atmospheric CO2 (μmol mol-1), Day of year

  ## DALEC EnKF (Quaife) format (.dat, space delimited): The nine columns of driving data are: day
  ## of year; mean air temperature (deg C); max daily temperature (deg C); min daily temperature
  ## (deg C); incident radiation (MJ/m2/day); maximum soil-leaf water potential difference (MPa);
  ## atmospheric carbon dioxide concentration (ppm); total plant-soil hydraulic resistance
  ## (MPa.m2.s/mmol-1); average foliar nitorgen (gC/m2 leaf area).  Calculate these from
  ## air_temperature (K), surface_downwelling_shortwave_flux_in_air (W/m2), CO2 (ppm)

  start_date <- as.POSIXlt(start_date, tz = "UTC")
  start_date_string <- as.character(strptime(start_date, "%Y-%m-%d"))
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  if(nchar(in.prefix)>0 & substr(in.prefix,nchar(in.prefix),nchar(in.prefix)) != ".") in.prefix = paste0(in.prefix,".")

  if(!is.null(spin_nyear)){
    ## if spinning up, extend processed met by resampling or cycling met
    PEcAn.logger::logger.info("Adding Spin-up met for DALEC")
    spin_nyear <- as.numeric(spin_nyear)
    spin_nsample <- as.numeric(spin_nsample)
    spin_resample <- as.logical(spin_resample)
    start_date <- PEcAn.data.atmosphere::spin.met(in.path,in.prefix,start_date,end_date,
                                                  spin_nyear,spin_nsample,spin_resample)
#    start_date <- as.POSIXlt(strftime(start_date, "%Y-%m-%d"), tz = "UTC")
#    start_date <- strptime(paste0(start_year,"-01-01"),"%Y-%m-%d", tz = "UTC")
    start_date_string <- paste0(lubridate::year(start_date),"-01-01") ## strptime can't parse negative years
    PEcAn.logger::logger.info("New Start Date",start_date_string)
  }

  out.file <- paste0(in.prefix, start_date_string,".",
                    strptime(end_date, "%Y-%m-%d"),
                    ".dat")
  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = c(out.file.full),
                        host = c(PEcAn.remote::fqdn()),
                        mimetype = c("text/plain"),
                        formatname = c("DALEC meteorology"),
                        startdate = c(start_date),
                        enddate = c(end_date),
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

  ## loop over files
  for (year in start_year:end_year) {
    print(year)
    ## Assuming default values for leaf water potential, hydraulic resistance, foliar N
    leafN <- 2.5
    HydResist <- 1
    LeafWaterPot <- -0.8

    old.file <- file.path(in.path, paste(in.prefix, year, ".nc", sep = ""))
    if(!file.exists(old.file)) PEcAn.logger::logger.error("file not found",old.file)
    ## open netcdf
    nc <- ncdf4::nc_open(old.file)

    ## convert time to seconds
    sec <- nc$dim$time$vals
    sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
    timestep.s <- 86400  # seconds in a day
    dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
    tstep <- round(timestep.s / dt)
    dt    <- timestep.s / tstep  #dt is now an integer

    ## extract variables
    lat  <- ncdf4::ncvar_get(nc, "latitude")
    lon  <- ncdf4::ncvar_get(nc, "longitude")
    Tair <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
    SW   <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## in W/m2
    CO2  <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))
    ncdf4::nc_close(nc)

    useCO2 <- is.numeric(CO2)
    if (useCO2)
      CO2 <- CO2 * 1e+06  ## convert from mole fraction (kg/kg) to ppm

    ## is CO2 present?
    if (!is.numeric(CO2)) {
      PEcAn.logger::logger.warn("CO2 not found in", old.file, "setting to default: 400 ppm")
      CO2 <- rep(400, length(Tair))
    }

    if (length(leafN) == 1) {
      PEcAn.logger::logger.warn("Leaf N not specified, setting to default: ", leafN)
      leafN <- rep(leafN, length(Tair))
    }
    if (length(HydResist) == 1) {
      PEcAn.logger::logger.warn("total plant-soil hydraulic resistance (MPa.m2.s/mmol-1) not specified, setting to default: ",
                  HydResist)
      HydResist <- rep(HydResist, length(Tair))
    }
    if (length(LeafWaterPot) == 1) {
      PEcAn.logger::logger.warn("maximum soil-leaf water potential difference (MPa) not specified, setting to default: ",
                  LeafWaterPot)
      LeafWaterPot <- rep(LeafWaterPot, length(Tair))
    }

    ## build day of year
    diy <- PEcAn.utils::days_in_year(year)
    doy <- rep(seq_len(diy), each = timestep.s / dt)[seq_along(sec)]

    ## Aggregate variables up to daily
    Tmean        <- PEcAn.utils::ud_convert(tapply(Tair, doy, mean, na.rm = TRUE), "Kelvin", "Celsius")
    Tmin         <- PEcAn.utils::ud_convert(tapply(Tair, doy, min, na.rm = TRUE), "Kelvin", "Celsius")
    Tmax         <- PEcAn.utils::ud_convert(tapply(Tair, doy, max, na.rm = TRUE), "Kelvin", "Celsius")
    Rin          <- tapply(SW, doy, sum) * dt * 1e-06  # J/m2/s * s * MJ/J
    LeafWaterPot <- tapply(LeafWaterPot, doy, mean)
    CO2          <- tapply(CO2, doy, mean)
    HydResist    <- tapply(HydResist, doy, mean)
    leafN        <- tapply(leafN, doy, mean)
    doy          <- tapply(doy, doy, mean)

    ## The nine columns of driving data are: day of year; mean air temperature (deg C); max daily
    ## temperature (deg C); min daily temperature (deg C); incident radiation (MJ/m2/day); maximum
    ## soil-leaf water potential difference (MPa); atmospheric carbon dioxide concentration (ppm);
    ## total plant-soil hydraulic resistance (MPa.m2.s/mmol-1); average foliar nitorgen (gC/m2 leaf
    ## area).

    ## build data matrix
    tmp <- cbind(doy, Tmean, Tmax, Tmin, Rin, LeafWaterPot, CO2, HydResist, leafN)

    ##filter out days not included in start or end date
    if(year == start_year){
      start.row <- length(as.Date(paste0(start_year, "-01-01")):as.Date(start_date)) #extra days length includes the start date
      if (start.row > 1){
        PEcAn.logger::logger.info("Subsetting DALEC met to match start date ", as.Date(start_date))
        print(start.row)
        print(nrow(tmp))
        tmp <- tmp[start.row:nrow(tmp),]
      }
    }
    if (year == end_year){
      if(year == start_year){
        end.row <- length(as.Date(start_date):as.Date(end_date))
        if (end.row < nrow(tmp)){
          PEcAn.logger::logger.info("Subsetting DALEC met to match end date")
          tmp <- tmp[1:end.row,]
        }
      } else{
        end.row <- length(as.Date(paste0(end_year, "-01-01")):as.Date(end_date))
        if (end.row < nrow(tmp)){
          PEcAn.logger::logger.info("Subsetting DALEC met to match end date")
          tmp <- tmp[1:end.row,]
        }
      }

    }

    if (is.null(out)) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
  }  ## end loop over years
    utils::write.table(out, out.file.full, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible(results))

} # met2model.DALEC
