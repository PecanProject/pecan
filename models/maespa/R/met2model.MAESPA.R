
# R Code to convert NetCDF CF met files into MAESPA met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model wrapper for MAESPA
##'
##' @title met2model.MAESPA
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##'
##' @author Tony Gardella
met2model.MAESPA <- function(in.path, in.prefix, outfolder, start_date, end_date, 
                             overwrite = FALSE, verbose = FALSE, ...) {
  print("START met2model.MAESPA")
  start.date <- as.POSIXlt(start_date, tz = "GMT")
  end.date <- as.POSIXlt(end_date, tz = "GMT")

  out.file <- paste(in.prefix,
                    strptime(start.date, "%Y-%m-%d"),
                    strptime(end.date, "%Y-%m-%d"),
                    "dat",
                    sep = ".")

  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/plain",
                        formatname = "maespa.met",
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
  start_year <- lubridate::year(start.date)
  end_year <- lubridate::year(end.date)

  ## loop over files
  for (year in start_year:end_year) {
    print(year)

    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))

    if (file.exists(old.file)) {
      ## open netcdf
      nc <- ncdf4::nc_open(old.file)
      ## convert time to seconds
      sec <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")

      dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
      tstep <- round(86400 / dt)
      dt <- 86400 / tstep

      # Check which variables are available and which are not

      ## extract variables
      lat   <- ncdf4::ncvar_get(nc, "latitude")
      lon   <- ncdf4::ncvar_get(nc, "longitude")
      RAD   <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  #W m-2
      PAR   <- try(ncdf4::ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air"))  #mol m-2 s-1
      TAIR  <- ncdf4::ncvar_get(nc, "air_temperature")  # K
      QAIR  <- ncdf4::ncvar_get(nc, "specific_humidity")  # 1
      PPT   <- ncdf4::ncvar_get(nc, "precipitation_flux")  #kg m-2 s-1
      CA    <- try(ncdf4::ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))  #mol/mol
      PRESS <- ncdf4::ncvar_get(nc, "air_pressure")  # Pa
      
      ## Convert specific humidity to fractional relative humidity
      RH <- PEcAn.data.atmosphere::qair2rh(QAIR, TAIR, PRESS)
      
      ## Process PAR
      if (!is.numeric(PAR)) {
        # Function from data.atmosphere will convert SW to par in W/m2
        PAR <- PEcAn.data.atmosphere::sw2par(RAD)
      } else {
        # convert
        PAR <- PEcAn.utils::ud_convert(PAR, "mol", "umol")
      }

      # Convert air temperature to Celsius
      TAIR <- PEcAn.utils::ud_convert(TAIR, "kelvin", "celsius")

      #### ppm. atmospheric CO2 concentration.
      ### Constant from Environ namelist used instead if CA is nonexistent
      defaultCO2 <- 400
      if (!is.numeric(CA)) {
        print("Atmospheric CO2 concentration will be set to constant value set in ENVIRON namelist ")
        rm(CA)
      } else {
        CA <- CA * 1e+06
      }

      ncdf4::nc_close(nc)
    } else {
      print("Skipping to next year")
      next
    }

    if (exists("CA")) {
      tmp <- cbind(TAIR, PPT, RAD, PRESS, PAR, RH, CA)
    } else {
      tmp <- cbind(TAIR, PPT, RAD, PRESS, PAR, RH)
    }

    if (is.null(out)) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }

  }  ### end loop over years

  ### Check for NA
  if (anyNA(out)) {
    PEcAn.logger::logger.debug("NA introduced in met data. Maespa will not be able to run properly. Please change Met Data Source or Site")
  } else {
    PEcAn.logger::logger.debug("No NA values contained in data")
  }

  ## Set Variable names
  columnnames <- colnames(out)

  # Set number of timesteps in a day(timetsep of input data)
  timesteps <- tstep
  # Set distribution of diffuse radiation incident from the sky.(0.0) is default.
  difsky <- 0.5
  # Change format of date to DD/MM/YY
  startdate <- paste0(format(as.Date(start_date), "%d/%m/%y"))
  enddate <- paste0(format(as.Date(end_date), "%d/%m/%y"))

  ## Units of Latitude and longitude
  if (nc$dim$latitude$units == "degree_north") {
    latunits <- "N"
  } else {
    latunits <- "S"
  }

  if (nc$dim$longitude$units == "degree_east") {
    lonunits <- "E"
  } else {
    lonunits <- "W"
  }

  ## Write output met.dat file
  metfile <- system.file("met.dat", package = "PEcAn.MAESPA")
  met.dat <- Maeswrap::replacemetdata(out, oldmetfile = metfile, newmetfile = out.file.full)
  
  Maeswrap::replacePAR(out.file.full, "difsky", "environ", newval = difsky, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "ca", "environ", newval = defaultCO2, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "lat", "latlong", newval = lat, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "long", "latlong", newval = lon, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "lonhem", "latlong", newval = lonunits, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "lathem", "latlong", newval = latunits, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "startdate", "metformat", newval = startdate, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "enddate", "metformat", newval = enddate, noquotes = TRUE)
  Maeswrap::replacePAR(out.file.full, "columns", "metformat", newval = columnnames, noquotes = TRUE)
  
  return(invisible(results))
} # met2model.MAESPA
