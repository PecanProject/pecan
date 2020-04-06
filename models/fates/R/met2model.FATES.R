#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into NetCDF FATES met files.

##' met2model wrapper for FATES
##'
##' @title met2model for FATES
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbosefor(year in start_year:end_year)
##' @importFrom ncdf4 ncvar_get ncdim_def ncatt_get ncvar_put
met2model.FATES <- function(in.path, in.prefix, outfolder, start_date, end_date, lst = 0, lat, lon,
                            overwrite = FALSE, verbose = FALSE, ...) {

  # General Structure- FATES Uses Netcdf so we need to rename vars, split files from years into months, and generate the header file
  # Get Met file from inpath.
  # Loop over years (Open nc.file,rename vars,change dimensions as needed,close/save .nc file)
  # close
  # defining temporal dimension needs to be figured out. If we configure FATES to use same tstep then we may not need to change dimensions

  library(PEcAn.utils)

  insert <- function(ncout, name, unit, data) {
    var <- ncdf4::ncvar_def(name = name, units = unit, dim = dim, missval = -6999, verbose = verbose)
    ncout <- ncdf4::ncvar_add(nc = ncout, v = var, verbose = verbose)
    ncvar_put(nc = ncout, varid = name, vals = data)
    return(invisible(ncout))
  }
  sm <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365) * 86400 ## day of year thresholds

  ## Create output directory
  dir.create(outfolder)

  # Process start and end dates
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  ## Build met
  for (year in start_year:end_year) {
    in.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))

    if (file.exists(in.file)) {

      ## Open netcdf file
      nc <- ncdf4::nc_open(in.file)

      ## extract variables. These need to be read in and converted to CLM names (all units are correct)
      time <- ncvar_get(nc, "time")
      latitude <- ncvar_get(nc, "latitude")
      longitude <- ncvar_get(nc, "longitude")
      FLDS <- ncvar_get(nc, "surface_downwelling_longwave_flux_in_air") ## W/m2
      FSDS <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air") ## W/m2
      PRECTmms <- ncvar_get(nc, "precipitation_flux") ## kg/m2/s -> mm/s (same val, diff name)
      PSRF <- ncvar_get(nc, "air_pressure") ## Pa
      SHUM <- ncvar_get(nc, "specific_humidity") ## g/g -> kg/kg
      TBOT <- ncvar_get(nc, "air_temperature") ## K
      WIND <- sqrt(ncvar_get(nc, "eastward_wind")^2 + ncvar_get(nc, "northward_wind")^2) ## m/s

      ## CREATE MONTHLY FILES
      for (mo in 1:12) {
        tsel <- which(time > sm[mo] & time <= sm[mo + 1])
        outfile <- file.path(outfolder, paste0(
          formatC(year, width = 4, flag = "0"), "-",
          formatC(mo, width = 2, flag = "0"), ".nc"
        ))
        if (file.exists(outfile) & overwrite == FALSE) {
          next
        }

        lat.dim <- ncdim_def(name = "latitude", units = "", vals = 1:1, create_dimvar = FALSE)
        lon.dim <- ncdim_def(name = "longitude", units = "", vals = 1:1, create_dimvar = FALSE)
        time.dim <- ncdim_def(
          name = "time", units = "seconds", vals = time,
          create_dimvar = TRUE, unlim = TRUE
        )
        dim <- list(lat.dim, lon.dim, time.dim) ## docs say this should be time,lat,lon but get error writing unlimited first
        ## http://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/x12979.html

        # LATITUDE
        var <- ncdf4::ncvar_def(
          name = "latitude", units = "degree_north",
          dim = list(lat.dim, lon.dim), missval = as.numeric(-9999)
        )
        ncout <- ncdf4::nc_create(outfile, vars = var, verbose = verbose)
        ncvar_put(nc = ncout, varid = "latitude", vals = latitude)

        # LONGITUDE
        var <- ncdf4::ncvar_def(
          name = "longitude", units = "degree_east",
          dim = list(lat.dim, lon.dim), missval = as.numeric(-9999)
        )
        ncout <- ncdf4::ncvar_add(nc = ncout, v = var, verbose = verbose)
        ncvar_put(nc = ncout, varid = "longitude", vals = longitude)

        ## surface_downwelling_longwave_flux_in_air
        ncout <- insert(ncout, "FLDS", "W m-2", FLDS)

        ## surface_downwelling_shortwave_flux_in_air
        ncout <- insert(ncout, "FSDS", "W m-2", FSDS)

        ## precipitation_flux
        ncout <- insert(ncout, "PRECTmms", "mm/s", PRECTmms)

        ## air_pressure
        ncout <- insert(ncout, "PSRF", "Pa", PSRF)

        ## specific_humidity
        ncout <- insert(ncout, "SHUM", "kg/kg", SHUM)

        ## air_temperature
        ncout <- insert(ncout, "TBOT", "K", TBOT)

        ## eastward_wind & northward_wind
        ncout <- insert(ncout, "WIND", "m/s", WIND)

        ncdf4::nc_close(ncout)

        #   ncvar_rename(ncfile,varid="LONGXY")
        #   ncvar_rename(ncfile,varid="LATIXY")
        #   #
        #   #     double EDGEW(scalar) ;
        #   #     EDGEW:long_name = "western edge in atmospheric data" ;
        #   #     EDGEW:units = "degrees E" ;
        #   EDGEW = ncvar_rename(ncfile,"EDGEW","EDGEW")
        #
        #   #     double EDGEE(scalar) ;
        #   #     EDGEE:long_name = "eastern edge in atmospheric data" ;
        #   #     EDGEE:units = "degrees E" ;
        #   EDGEE = ncvar_rename(ncfile,"EDGEE","EDGEE")
        #
        #   #     double EDGES(scalar) ;
        #   #     EDGES:long_name = "southern edge in atmospheric data" ;
        #   #     EDGES:units = "degrees N" ;
        #   EDGES = ncvar_rename(ncfile,"EDGES","EDGES")
        #   #
        #   #     double EDGEN(scalar) ;
        #   #     EDGEN:long_name = "northern edge in atmospheric data" ;
        #   #     EDGEN:units = "degrees N" ;
        #   EDGEN = ncvar_rename(ncfile,"EDGEN","EDGEN")
      }

      ncdf4::nc_close(nc)
    } ## end file exists
  } ### end loop over met files

  PEcAn.logger::logger.info("Done with met2model.FATES")

  return(data.frame(
    file = paste0(outfolder, "/"),
    host = c(PEcAn.remote::fqdn()),
    mimetype = c("application/x-netcdf"),
    formatname = c("CLM met"),
    startdate = c(start_date),
    enddate = c(end_date),
    dbfile.name = "",
    stringsAsFactors = FALSE
  ))
} # met2model.FATES
