insertPmet <- function(vals, nc2, var2, dim2, units2 = NA, conv = NULL,
                       missval = -6999, verbose = FALSE, ...) {
  vals[vals == -6999 | vals == -9999] <- NA
  if (!is.null(conv)) {
    vals <- lapply(vals, conv)
  }
  var <- ncdf4::ncvar_def(name = var2, units = units2, dim = dim2, missval = missval, verbose = verbose)
  nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
  ncdf4::ncvar_put(nc = nc2, varid = var2, vals = vals)
} # insertPmet

##' Get meteorology variables from PalEON netCDF files and convert to netCDF CF format
##'
##' @name met2CF.PalEONregional
##' @title met2CF.PalEONregional
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose logical: enable verbose mode for netcdf writer functions?
##' @param ... further arguments, currently ignored
##'
##' @author Mike Dietze
met2CF.PalEONregional <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite = FALSE,
                          verbose = FALSE, ...) {

  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  ## check file organization
  by.folder <- list.dirs(in.path, recursive = FALSE, full.names = FALSE)
  if (length(by.folder) == 0) {
    PEcAn.logger::logger.severe("met2CF.PalEON, could not detect input folders", in.path)
  }

  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows),
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  for (year in start_year:end_year) {
    my.prefix <- in.prefix
    if (nchar(my.prefix) > 0) {
      my.prefix <- paste0(my.prefix, ".")
    }
    new.file <- file.path(outfolder, sprintf("%s%04d.nc", my.prefix, year))

    row <- year - start_year + 1
    results$file[row]       <- new.file
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[row]   <- "application/x-netcdf"
    results$formatname[row] <- "CF"

    if (file.exists(new.file) && !overwrite) {
      PEcAn.logger::logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }

    ### ASSUMING PALEON ORGANIZATION ONE FILE PER VARIABLE PER MONTH EACH VARIABLE IN A FOLDER WITH IT'S
    ### OWN NAME

    met <- list()
    for (i in seq_along(by.folder)) {
      met[[i]] <- NA
    }
    names(met) <- by.folder
    met[["time"]] <- NA

    for (v in by.folder) {
      fnames <- dir(file.path(in.path, v), full.names = TRUE)
      for (m in 1:12) {
        stub <- paste0(year, "_", formatC(m, width = 2, format = "d", flag = "0"))
        sel <- grep(stub, fnames)
        if (length(sel) == 0) {
          PEcAn.logger::logger.severe("missing file", v, stub)
        }
        old.file <- fnames[sel]
        nc1      <- ncdf4::nc_open(old.file, write = FALSE)

        if (length(met[[v]]) <= 1) {
          met[[v]] <- aperm(ncdf4::ncvar_get(nc = nc1, varid = v),c(2,1,3)) ## switch order from lon/lat/time to lat/lon/time
        } else {
          tmp      <- aperm(ncdf4::ncvar_get(nc = nc1, varid = v),c(2,1,3)) ## switch order from lon/lat/time to lat/lon/time
          met[[v]] <- abind::abind(met[[v]], tmp)
        }
        if (v == by.folder[1]) {
          if (length(met[["time"]]) <= 1) {
            met[["time"]] <- nc1$dim[["time"]]$vals
          } else {
            tmp <- nc1$dim[["time"]]$vals
            met[["time"]] <- abind::abind(met[["time"]], tmp)
          }
        }
        ncdf4::nc_close(nc1)
      }  ## end loop over months
    }  ## end loop over variables

    # create new coordinate dimensions based on site location lat/lon
    nc1           <- ncdf4::nc_open(old.file)
    tdim          <- nc1$dim[["time"]]
    met[["time"]] <- PEcAn.utils::ud_convert(met[["time"]],"days","seconds")
    tdim$units    <- paste0("seconds since ",year,"-01-01 00:00:00")
    tdim$vals     <- met[["time"]]
    tdim$len      <- length(tdim$vals)
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degrees", vals = nc1$dim[["lat"]]$vals, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degrees", vals = nc1$dim[["lon"]]$vals, create_dimvar = TRUE)
    time <- ncdf4::ncdim_def(name = "time", units = tdim$units, vals = tdim$vals,
                      create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)
    cp.global.atts <- ncdf4::ncatt_get(nc = nc1, varid = 0)
    ncdf4::nc_close(nc1)

    # Open new file and fill in air_temperature
    print(year)
    var <- ncdf4::ncvar_def(name = "air_temperature", units = "K", dim = dim,
                     missval = as.numeric(-9999))
    nc2 <- ncdf4::nc_create(filename = new.file, vars = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "air_temperature", vals = met[["tair"]])

    # air_pressure
    insertPmet(met[["psurf"]], nc2 = nc2, var2 = "air_pressure", units2 = "Pa", dim2 = dim,
               verbose = verbose)

    # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
    # insertPmet(nc1=nc1, var1='CO2', nc2=nc2, var2='mole_fraction_of_carbon_dioxide_in_air', units2='mole/mole', dim2=dim, conv=function(x) {
    # x * 1e6 }, verbose=verbose)

    # specific_humidity
    insertPmet(met[["qair"]], nc2 = nc2, var2 = "specific_humidity", units2 = "kg/kg", dim2 = dim,
               verbose = verbose)

    # surface_downwelling_shortwave_flux_in_air
    insertPmet(met[["swdown"]], nc2 = nc2, var2 = "surface_downwelling_shortwave_flux_in_air",
               units2 = "W m-2", dim2 = dim, verbose = verbose)

    # surface_downwelling_longwave_flux_in_air
    insertPmet(met[["lwdown"]], nc2 = nc2, var2 = "surface_downwelling_longwave_flux_in_air",
               units2 = "W m-2", dim2 = dim, verbose = verbose)

    # wind_speed
    insertPmet(met[["wind"]], nc2 = nc2, var2 = "wind_speed", units2 = "m s-1", dim2 = dim, verbose = verbose)

    # precipitation_flux
    insertPmet(met[["precipf"]], nc2 = nc2, var2 = "precipitation_flux", units2 = "kg/m^2/s",
               dim2 = dim, verbose = verbose)

    # add global attributes from original file
    for (j in seq_along(cp.global.atts)) {
      ncdf4::ncatt_put(nc = nc2, varid = 0, attname = names(cp.global.atts)[j], attval = cp.global.atts[[j]])
    }

    # done, close file
    ncdf4::nc_close(nc2)
#    save(results,file="met2CF.PalEON.RData")
  }  ## end loop over years

  return(invisible(results))
} # met2CF.PalEONregional

##' Get meteorology variables from PalEON netCDF files and convert to netCDF CF format
##'
##' @name met2CF.PalEON
##' @title met2CF.PalEON
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lat,lon site location in decimal degrees. Caution: both must have length one.
##' @param overwrite should existing files be overwritten
##' @param verbose logical: enable verbose mode for netcdf writer functions?
##' @param ... further arguments, currently ignored
##'
##' @author Mike Dietze
met2CF.PalEON <- function(in.path, in.prefix, outfolder, start_date, end_date, lat, lon, overwrite = FALSE,
                          verbose = FALSE, ...) {

  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  ## check file organization
  by.folder <- list.dirs(in.path, recursive = FALSE, full.names = FALSE)
  if (length(by.folder) == 0) {
    PEcAn.logger::logger.severe("met2CF.PalEON, could not detect input folders", in.path)
  }

  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows),
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  for (year in start_year:end_year) {
    my.prefix <- in.prefix
    if (nchar(my.prefix) > 0) {
      my.prefix <- paste0(my.prefix, ".")
    }
    new.file <- file.path(outfolder, sprintf("%s%04d.nc", my.prefix, year))

    row <- year - start_year + 1
    results$file[row]       <- new.file
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[row]   <- "application/x-netcdf"
    results$formatname[row] <- "CF"

    if (file.exists(new.file) && !overwrite) {
      PEcAn.logger::logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }

    ### ASSUMING PALEON ORGANIZATION ONE FILE PER VARIABLE PER MONTH EACH VARIABLE IN A FOLDER WITH IT'S
    ### OWN NAME

    met <- list()
    for (i in seq_along(by.folder)) {
      met[[i]] <- NA
    }
    names(met) <- by.folder
    met[["time"]] <- NA

    if(FALSE){
      for (v in by.folder) {
        fnames <- dir(file.path(in.path, v), full.names = TRUE)
        for (m in 1:12) {
          stub <- paste0(formatC(m, width = 2, format = "d", flag = "0"), ".",
                         formatC(year,width = 4,format = 'd',flag = '0'))
          sel <- grep(stub, fnames)
          if (length(sel) == 0) {
            PEcAn.logger::logger.severe("missing file", v, stub)
          }
          old.file <- fnames[sel]
          nc1      <- ncdf4::nc_open(old.file, write = FALSE)
          if (length(met[[v]]) <= 1) {
            met[[v]] <- ncdf4::ncvar_get(nc = nc1, varid = v)
          } else {
            tmp      <- ncdf4::ncvar_get(nc = nc1, varid = v)
            met[[v]] <- abind::abind(met[[v]], tmp)
          }
          if (v == by.folder[1]) {
            if (length(met[["time"]]) <= 1) {
              met[["time"]] <- nc1$dim[["time"]]$vals
            } else {
              tmp <- nc1$dim[["time"]]$vals
              met[["time"]] <- abind::abind(met[["time"]], tmp)
            }
          }
          ncdf4::nc_close(nc1)
        }  ## end loop over months
      }  ## end loop over variables
    }
    
    
    fnames <- dir(file.path(in.path, by.folder[1]), full.names = TRUE)
      stub <- paste0(formatC(1, width = 2, format = "d", flag = "0"), ".",
                     formatC(year,width = 4,format = 'd',flag = '0'))
      sel <- grep(stub, fnames)
      if (length(sel) == 0) {
        PEcAn.logger::logger.severe("missing file", by.folder[1], stub)
      }
      old.file <- fnames[sel]
      
      var.ids <- c('air_temperature','precipitation_flux',
                   'surface_downwelling_shortwave_flux_in_air',
                   'surface_downwelling_longwave_flux_in_air',
                   'air_pressure','specific_humidity',
                   'wind_speed')
      for(v in var.ids){
        met[[v]] <-  ncdf4::ncvar_get(nc = nc1, varid = v)
      }

    # create new coordinate dimensions based on site location lat/lon
    nc1           <- ncdf4::nc_open(old.file)
    tdim          <- nc1$dim[["time"]]
    met[["time"]] <- met[["time"]] + (850 - 1700)
    tdim$units    <- "days since 1700-01-01 00:00:00"
    tdim$vals     <- met[["time"]]
    tdim$len      <- length(tdim$vals)
    latlon        <- lat  # nc1$dim$lat$vals
    latlon[2]     <- lon  # nc1$dim$lon$vals
    lat <- ncdf4::ncdim_def(name = "latitude", units = "", vals = 1:1, create_dimvar = FALSE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "", vals = 1:1, create_dimvar = FALSE)
    time <- ncdf4::ncdim_def(name = "time", units = tdim$units, vals = tdim$vals,
                      create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)
    cp.global.atts <- ncdf4::ncatt_get(nc = nc1, varid = 0)
    ncdf4::nc_close(nc1)

    # Open new file and copy lat attribute to latitude
    print(c(latlon, year))
    var <- ncdf4::ncvar_def(name = "latitude", units = "degree_north", dim = (list(lat, lon, time)),
                     missval = as.numeric(-9999))
    nc2 <- ncdf4::nc_create(filename = new.file, vars = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "latitude", vals = rep(latlon[1], tdim$len))

    # copy lon attribute to longitude
    var <- ncdf4::ncvar_def(name = "longitude", units = "degree_east", dim = (list(lat, lon, time)),
                     missval = as.numeric(-9999))
    nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "longitude", vals = rep(latlon[2], tdim$len))

    # air_temperature
    insertPmet(met[["air_temperature"]], nc2 = nc2, var2 = "air_temperature", units2 = "K", dim2 = dim,
               verbose = verbose)

    # air_pressure
    insertPmet(met[["air_pressure"]], nc2 = nc2, var2 = "air_pressure", units2 = "Pa", dim2 = dim,
               verbose = verbose)

    # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
    # insertPmet(nc1=nc1, var1='CO2', nc2=nc2, var2='mole_fraction_of_carbon_dioxide_in_air', units2='mole/mole', dim2=dim, conv=function(x) {
    # x * 1e6 }, verbose=verbose)

    # specific_humidity
    insertPmet(met[["specific_humidity"]], nc2 = nc2, var2 = "specific_humidity", units2 = "kg/kg", dim2 = dim,
               verbose = verbose)

    # surface_downwelling_shortwave_flux_in_air
    insertPmet(met[["surface_downwelling_shortwave_flux_in_air"]], nc2 = nc2, var2 = "surface_downwelling_shortwave_flux_in_air",
               units2 = "W m-2", dim2 = dim, verbose = verbose)

    # surface_downwelling_longwave_flux_in_air
    insertPmet(met[["surface_downwelling_longwave_flux_in_air"]], nc2 = nc2, var2 = "surface_downwelling_longwave_flux_in_air",
               units2 = "W m-2", dim2 = dim, verbose = verbose)

    # wind_speed
    insertPmet(met[["wind_speed"]], nc2 = nc2, var2 = "wind_speed", units2 = "m s-1", dim2 = dim, verbose = verbose)

    # precipitation_flux
    insertPmet(met[["precipitation_flux"]], nc2 = nc2, var2 = "precipitation_flux", units2 = "kg/m^2/s",
               dim2 = dim, verbose = verbose)

    # add global attributes from original file
    for (j in seq_along(cp.global.atts)) {
      ncdf4::ncatt_put(nc = nc2, varid = 0, attname = names(cp.global.atts)[j], attval = cp.global.atts[[j]])
    }

    # done, close file
    ncdf4::nc_close(nc2)
  }  ## end loop over years

  return(invisible(results))
} # met2CF.PalEON


##' Get meteorology variables from ALMA netCDF files and convert to netCDF CF format
##'
##' @name met2CF.ALMA
##' @title met2CF.ALMA
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose logical: enable verbose mode for netcdf writer functions?
##'
##' @author Mike Dietze
met2CF.ALMA <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite = FALSE, verbose = FALSE) {

  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  ## check file organization
  by.file <- dir(in.path, pattern = ".nc")
  if (length(by.file) == 0) {
    by.file <- FALSE
    by.folder <- list.dirs(in.path, recursive = FALSE, full.names = FALSE)
    if (length(by.folder) == 0) {
      PEcAn.logger::logger.severe("met2CF.ALMA, could not detect input file or folders", in.path)
    }
  } else {
    by.file <- TRUE
  }

  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows),
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  for (year in start_year:end_year) {
    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep = "."))

    row <- year - start_year + 1
    results$file[row]       <- new.file
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[row]   <- "application/x-netcdf"
    results$formatname[row] <- "CF"

    if (file.exists(new.file) && !overwrite) {
      PEcAn.logger::logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }

    # create array with results
    if (by.file) {
      old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))

      # open original annual file
      nc1 <- ncdf4::nc_open(old.file, write = TRUE)

      # get dimension and site info
      tdim <- nc1$dim[["DTIME"]]

      ### LOTS MORE TO DO TO IMPLEMENT

      ncdf4::nc_close(nc1)
    } else {

      ### ASSUMING PALEON ORGANIZATION ONE FILE PER VARIABLE PER MONTH EACH VARIABLE
      ### IN A FOLDER WITH ITS OWN NAME

      met <- list()
      for (i in seq_along(by.folder)) {
        met[[i]] <- NA
      }
      names(met) <- by.folder
      met[["time"]] <- NA

      for (v in by.folder) {
        fnames <- dir(file.path(in.path, v), full.names = TRUE)
        for (m in 1:12) {
          sel      <- grep(paste0(year, "_", formatC(m, width = 2, format = "d", flag = "0")), fnames)
          old.file <- fnames[sel]
          nc1      <- ncdf4::nc_open(old.file, write = FALSE)
          if (length(met[[v]]) <= 1) {
            met[[v]] <- ncdf4::ncvar_get(nc = nc1, varid = v)
          } else {
            tmp      <- ncdf4::ncvar_get(nc = nc1, varid = v)
            met[[v]] <- abind::abind(met[[v]], tmp)
          }
          if (v == by.folder[1]) {
            if (length(met[["time"]]) <= 1) {
              met[["time"]] <- nc1$dim[["time"]]$vals
            } else {
              tmp           <- nc1$dim[["time"]]$vals
              met[["time"]] <- abind::abind(met[["time"]], tmp)
            }
          }
          ncdf4::nc_close(nc1)
        }
      }
    }

    # create new coordinate dimensions based on site location lat/lon
    nc1       <- ncdf4::nc_open(old.file)
    tdim      <- nc1$dim[["time"]]
    latlon    <- nc1$dim$lat$vals
    latlon[2] <- nc1$dim$lon$vals
    lat <- ncdf4::ncdim_def(name = "latitude", units = "", vals = 1:1, create_dimvar = FALSE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "", vals = 1:1, create_dimvar = FALSE)
    time <- ncdf4::ncdim_def(name = "time", units = tdim$units, vals = met[["time"]],
                      create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)

    # copy lat attribute to latitude
    print(latlon)
    var <- ncdf4::ncvar_def(name = "latitude", units = "degree_north", dim = (list(lat, lon, time)),
                     missval = as.numeric(-9999))
    nc2 <- ncdf4::nc_create(filename = new.file, vars = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "latitude", vals = rep(latlon[1], tdim$len))

    # copy lon attribute to longitude
    var <- ncdf4::ncvar_def(name = "longitude", units = "degree_east", dim = (list(lat, lon, time)),
                     missval = as.numeric(-9999))
    nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "longitude", vals = rep(latlon[2], tdim$len))

    # Convert all variables
    # This will include conversions or computations to create values from
    # original file. In case of conversions the steps will pretty much always be:
    # a) get values from original file
    # b) set -6999 and -9999 to NA
    # c) do unit conversions
    # d) create output variable
    # e) write results to new file

    # convert TA to air_temperature
    copyvals(nc1 = nc1,
             var1 = "TA",
             nc2 = nc2,
             var2 = "air_temperature", units2 = "K",
             dim2 = dim,
             conv = function(x) { PEcAn.utils::ud_convert(x, "degC", "K") },
             verbose = verbose)

    # convert PRESS to air_pressure
    copyvals(nc1 = nc1,
             var1 = "PRESS",
             nc2 = nc2,
             var2 = "air_pressure", units2 = "Pa",
             dim2 = dim,
             conv = function(x) { PEcAn.utils::ud_convert(x, 'kPa', 'Pa') },
             verbose = verbose)

    # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
    copyvals(nc1 = nc1,
             var1 = "CO2",
             nc2 = nc2,
             var2 = "mole_fraction_of_carbon_dioxide_in_air",  units2 = "mole/mole",
             dim2 = dim, conv = function(x) { PEcAn.utils::ud_convert(x, "mol/mol", "ppm") },
             verbose = verbose)

    # convert TS1 to soil_temperature
    copyvals(nc1 = nc1,
             var1 = "TS1",
             nc2 = nc2,
             var2 = "soil_temperature", units2 = "K",
             dim2 = dim,
             conv = function(x) { PEcAn.utils::ud_convert(x, "degC", "K") },
             verbose = verbose)

    # copy RH to relative_humidity
    copyvals(nc1 = nc1,
             var1 = "RH",
             nc2 = nc2,
             var2 = "relative_humidity", dim2 = dim,
             verbose = verbose)

    # convert RH to SH
    rh <- ncdf4::ncvar_get(nc = nc1, varid = "RH")
    rh[rh == -6999 | rh == -9999] <- NA
    rh <- rh/100
    ta <- ncdf4::ncvar_get(nc = nc1, varid = "TA")
    ta[ta == -6999 | ta == -9999] <- NA
    ta <- PEcAn.utils::ud_convert(ta, "degC", "K")
    sh <- rh2qair(rh = rh, T = ta)
    var <- ncdf4::ncvar_def(name = "specific_humidity", units = "kg/kg", dim = dim, missval = -6999,
                     verbose = verbose)
    nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "specific_humidity", vals = sh)

    # convert VPD to water_vapor_saturation_deficit
    # HACK : conversion will make all values < 0 to be NA
    copyvals(nc1 = nc1,
             var1 = "VPD",
             nc2 = nc2,
             var2 = "water_vapor_saturation_deficit", units2 = "mol m-2 s-1",
             dim2 = dim,
             conv = function(x) { ifelse(x < 0, NA, x * 1000) },
             verbose = verbose)

    # copy Rg to surface_downwelling_shortwave_flux_in_air
    copyvals(nc1 = nc1,
             var1 = "Rg",
             nc2 = nc2,
             var2 = "surface_downwelling_shortwave_flux_in_air", dim2 = dim,
             verbose = verbose)

    # copy Rgl to surface_downwelling_longwave_flux_in_air
    copyvals(nc1 = nc1,
             var1 = "Rgl",
             nc2 = nc2,
             var2 = "surface_downwelling_longwave_flux_in_air", dim2 = dim,
             verbose = verbose)

    # convert PAR to surface_downwelling_photosynthetic_photon_flux_in_air
    copyvals(nc1 = nc1,
             var1 = "PAR",
             nc2 = nc2,
             var2 = "surface_downwelling_photosynthetic_photon_flux_in_air", units2 = "mol m-2 s-1",
             dim2 = dim,
             conv = function(x) { PEcAn.utils::ud_convert(x, "umol m-2 s-1", "mol m-2 s-1") },
             verbose = verbose)

    # copy WD to wind_direction (not official CF)
    copyvals(nc1 = nc1,
             var1 = "WD",
             nc2 = nc2,
             var2 = "wind_direction", dim2 = dim,
             verbose = verbose)

    # copy WS to wind_speed
    copyvals(nc1 = nc1,
             var1 = "WS",
             nc2 = nc2,
             var2 = "wind_speed", dim2 = dim,
             verbose = verbose)

    # convert PREC to precipitation_flux
    t <- tdim$vals
    min <- 0.02083 / 30  # 0.02083 time = 30 minutes
    timestep <- round(x = mean(diff(t)) / min, digits = 1)  # round to nearest 0.1 minute
    copyvals(nc1 = nc1,
             var1 = "PREC",
             nc2 = nc2,
             var2 = "precipitation_flux", units2 = "kg/m^2/s",
             dim2 = dim, conv = function(x) { x / timestep / 60 },
             verbose = verbose)

    # convert wind speed and wind direction to eastward_wind and northward_wind
    wd <- ncdf4::ncvar_get(nc = nc1, varid = "WD")  #wind direction
    wd[wd == -6999 | wd == -9999] <- NA
    ws <- ncdf4::ncvar_get(nc = nc1, varid = "WS")  #wind speed
    ws[ws == -6999 | ws == -9999] <- NA
    ew <- ws * cos(wd * (pi / 180))
    nw <- ws * sin(wd * (pi / 180))
    max <- ncdf4::ncatt_get(nc = nc1, varid = "WS", "valid_max")$value

    var <- ncdf4::ncvar_def(name = "eastward_wind", units = "m/s", dim = dim, missval = -6999, verbose = verbose)
    nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "eastward_wind", vals = ew)
    ncdf4::ncatt_put(nc = nc2, varid = "eastward_wind", attname = "valid_min", attval = -max)
    ncdf4::ncatt_put(nc = nc2, varid = "eastward_wind", attname = "valid_max", attval = max)

    var <- ncdf4::ncvar_def(name = "northward_wind", units = "m/s", dim = dim, missval = -6999, verbose = verbose)
    nc2 <- ncdf4::ncvar_add(nc = nc2, v = var, verbose = verbose)
    ncdf4::ncvar_put(nc = nc2, varid = "northward_wind", vals = nw)
    ncdf4::ncatt_put(nc = nc2, varid = "northward_wind", attname = "valid_min", attval = -max)
    ncdf4::ncatt_put(nc = nc2, varid = "northward_wind", attname = "valid_max", attval = max)

    # add global attributes from original file
    cp.global.atts <- ncdf4::ncatt_get(nc = nc1, varid = 0)
    for (j in seq_along(cp.global.atts)) {
      ncdf4::ncatt_put(nc = nc2, varid = 0, attname = names(cp.global.atts)[j], attval = cp.global.atts[[j]])
    }

    # done, close both files
    ncdf4::nc_close(nc1)
    ncdf4::nc_close(nc2)
  }  ## end loop over years

  return(invisible(results))
} # met2CF.ALMA
