# R Code to convert NetCDF CF met files into LPJ-GUESS met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model wrapper for LPJ-GUESS
##'
##' @title met2model.LPJGUESS
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param ... additional arguments, currently ignored
##' @author Istem Fer
##' @importFrom ncdf4 ncvar_get ncvar_def ncdim_def ncatt_get ncatt_put nc_close
met2model.LPJGUESS <- function(in.path, in.prefix, outfolder, start_date, end_date,
                               overwrite = FALSE, verbose = FALSE, ...) {


  print("START met2model.LPJGUESS")
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  year <- sprintf("%04d", seq(start_year, end_year, 1))
  nyear <- length(year)  #number of years to simulate

  ## LPJ-GUESS looks for different input files for different climate variables
  out.file <- out.files.full <- list()
  var.names <- c("tmp", "pre", "cld")
  n.var <- length(var.names)
  long.names <- c("air_temperature",
                  "precipitation_flux",
                  "surface_downwelling_shortwave_flux_in_air")
  for (i in seq_len(n.var)) {
    out.file[[i]] <- paste(in.prefix, sprintf("%04d", start_year), end_year, var.names[[i]],
                           "nc", sep = ".")
  }
  for (i in seq_len(n.var)) {
    out.files.full[[i]] <- file.path(outfolder, out.file[[i]])
  }

  results <- data.frame(file = unlist(out.files.full),
                        host = PEcAn.remote::fqdn(),
                        mimetype = "application/x-netcdf",
                        formatname = "lpj-guess.metfile",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = unlist(out.file),
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)

  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) { # why not use `dir.exists`?
    dir.create(outfolder)
  }

  ## open netcdf files
  ncin <- lapply(file.path(in.path, paste(in.prefix, year, "nc", sep = ".")), ncdf4::nc_open)

  ## retrieve lat/lon
  lon <- ncdf4::ncvar_get(ncin[[1]], "longitude")
  lat <- ncdf4::ncvar_get(ncin[[1]], "latitude")

  ## at least 2 lat-lon required for LPJ-GUESS to load the data
  lon <- c(lon, lon)
  lat <- c(lat, lat)

  ## calculate time step from the time-dimension length, check for leap year
  tstep <- ifelse(ncin[[1]]$dim$time$len %% 365 == 0,
                  ncin[[1]]$dim$time$len / 365,
                  ncin[[1]]$dim$time$len / 366)

  ## read climate data
  nc.tmp <- lapply(ncin, ncdf4::ncvar_get, long.names[1])
  nc.pre <- lapply(ncin, ncdf4::ncvar_get, long.names[2])
  nc.cld <- lapply(ncin, ncdf4::ncvar_get, long.names[3])

  ## aggregate to daily time steps, LPJ-GUESS reads daily climate data
  tmp.list <- pre.list <- cld.list <- list()
  for (y in seq_len(nyear)) {
    diy <- PEcAn.utils::days_in_year(as.numeric(year[y]))
    ind.vec <- rep(seq_len(diy), each = tstep)
    tmp.list[[y]] <- tapply(nc.tmp[[y]], ind.vec, mean)
    pre.list[[y]] <- tapply(nc.pre[[y]], ind.vec, mean)
    cld.list[[y]] <- tapply(nc.cld[[y]], ind.vec, mean)
  }

  var.list <- list(unlist(tmp.list), unlist(pre.list), unlist(cld.list))

  var.units <- c("K", "kg m-2 s-1", "W m-2")

  ## write climate data define dimensions

  latdim  <- ncdf4::ncdim_def(name = "lat", "degrees_north", as.double(lat))
  londim  <- ncdf4::ncdim_def(name = "lon", "degrees_east", as.double(lon))
  timedim <- ncdf4::ncdim_def("time", paste0("days since ", start_year - 1, "-12-31", sep = ""), as.double(c(1:length(unlist(tmp.list)))))

  fillvalue <- 9.96920996838687e+36

  for (n in seq_len(n.var)) {
    # define variable
    var.def <- ncdf4::ncvar_def(name = var.names[n],
                         units = var.units[n],
                         dim = (list(londim, latdim, timedim)),
                         fillvalue, long.names[n],
                         verbose = verbose,
                         prec = "float")

    # create netCD file for LPJ-GUESS
    ncfile <- ncdf4::nc_create(out.files.full[[n]], vars = var.def, force_v4 = TRUE)

    # put variable, rep(...,each=4) is a hack to write the same data for all grids (which all are the
    # same)
    ncdf4::ncvar_put(ncfile, var.def, rep(var.list[[n]], each = 4))

    # additional attributes for LPJ-GUESS
    ncdf4::ncatt_put(nc = ncfile, varid = var.names[n], attname = "standard_name", long.names[n])

    ncdf4::ncatt_put(nc = ncfile, varid = "lon", attname = "axis", "X")
    ncdf4::ncatt_put(nc = ncfile, varid = "lon", attname = "standard_name", "longitude")

    ncdf4::ncatt_put(nc = ncfile, varid = "lat", attname = "axis", "Y")
    ncdf4::ncatt_put(nc = ncfile, varid = "lat", attname = "standard_name", "latitude")

    ncdf4::ncatt_put(nc = ncfile, varid = "time", attname = "calendar", "gregorian")

    ncdf4::nc_close(ncfile)
  }

  ## close netcdf files
  sapply(ncin, ncdf4::nc_close)

  return(invisible(results))
} # met2model.LPJGUESS
