
#' Convert geostreams JSON to CF met file
#' 
#' @param in.path directory containing Geostreams JSON file(s) to be converted
#' @param in.prefix initial portion of input filenames (everything before the dates)
#' @param outfolder directory where nc output files should be written. Will be created if necessary
#' @param start_date,end_date beginning and end of run, YYYY-MM-DD.
#' @param overwrite logical: Regenerate existing files of the same name?
#' @param verbose logical, passed on to \code{\link[ncdf4]{nc_create}} 
#'  to control how chatty it should be during netCDF creation
#' @param ... other arguments, currently ignored
#' @export
#' @author Harsh Agrawal, Chris Black
met2CF.Geostreams <- function(in.path, in.prefix, outfolder, 
                    start_date, end_date, 
                    overwrite = FALSE, verbose = FALSE, ...) {

  met.lookup = read.csv(system.file("/data/met.lookup.csv", package = "PEcAn.data.atmosphere"),
                        header = TRUE, stringsAsFactors = FALSE)

  start_date <- as.POSIXct(start_date, tz="UTC")
  end_date <- as.POSIXct(end_date, tz="UTC")

  in_file <- file.path(in.path, paste(in.prefix, start_date, end_date, "json", sep = "."))
  dat <- jsonlite::read_json(in_file, simplifyVector = TRUE, flatten = TRUE)
  vars <- unlist(dat$sensor_info$parameters)
  # TODO future versions of geostreams API may add units for each param
  # if/when they are available, add code to check them
  dat <- dat$data
  names(dat) <- sub("^properties\\.", "", names(dat))

  dat$start_time <- lubridate::parse_date_time(dat$start_time, orders = "ymdHMSz", tz = "UTC")
  dat$end_time <- lubridate::parse_date_time(dat$end_time, orders = "ymdHMSz", tz = "UTC")
  if (start_date < min(dat$start_time) | end_date > max(dat$end_time)) {
   logger.severe("Requested dates", start_date, "--", end_date,
                 "fall outside available data", min(dat$start_time), "--", max(dat$end_time))
  }

  dat$mid_time <- dat$start_time + (dat$end_time - dat$start_time)/2
  dat <- dat[dat$start_time >= start_date & dat$end_time <= end_date,]
  secs_elapsed <- unclass(dat$mid_time) - unclass(start_date)
  days_elapsed <- udunits2::ud.convert(secs_elapsed, "sec", "days")
  
  ref_time_str <- strftime(start_date, format = "%FT%TZ")
  time <- ncdf4::ncdim_def(name = "time", units = paste("days since", ref_time_str),
                           vals = days_elapsed, create_dimvar = TRUE, unlim = TRUE)

  if (length(unique(dat$geometry.coordinates)) == 1) {
    # all lat/lons are are identical-- no need to store extra copies
    raw_lat <- dat$geometry.coordinates[[1]][[1]]
    raw_lon <- dat$geometry.coordinates[[1]][[2]]
  } else {
    # multiple coords in same file -- keep lat and lon as full-length vectors
    raw_lat <- sapply(dat$geometry.coordinates, function(x)x[[1]])
    raw_lon <- sapply(dat$geometry.coordinates, function(x)x[[2]])
  }
  
  lat <- ncdf4::ncdim_def(name = "latitude", units = "degrees_north", vals = raw_lat, create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degrees_east", vals = raw_lon, create_dimvar = TRUE)

  cf_dims <- list(lat, lon, time)

  # Hacky switch between naming modes:
  # Input data may be any length, including partial years, and files are named
  # `in.prefix.START_DATE.END_DATE.json`.
  # But many downstream PEcAn fxns expect CF files to be exactly one year long and named
  # `in.prefix.YEAR.nc`.
  # To support both with hopefully minimal confusion, we name the file in PEcAn standard style
  # (in.prefix.YEAR.nc) iff input is one full year long,
  # otherwise we name it with full dates (in.prefix.START_DATE.END_DATE.nc)
  if (lubridate::year(start_date) == lubridate::year(end_date)
      && lubridate::yday(start_date) == 1
      && (lubridate::month(end_date) == 12 && lubridate::mday(end_date) == 31)) {
    nc_date <- lubridate::year(start_date)
  } else {
    nc_date <- paste(start_date, end_date, sep=".")
  }

  nc.file <- file.path(outfolder, paste(in.prefix, nc_date, "nc", sep = "."))
  dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
  if (!overwrite &&  file.exists(nc.file)) {
    logger.severe("Refusing to overwrite existing file", nc.file, " -- If you're sure, set overwrite=TRUE")
  }

  vars <- vars[vars != "source_file"] # This var is metadata, not met data
  if ("precipitation_rate" %in% vars) {
    # rate is in mm/sec, flux is in kg m^-2 sec^-1, conversion factor is 1.
    vars <- sub("precipitation_rate", "precipitation_flux", vars)
    names(dat) <- sub("precipitation_rate", "precipitation_flux", names(dat))
  }

  make_ncvar <- function(name){
    if (! name %in% met.lookup$CF_standard_name) {
      logger.severe("Don't know how to convert parameter", name, "to CF standard format")
    }
    unit <- met.lookup[met.lookup$CF_standard_name == name, "units"]
    ncdf4::ncvar_def(name = name,
                     units = unit,
                     dim = cf_dims,
                     missval = -999,
                     verbose = verbose)
  }
  var_list = lapply(vars, make_ncvar)
  
  cf <- ncdf4::nc_create(filename = nc.file, vars = var_list, verbose = verbose)

  for (var in var_list) {
    ncdf4::ncvar_put(nc = cf,
                     varid = var,
                     vals = get(var$name, dat),
                     verbose = verbose)
  }
  ncdf4::nc_close(cf)

  return(data.frame(file = nc.file,
                    host = PEcAn.utils::fqdn(),
                    startdate = start_date,
                    enddate = end_date,
                    mimetype = "application/x-netcdf",
                    formatname = "CF Meteorology",
                    dbfile.name = in.prefix,
                    stringsAsFactors = FALSE))

}
