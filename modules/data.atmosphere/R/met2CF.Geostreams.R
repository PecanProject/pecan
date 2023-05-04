
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

  start_date <- as.POSIXct(start_date, tz="UTC")
  end_date <- as.POSIXct(end_date, tz="UTC")

  out_files = c()
  for (year in lubridate::year(start_date):lubridate::year(end_date)) {
    in_file <- file.path(in.path, paste(in.prefix, year, "json", sep = "."))
    dat <- jsonlite::read_json(in_file, simplifyVector = TRUE, flatten = TRUE)
    vars <- unlist(dat$sensor_info$parameters)
    # TODO future versions of geostreams API may add units for each param
    # if/when they are available, add code to check them
    dat <- dat$data
    names(dat) <- sub("^properties\\.", "", names(dat))

    dat$start_time <- lubridate::parse_date_time(dat$start_time, orders = "ymdHMSz", tz = "UTC")
    dat$end_time <- lubridate::parse_date_time(dat$end_time, orders = "ymdHMSz", tz = "UTC")
    if (year == lubridate::year(start_date) & start_date < min(dat$start_time)) {
      PEcAn.logger::logger.severe(
        "Requested start date is", start_date,
        "but", year, "data begin on", min(dat$start_time))
    }
    if (year == lubridate::year(end_date) & end_date > max(dat$end_time)) {
      PEcAn.logger::logger.severe(
        "Requested end date is", end_date,
        "but", year, "data end on", max(dat$end_time))
    }

    dat$mid_time <- dat$start_time + (dat$end_time - dat$start_time)/2
    dat <- dat[dat$start_time >= start_date & dat$end_time <= end_date,]
    secs_elapsed <- unclass(dat$mid_time) - unclass(start_date)
    days_elapsed <- PEcAn.utils::ud_convert(secs_elapsed, "sec", "days")
    
    ref_time_str <- strftime(start_date, format = "%FT%TZ")
    time <- ncdf4::ncdim_def(name = "time", units = paste("days since", ref_time_str),
                             vals = days_elapsed, create_dimvar = TRUE, unlim = TRUE)

    if (length(unique(dat$geometry.coordinates)) == 1) {
      # all lat/lons are are identical-- no need to store extra copies
      raw_lon <- dat$geometry.coordinates[[1]][[1]]
      raw_lat <- dat$geometry.coordinates[[1]][[2]]
    } else {
      # multiple coords in same file -- keep lat and lon as full-length vectors
      raw_lon <- sapply(dat$geometry.coordinates, function(x)x[[1]])
      raw_lat <- sapply(dat$geometry.coordinates, function(x)x[[2]])
    }
    
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degrees_north", vals = raw_lat, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degrees_east", vals = raw_lon, create_dimvar = TRUE)

    cf_dims <- list(lat, lon, time)

    vars <- vars[vars != "source_file"] # This var is metadata, not met data
    if ("precipitation_rate" %in% vars) {
      # rate is in mm/sec, flux is in kg m^-2 sec^-1, conversion factor is 1.
      vars <- sub("precipitation_rate", "precipitation_flux", vars)
      names(dat) <- sub("precipitation_rate", "precipitation_flux", names(dat))
    }

    make_ncvar <- function(name){
      if (! name %in% pecan_standard_met_table$cf_standard_name) {
       PEcAn.logger::logger.severe("Don't know how to convert parameter", name, "to CF standard format")
      }
      unit <- pecan_standard_met_table[pecan_standard_met_table$cf_standard_name == name, "units"]
      ncdf4::ncvar_def(name = name,
                       units = unit,
                       dim = cf_dims,
                       missval = -999,
                       verbose = verbose)
    }
    var_list = lapply(vars, make_ncvar)

    dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
    nc.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep = "."))
    if (!overwrite &&  file.exists(nc.file)) {
     PEcAn.logger::logger.severe("Refusing to overwrite existing file", nc.file, " -- If you're sure, set overwrite=TRUE")
    }
    cf <- ncdf4::nc_create(filename = nc.file, vars = var_list, verbose = verbose)
    for (var in var_list) {
      ncdf4::ncvar_put(nc = cf,
                       varid = var,
                       vals = get(var$name, dat),
                       verbose = verbose)
    }
    ncdf4::nc_close(cf)
    out_files <- append(out_files, nc.file)
  }

  return(data.frame(file = out_files,
                    host = PEcAn.remote::fqdn(),
                    startdate = start_date,
                    enddate = end_date,
                    mimetype = "application/x-netcdf",
                    formatname = "CF Meteorology",
                    dbfile.name = in.prefix,
                    stringsAsFactors = FALSE))
}
