
#' Convert geostreams JSON to CF met file
#' 
#' @param in.path directory containing Geostreams JSON file(s) to be converted
#' @param in.prefix initial portion of input filenames (everything before the dates)
#' @param outfolder directory where nc output files should be written. Will be created if necessary
#' @param overwrite logical: Regenerate existing files of the same name?
#' @param verbose logical, passed on to \code{\link[ncdf4]{nc_create}} 
#'  to control how chatty it should be during netCDF creation
#' @param ... other arguments, currently ignored
#' @export
#' @author Harsh Agrawal, Chris Black
met2cf.geostreams <- function(in.path, in.prefix, outfolder, 
                    start_date, end_date, 
                    overwrite = FALSE, verbose = FALSE, ...) {

  # TODO confirm all units / variable names match geostreams spec,
  #   and that output names / units match CF convention
  vars <- read.csv(
    text = "name, units
      air_temperature, K
      relative_humidity, %
      wind_speed, m/s
      precipitation_rate, mm/s
      air_pressure, Pa
      surface_downwelling_photosynthetic_photon_flux_in_air, mol m-2 s-1
      northward_wind, degrees
      eastward_wind, degrees",
    header=TRUE,
    strip.white = TRUE,
    stringsAsFactors = FALSE)

  start_date <- as.POSIXct(start_date, tz="UTC")
  end_date <- as.POSIXct(end_date, tz="UTC")

  in_file <- file.path(in.path, paste(in.prefix, start_date, end_date, "json", sep = "."))
  dat <- jsonlite::read_json(in_file, simplifyVector = TRUE, flatten = FALSE)

  
  dat$start_time <- as.POSIXct(dat$start_time, tz="UTC")
  dat$end_time <- as.POSIXct(dat$end_time, tz="UTC")
  if (start_date < min(dat$start_time) | end_date > max(dat$end_time)) {
    logger.severe("Requested dates", start_date, "--", end_date,
                  "fall outside available data", min(dat$start_time), "--", max(dat$end_time))
  }

  if (isTRUE(all.equal(dat$start_time, dat$start_time))) {
    dat$mid_time = dat$start_time
  } else {
    dat$mid_time <- dat$start_time + (dat$end_time - dat$start_time)/2
  }
  dat <- dat[dat$start_time >= start_date & dat$end_time <= end_date,]
  secs_elapsed <- unclass(dat$mid_time) - unclass(start_date)
  days_elapsed <- udunits2::ud.convert(secs_elapsed, "sec", "days")
  
  ref_time_str <- strftime(start_date, format = "%FT%TZ")
  time <- ncdf4::ncdim_def(name = "time", units = paste("days since", ref_time_str),
                           vals = days_elapsed, create_dimvar = TRUE, unlim = TRUE)

  if (length(unique(dat$geometry$coordinates)) == 1) {
    # all lat/lons are are identical-- no need to store extra copies
    raw_lat <- dat$geometry$coordinates[[1]][[1]]
    raw_lon <- dat$geometry$coordinates[[1]][[2]]
  } else {
    # multiple coords in same file -- keep lat and lon as full-length vectors
    raw_lat <- sapply(dat$geometry$coordinates, function(x)x[[1]])
    raw_lon <- sapply(dat$geometry$coordinates, function(x)x[[2]])
  }
  
  lat <- ncdf4::ncdim_def(name = "latitude", units = "degrees_north", vals = raw_lat, create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degrees_east", vals = raw_lon, create_dimvar = TRUE)

  cf_dims <- list(lat, lon, time)


  nc.file <- file.path(outfolder, paste(in.prefix, start_date, end_date, "nc", sep = "."))
  if (!overwrite &&  file.exists(nc.file)) {
    logger.severe("Refusing to overwrite existing file", nc.file, " -- If you're sure, set overwrite=FALSE")
  }

  var_list = list()
  for (i in seq_len(nrow(vars))) {
    # TODO check file for any vars NOT in predefined list. How to handle?
    var_list[[i]] <- ncdf4::ncvar_def(name = vars$name[[i]], 
                                      units = vars$units[[i]], 
                                      dim = cf_dims, 
                                      missval = -999, 
                                      verbose = verbose)
  }
  
  cf <- ncdf4::nc_create(filename = nc.file, vars = var_list, verbose = verbose)

  for (i in seq_len(nrow(vars))) {
    vi = vars$name[[i]]
    ncdf4::ncvar_put(nc = cf,
                     varid = vi,
                     vals = dat$properties[[vi]],
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