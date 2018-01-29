##' take mean at fixed intervals along a vector
##' @param x numeric vector
##' @param step integer step size
##' @return numeric of length length(x)/step
##' @details User should check that length(x) is an even multiple of step
step_means <- function(x, step){
  colMeans(matrix(x, nrow = step))
}

##' upscale_met upscales the temporal resolution of a dataset
##' @name upscale_met
##' @title upscale_met
##' @export
##' @param input_met path to netcdf file containing met dataset
##' @param outfolder path to directory where output should be saved
##'           Output is netcdf named as <input_met_filename>.upscaled.nc
##' @param resolution desired output resolution, in days
##' @param overwrite logical: replace output file if it already exists?
##' @param verbose logical: should \code{\link[ncdf4:ncdf4-package]{ncdf4}} functions print debugging information as they run?
##' @param ... other arguments, currently ignored
##' @author James Simkins, Chris Black

upscale_met <- function(outfolder, input_met, resolution = 1/24, overwrite = FALSE,
                        verbose = FALSE, ...) {

  loc.file = file.path(outfolder, paste("upscaled", basename(input_met), sep = "."))
  if (file.exists(loc.file) && !isTRUE(overwrite)){
   PEcAn.logger::logger.severe("Output file", loc.file, "already exists. To replace it, set overwrite = TRUE")
  }

  met_lookup <- read.csv(system.file("/data/met.lookup.csv", package = "PEcAn.data.atmosphere"),
                         header = TRUE, stringsAsFactors = FALSE)
  tem <- ncdf4::nc_open(input_met)
  dim <- tem$dim
  met_data <- list()
  met_units <- list()
  for (name in names(tem$var)) {
    if (!(name %in% met_lookup$CF_standard_name)) {
      next
    }
    met_data[[name]] <- ncdf4::ncvar_get(nc = tem, varid = name)
    met_units[[name]] <- ncdf4::ncatt_get(nc = tem, varid = name, attname = "units")$value
  }
  met_data <- data.frame(met_data)

  time_unit <- sub(" since.*", "", tem$dim$time$units)
  time_base <- lubridate::parse_date_time(sub(".*since ", "", tem$dim$time$units),
                                          orders = c("ymdHMSz", "ymdHMS", "ymd"))
  time_data <- udunits2::ud.convert(tem$dim$time$vals, time_unit, "days")

  lat_data <- as.numeric(ncdf4::ncvar_get(tem, "latitude"))
  lon_data <- as.numeric(ncdf4::ncvar_get(tem, "longitude"))
  ncdf4::nc_close(tem)
  
  # Here's where the magic happens: find the stepsize that generates requested
  # output resolution, then take means of each variable in increments of stepsize.
  # N.B. Drops rows from the end  of met_data if necessary to end at a full step.
  n_times <- diff(range(time_data)) / resolution
  stepsize <- round(nrow(met_data) / n_times, 0)
  rows_used <- nrow(met_data) - (nrow(met_data) %% stepsize)
  n_steps <- (rows_used %/% stepsize)
  met_data <- met_data[seq_len(rows_used),]
  upscaled_time = step_means(time_data[seq_len(rows_used)], step = stepsize)
  upscale_data <- as.data.frame(lapply(met_data, step_means, step = stepsize))
  
  if (!is.null(upscale_data$air_temperature)
      && is.null(upscale_data$air_temperature_max)
      && is.null(upscale_data$air_temperature_min)) {
    for (step_i in seq_len(n_steps)) {
      upscale_data$air_temperature_max[step_i] <- max(
        met_data$air_temperature[(step_i * stepsize - stepsize + 1):(step_i * stepsize)])
      upscale_data$air_temperature_min[step_i] <- min(
        met_data$air_temperature[(step_i * stepsize - stepsize + 1):(step_i * stepsize)])
    }
    met_units$air_temperature_max <- met_units$air_temperature_min <- met_units$air_temperature
  }

  lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat_data, 
                          create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon_data, 
                          create_dimvar = TRUE)
  time <- ncdf4::ncdim_def(name = "time", units = paste(time_unit, "since", time_base),
                           vals = udunits2::ud.convert(upscaled_time, "days", time_unit),
                           create_dimvar = TRUE, unlim = TRUE)
  dim <- list(lat, lon, time)
  
  upscale.list <- list()
  for (name in names(upscale_data)) {
    upscale.list[[name]] <- ncdf4::ncvar_def(name = name, units = met_units[[name]],
                                          dim = dim, missval = -999, verbose = verbose)
  }
  
  rows <- 1
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  results <- data.frame(file = character(rows), host = character(rows), mimetype = character(rows), 
                        formatname = character(rows), startdate = character(rows), enddate = character(rows), 
                        dbfile.name = paste("upscaled", sep = "."), stringsAsFactors = FALSE)
  
  loc <- ncdf4::nc_create(filename = loc.file, vars = upscale.list, verbose = verbose)
  
  for (name in names(upscale_data)) {
    ncdf4::ncvar_put(nc = loc, varid = name, vals = upscale_data[[name]])
  }
  ncdf4::nc_close(loc)
  
  results$file <- loc.file
  results$host <- PEcAn.remote::fqdn()
  results$startdate <- time_base + udunits2::ud.convert(upscaled_time[[1]], "days", "sec")
  results$enddate <- time_base + udunits2::ud.convert(upscaled_time[[nrow(upscale_data)]], "days", "sec")
  results$mimetype <- "application/x-netcdf"
  results$formatname <- "CF Meteorology"
  
  return(invisible(results))
}
