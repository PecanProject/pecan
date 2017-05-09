
##' upscale_met upscales the temporal resolution of a dataset
##' @name upscale_met
##' @title upscale_met
##' @export
##' @param input_met path to netcdf file containing met dataset
##' @param outfolder path to directory where output should be saved
##'           Output is netcdf with named as <input_met_filename>.upscaled.nc
##' @param resolution numeric value that is the desired resolution for the dataset to be upscaled to
##' @param reso_unit character units for the resolution timestep.
##'                   Must be interpretable as a time unit by \code{\link[udunits2]{ud.convert}}
##' @param overwrite logical: replace output file if it already exists?
##' @param verbose logical: should \code{\link[ncdf4:ncdf4-package]{ncdf4}} functions print debugging information as they run?
##' @author James Simkins, Chris Black

upscale_met <- function(outfolder, input_met, resolution = 6, reso_unit = "hours", overwrite = FALSE,
                        verbose = FALSE, ...) {
  tem <- ncdf4::nc_open(input_met)
  dim <- tem$dim
  met_data <- list()
  met_units <- list()
  for (v in names(tem$var)) {
    met_data[[v]] <- ncdf4::ncvar_get(nc = tem, varid = v)
    met_units[[v]] <- ncdf4::ncatt_get(nc = tem, varid = v, attname = "units")$value
  }
  met_data <- data.frame(met_data)

  time_unit <- sub(" since.*", "", tem$dim$time$units)
  time_base <- lubridate::parse_date_time(sub(".*since ", "", tem$dim$time$units),
                                          orders = c("ymdHMSz", "ymdHMS", "ymd"))
  time_data <- udunits2::ud.convert(tem$dim$time$vals, time_unit, reso_unit)

  lat_data <- as.numeric(ncdf4::ncvar_get(tem, "latitude"))
  lon_data <- as.numeric(ncdf4::ncvar_get(tem, "longitude"))
  ncdf4::nc_close(tem)
  
  reso_len <- diff(range(time_data)) %/% resolution
  step <- nrow(met_data) %/% reso_len
  met_data <- met_data[1:(step*reso_len),]
  upscaled_time = colMeans(matrix(time_data[1:(step*reso_len)], nrow=step))
  upscale_data <- data.frame()
  for (n in names(met_data)) {
    upscale_data[1:reso_len,n] <- colMeans(matrix(met_data[[n]], nrow=step))
  }
  
  if (!is.null(upscale_data$air_temperature)
      && is.null(upscale_data$air_temperature_max)
      && is.null(upscale_data$air_temperature_min)) {
    for (x in 1:reso_len) {
      upscale_data$air_temperature_max[x] <- max(
        met_data$air_temperature[(x * step - step + 1):(x * step)])
      upscale_data$air_temperature_min[x] <- min(
        met_data$air_temperature[(x * step - step + 1):(x * step)])
    }
    met_units$air_temperature_max <- met_units$air_temperature_min <- met_units$air_temperature
  }

  lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat_data, 
                          create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon_data, 
                          create_dimvar = TRUE)
  time <- ncdf4::ncdim_def(name = "time", units = paste(reso_unit, "since", time_base),
                           vals = upscaled_time,
                           create_dimvar = TRUE, unlim = TRUE)
  dim <- list(lat, lon, time)
  
  upscale.list <- list()
  for (j in names(upscale_data)) {
    upscale.list[[j]] <- ncdf4::ncvar_def(name = j, units = met_units[[j]],
                                          dim = dim, missval = -999, verbose = verbose)
  }
  
  rows <- 1
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  results <- data.frame(file = character(rows), host = character(rows), mimetype = character(rows), 
                        formatname = character(rows), startdate = character(rows), enddate = character(rows), 
                        dbfile.name = paste("upscaled", sep = "."), stringsAsFactors = FALSE)
  
  loc.file = file.path(outfolder, paste("upscaled", basename(input_met), sep = "."))
  loc <- ncdf4::nc_create(filename = loc.file, vars = upscale.list, verbose = verbose)
  
  for (j in names(upscale_data)) {
    ncdf4::ncvar_put(nc = loc, varid = j, vals = upscale_data[[j]])
  }
  ncdf4::nc_close(loc)
  
  results$file <- loc.file
  results$host <- PEcAn.utils::fqdn()
  results$startdate <- time_base + udunits2::ud.convert(upscaled_time[[1]], reso_unit, "sec")
  results$enddate <- time_base + udunits2::ud.convert(upscaled_time[[nrow(upscale_data)]], reso_unit, "sec")
  results$mimetype <- "application/x-netcdf"
  results$formatname <- "CF Meteorology"
  
  return(invisible(results))
}
