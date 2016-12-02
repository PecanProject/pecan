substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

##' upscale_met upscales the temporal resolution of a dataset
##' @name upscale_met
##' @title upscale_met
##' @export
##' @param outfolder
##' @param input_met - met dataset
##' @param site.id
##' @param reso - numeric value in HOURS that is the desired resolution for the dataset to be upscaled to
##'               # might need to change this for monthly/yearly option later
##' @param overwrite
##' @param verbose
##' @author James Simkins

upscale_met <- function(outfolder, input_met, site.id, reso = 6, overwrite = FALSE, 
                        verbose = FALSE, ...) {
  
  
  sub_str <- substrRight(input_met, 7)
  year <- substr(sub_str, 1, 4)
  year <- as.numeric(year)
  
  # Variable names
  var <- data.frame(CF.name <- c("air_temperature", "air_temperature_max", "air_temperature_min", 
                                 "surface_downwelling_longwave_flux_in_air", "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                 "eastward_wind", "northward_wind", "specific_humidity", "precipitation_flux"), 
                    units <- c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", 
                               "m/s", "g/g", "kg/m2/s"))
  # Reading in the data
  met_data <- list()
  tem <- ncdf4::nc_open(input_met)
  dim <- tem$dim
  for (j in seq_along(var$CF.name)) {
    if (exists(as.character(var$CF.name[j]), tem$var) == FALSE) {
      met_data[[j]] <- NA
    } else {
      met_data[[j]] <- ncdf4::ncvar_get(tem, as.character(var$CF.name[j]))
    }
  }
  
  lat_data <- as.numeric(ncdf4::ncvar_get(tem, "latitude"))
  lon_data <- as.numeric(ncdf4::ncvar_get(tem, "longitude"))
  ncdf4::nc_close(tem)
  
  met_data <- data.frame(met_data)
  colnames(met_data) <- var$CF.name
  
  if (lubridate::leap_year(year) == TRUE) {
    sp <- 366
  }
  if (lubridate::leap_year(year) == FALSE) {
    sp <- 365
  }
  reso_len <- sp * 24/reso
  
  step <- nrow(met_data)/reso_len
  upscale_data <- data.frame()
  for (n in seq_along(var$CF.name)) {
    upscale_data[1:reso_len,n] <- colMeans(matrix(met_data[[n]], nrow=step))
  }
  
  colnames(upscale_data) <- var$CF.name
  for (x in 1:reso_len) {
    upscale_data$air_temperature_max[x] <- max(met_data$air_temperature[(x * step - 
                                                                           step + 1):(x * step)])
    upscale_data$air_temperature_min[x] <- min(met_data$air_temperature[(x * step - 
                                                                           step + 1):(x * step)])
  }
  
  upscale.list <- list()
  lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat_data, 
                          create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon_data, 
                          create_dimvar = TRUE)
  time <- ncdf4::ncdim_def(name = "time", units = "sec", vals = (1:reso_len) * 
                             reso * 3600, create_dimvar = TRUE, unlim = TRUE)
  dim <- list(lat, lon, time)
  
  for (j in seq_along(var$CF.name)) {
    upscale.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]), 
                                          units = as.character(var$units[j]), 
                                          dim = dim, missval = -999, verbose = verbose)
  }
  
  rows <- 1
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  results <- data.frame(file = character(rows), host = character(rows), mimetype = character(rows), 
                        formatname = character(rows), startdate = character(rows), enddate = character(rows), 
                        dbfile.name = paste("upscaled", sep = "."), stringsAsFactors = FALSE)
  
  loc.file = file.path(outfolder, paste("upscaled", year, "nc", sep = "."))
  loc <- ncdf4::nc_create(filename = loc.file, vars = upscale.list, verbose = verbose)
  
  for (j in seq_along(var$CF.name)) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = upscale_data[[j]])
  }
  ncdf4::nc_close(loc)
  
  results$file <- loc.file
  results$host <- PEcAn.utils::fqdn()
  results$startdate <- paste0(year, "-01-01 00:00:00", tz = "UTC")
  results$enddate <- paste0(year, "-12-31 23:59:59", tz = "UTC")
  results$mimetype <- "application/x-netcdf"
  results$formatname <- "CF Meteorology"
  
  return(invisible(results))
}
