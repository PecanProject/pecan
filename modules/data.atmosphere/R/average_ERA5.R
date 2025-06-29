#' @description
#' This function helps to average the ERA5 data based on the start and end dates, and convert it to the GeoTIFF file.
#' @title Average_ERA5_2_GeoTIFF
#' 
#' @param start.date character: start point of when to average the data (e.g., 2012-01-01).
#' @param end.date character: end point of when to average the data (e.g., 2021-12-31).
#' @param in.path character: the directory where your ERA5 data stored (they should named as ERA5_YEAR.nc).
#' @param outdir character: the output directory where the averaged GeoTIFF file will be generated.
#'
#' @return character: path to the exported GeoTIFF file.
#' 
#' @export
#' @author Dongchen Zhang
Average_ERA5_2_GeoTIFF <- function (start.date, end.date, in.path, outdir) {
  # create dates.
  years <- sort(unique(lubridate::year(start.date):lubridate::year(end.date)))
  # initialize final outcomes.
  temp.all <- precip.all <- srd.all <- dewpoint.all <- c()
  # loop over years.
  for (i in seq_along(years)) {
    # open ERA5 nc file as geotiff format for referencing crs and ext.
    ERA5.tiff <- terra::rast(file.path(in.path, paste0("ERA5_", years[i], ".nc")))
    # open ERA5 nc file.
    met.nc <- ncdf4::nc_open(file.path(in.path, paste0("ERA5_", years[i], ".nc")))
    # find index for the date.
    times <- as.POSIXct(met.nc$dim$time$vals*3600, origin="1900-01-01 00:00:00", tz = "UTC")
    time.inds <- which(lubridate::date(times) >= start.date & lubridate::date(times) <= end.date)
    # extract temperature.
    PEcAn.logger::logger.info("entering temperature.")
    temp.all <- abind::abind(temp.all, apply(ncdf4::ncvar_get(met.nc, "t2m")[,,,time.inds], c(1,2,4), mean), along = 3)
    # extract precipitation.
    PEcAn.logger::logger.info("entering precipitation.")
    precip.all <- abind::abind(precip.all, apply(ncdf4::ncvar_get(met.nc, "tp")[,,,time.inds], c(1,2,4), mean), along = 3)
    # extract shortwave solar radiation.
    PEcAn.logger::logger.info("entering solar radiation.")
    srd.all <- abind::abind(srd.all, apply(ncdf4::ncvar_get(met.nc, "ssrd")[,,,time.inds], c(1,2,4), mean), along = 3)
    # extract dewpoint.
    PEcAn.logger::logger.info("entering dewpoint.")
    dewpoint.all <- abind::abind(dewpoint.all, apply(ncdf4::ncvar_get(met.nc, "d2m")[,,,time.inds], c(1,2,4), mean), along = 3)
    # close the NC connection.
    ncdf4::nc_close(met.nc)
  }
  # aggregate across time.
  # temperature.
  temp <- apply(temp.all, c(1, 2), mean)
  temp <- PEcAn.utils::ud_convert(temp, "K", "degC")
  # precipitation.
  precip <- apply(precip.all, c(1, 2), mean)
  # solar radiation.
  srd <- apply(srd.all, c(1, 2), mean)
  # dewpoint.
  dewpoint <- apply(dewpoint.all, c(1, 2), mean)
  dewpoint <- PEcAn.utils::ud_convert(dewpoint, "K", "degC")
  # convert dew point to relative humidity.
  beta <- (112 - (0.1 * temp) + dewpoint) / (112 + (0.9 * temp))
  relative.humidity <- beta ^ 8
  VPD <- get.vpd(100*relative.humidity, temp)
  # combine together.
  PEcAn.logger::logger.info("Aggregate maps.")
  met.rast <- c(terra::rast(matrix(temp, nrow = dim(temp)[2], ncol = dim(temp)[1], byrow = T)),
                terra::rast(matrix(precip, nrow = dim(precip)[2], ncol = dim(precip)[1], byrow = T)),
                terra::rast(matrix(srd, nrow = dim(srd)[2], ncol = dim(srd)[1], byrow = T)),
                terra::rast(matrix(VPD, nrow = dim(VPD)[2], ncol = dim(VPD)[1], byrow = T)))
  # adjust crs and extents.
  terra::crs(met.rast) <- terra::crs(ERA5.tiff)
  terra::ext(met.rast) <- terra::ext(ERA5.tiff)
  names(met.rast) <- c("temp", "prec", "srad", "vapr")
  # write into geotiff file.
  terra::writeRaster(met.rast, file.path(outdir, paste0("ERA5_met_", lubridate::year(end.date), ".tiff")))
  # end.
  gc()
  return(file.path(outdir, paste0("ERA5_met_", lubridate::year(end.date), ".tiff")))
}