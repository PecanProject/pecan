#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

.datatable.aware <- TRUE
##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are called <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.BIOCRO
##' @title Write BioCro met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written
##' @param lat,lon Site latitude and longitude
##' @param start_date,end_date Date range to convert. Each year will be written to a separate file
##' @param overwrite logical: Write over any existing file of the same name? If FALSE, leaves the existing file untouched and skips to the next year.
##' @param ... other arguments passed from PEcAn, currently ignored
##' @return a dataframe of information about the written file
##' @export
##' @author Rob Kooper, David LeBauer
##-------------------------------------------------------------------------------------------------#
met2model.BIOCRO <- function(in.path, in.prefix, outfolder, overwrite = FALSE,
                             lat, lon, start_date, end_date, ...) {
  start_date <- lubridate::parse_date_time(start_date, tz = "UTC",
                                           orders = c("ymdHMSz", "ymdHMS", "ymdH", "ymd"))
  end_date <- lubridate::parse_date_time(end_date, tz = "UTC",
                                           orders = c("ymdHMSz", "ymdHMS", "ymdH", "ymd"))
  dir.create(file.path(outfolder), recursive = TRUE, showWarnings = FALSE)
  years_wanted <- lubridate::year(start_date):lubridate::year(end_date)

  res <- list()
  for (year in years_wanted) {
    yrstart = max(lubridate::date(start_date), lubridate::ymd(paste0(year, "-01-01")))
    yrend = min(lubridate::date(end_date), lubridate::ymd(paste0(year, "-12-31")))

    ncfile <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    csvfile <- file.path(outfolder, paste(in.prefix, year, "csv", sep = "."))

    if (file.exists(csvfile) && as.logical(overwrite) != TRUE){
      PEcAn.logger::logger.warn(paste("Output file", csvfile, "already exists! Moving to next year."))
      next
    }

    met.nc <- ncdf4::nc_open(ncfile)
    on.exit(close_nc_if_open(met.nc), add = FALSE)
      # add = FALSE because any previous file was closed at end of prev. loop

    dt <- mean(diff(PEcAn.utils::ud_convert(
      met.nc$dim$time$vals,
      met.nc$dim$time$units,
      "hours since 1700-01-01 00:00:00")))
    if (dt < 1) {
      # More than one obs/hour. Write upscaled hourly file and reload.
      ncdf4::nc_close(met.nc)

      upscale_result <- PEcAn.data.atmosphere::upscale_met(
        outfolder = outfolder, input_met = ncfile,
        site.id = in.prefix, resolution = 1/24,
        overwrite = overwrite)

      met.nc <- ncdf4::nc_open(upscale_result$file)
    }

    tmp.met <- PEcAn.data.atmosphere::load.cfmet(
      met.nc, lat = lat, lon = lon,
      start.date = yrstart, end.date = yrend)

    # NB we need this nc_close even though on.exit is set:
    # close here to avoid leaking filehandle at end of loop iteration,
    # close in on.exit to avoid leaking at early function exit.
    ncdf4::nc_close(met.nc)

    if (dt > 1) {
      # Data have fewer than 1 obs/hour. Need to downscale.
      # Unlike upscale, downscale returns result directly--No file needed.
      tmp.met <- PEcAn.data.atmosphere::cfmet.downscale.time(cfmet = tmp.met, output.dt = 1)
    }

    met <- cf2biocro(tmp.met)
    utils::write.csv(met, file = csvfile, row.names = FALSE)

    res[[as.character(year)]] <- data.frame(
      file = csvfile,
      host = PEcAn.remote::fqdn(),
      mimetype = "text/csv",
      formatname = "biocromet",
      startdate = yrstart,
      enddate = yrend,
      dbfile.name = in.prefix,
      stringsAsFactors = FALSE)
  }

  result <- do.call("rbind", res)
  return(result)
}  # met2model.BIOCRO


##-------------------------------------------------------------------------------------------------#
##' Converts a CF data frame into a BioCro met input
##'
##' @name cf2biocro
##' @title Convert CF-formatted met data to BioCro met
##' @param met data.table object  with met for a single site; output from \code{\link{load.cfmet}}
##' \itemize{
##' \item {year} {int}
##' \item {month} {int}
##' \item {day} {int: day of month (1-31)}
##' \item {doy} {int: day of year (1-366)}
##' \item {hour} {int (0-23)}
##' \item {date} {YYYY-MM-DD HH:MM:SS POSIXct}
##' \item {wind_speed} {num m/s}
##' \item {northward_wind}
##' \item {eastward_wind}
##' \item {ppfd} {optional; if missing, requires surface_downwelling_shortwave_flux_in_air}
##' \item {surface_downwelling_shortwave_flux_in_air}
##' \item {air_pressure (Pa)} {optional; if missing, requires relative_humidity}
##' \item {specific_humidity} {optional; if missing, requires relative_humidity}
##' \item {relative_humidity} {optional; if missing, requires air_pressure and specific_humidity}
##' \item {precipitation_flux}
##' \item {air_temperature}
##' }
##' @param longitude in degrees east, used for calculating solar noon
##' @param zulu2solarnoon logical; if TRUE, convert time from GMT to local solar time.
##' @return data.table / data.frame with fields
##' \itemize{
##' \item {doy} {day of year}
##' \item {hr} {hour}
##' \item {solar} {solar radiation (PPFD)}
##' \item {temp} {temperature, degrees celsius}
##' \item {rh} {relative humidity, as fraction (0-1)}
##' \item {windspeed} {m/s}
##' \item {precip} {cm/h}
##' }
##' @export cf2biocro
##' @importFrom data.table :=
##' @author David LeBauer
cf2biocro <- function(met, longitude = NULL, zulu2solarnoon = FALSE) {

  if (!data.table::is.data.table(met)) {
    met <- data.table::copy(met)
    data.table::setDT(met)
  }

  if ((!is.null(longitude)) & zulu2solarnoon) {
    solarnoon_offset <- PEcAn.utils::ud_convert(longitude/360, "day", "minute")
    met[, `:=`(solardate = met$date + lubridate::minutes(solarnoon_offset))]
  }
  if (!"relative_humidity" %in% colnames(met)) {
    if (all(c("air_temperature", "air_pressure", "specific_humidity") %in% colnames(met))) {
      rh <- PEcAn.data.atmosphere::qair2rh(
        qair = met$specific_humidity,
        temp = PEcAn.utils::ud_convert(met$air_temperature, "Kelvin", "Celsius"),
        press = PEcAn.utils::ud_convert(met$air_pressure, "Pa", "hPa"))
      met[, `:=`(relative_humidity = rh)]
    } else {
      PEcAn.logger::logger.error("neither relative_humidity nor [air_temperature, air_pressure, and specific_humidity]", 
                   "are in met data")
    }
  }
  if (!"ppfd" %in% colnames(met)) {
    if ("surface_downwelling_photosynthetic_photon_flux_in_air" %in% colnames(met)) {
      ppfd <- PEcAn.utils::ud_convert(met$surface_downwelling_photosynthetic_photon_flux_in_air, "mol", "umol")
    } else if ("surface_downwelling_shortwave_flux_in_air" %in% colnames(met)) {
      par <- PEcAn.data.atmosphere::sw2par(met$surface_downwelling_shortwave_flux_in_air)
      ppfd <- PEcAn.data.atmosphere::par2ppfd(par)
    } else {
      PEcAn.logger::logger.error("Need either ppfd or surface_downwelling_shortwave_flux_in_air in met dataset")
    }
  }
  if (!"wind_speed" %in% colnames(met)) {
    if (all(c("northward_wind", "eastward_wind") %in% colnames(met))) {
      wind_speed <- sqrt(met$northward_wind^2 + met$eastward_wind^2)
    } else {
      PEcAn.logger::logger.error("neither wind_speed nor both eastward_wind and northward_wind are present in met data")
    }
  }
  
  ## Convert RH from percent to fraction BioCro functions just to confirm
  if (max(met$relative_humidity) > 1) {
    met[, `:=`(relative_humidity = met$relative_humidity/100)]
  }
  newmet <- met[, list(year = lubridate::year(met$date),
                       doy = lubridate::yday(met$date),
                       hour = round(lubridate::hour(met$date) + lubridate::minute(met$date) / 60, 0),
                       solar = ppfd,
                       Temp = PEcAn.utils::ud_convert(met$air_temperature, "Kelvin", "Celsius"),
                       RH = met$relative_humidity,
                       windspeed = wind_speed,
                       precip = PEcAn.utils::ud_convert(met$precipitation_flux, "s-1", "h-1"))]
  newmet <- newmet[newmet$hour <= 23,]
  return(as.data.frame(newmet))
}  # cf2biocro
