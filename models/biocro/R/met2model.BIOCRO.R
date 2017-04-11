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
##' files are calld <in.path>/<in.prefix>.YYYY.cf
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
##' @importFrom PEcAn.data.atmosphere load.cfmet cfmet.downscale.time
##' @author Rob Kooper, David LeBauer
##-------------------------------------------------------------------------------------------------#
met2model.BIOCRO <- function(in.path, in.prefix, outfolder, overwrite = FALSE,
                             lat, lon, start_date, end_date, ...) {
  dir.create(file.path(outfolder), recursive = TRUE, showWarnings = FALSE)
  years_wanted <- lubridate::year(start_date):lubridate::year(end_date)

  res <- list()
  for (year in years_wanted) {
    yrstart = max(lubridate::ymd(start_date), lubridate::ymd(paste0(year, "-01-01")))
    yrend = min(lubridate::ymd(end_date), lubridate::ymd(paste0(year, "-12-31")))

    ncfile <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    csvfile <- file.path(outfolder, paste(in.prefix, yrstart, yrend, "csv", sep="."))

    if (file.exists(csvfile) && as.logical(overwrite) != TRUE){
      logger.warn(paste("Output file", csvfile, "already exists! Moving to next year."))
      next
    }

    met.nc <- ncdf4::nc_open(ncfile)
    tmp.met <- load.cfmet(met.nc, lat = lat, lon = lon,
                          start.date = yrstart, end.date = yrend)

    dt <- lubridate::as.period(mean(diff(tmp.met$date)))
    if (dt > lubridate::hours(1)) {
      tmp.met <- cfmet.downscale.time(cfmet = tmp.met, output.dt = 1)
    }

    met <- cf2biocro(tmp.met)
    utils::write.csv(met, file = csvfile, row.names = FALSE)

    res[[as.character(year)]] <- data.frame(
      file = csvfile,
      host = fqdn(),
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
##' @import PEcAn.utils
##' @importFrom PEcAn.data.atmosphere qair2rh sw2par par2ppfd
##' @importFrom data.table :=
##' @author David LeBauer
cf2biocro <- function(met, longitude = NULL, zulu2solarnoon = FALSE) {

  if ((!is.null(longitude)) & zulu2solarnoon) {
    solarnoon_offset <- udunits2::ud.convert(longitude/360, "day", "minute")
    met[, `:=`(solardate = date + lubridate::minutes(solarnoon_offset))]
  }
  if (!"relative_humidity" %in% colnames(met)) {
    if (all(c("air_temperature", "air_pressure", "specific_humidity") %in% colnames(met))) {
      rh <- qair2rh(qair = met$specific_humidity, temp = udunits2::ud.convert(met$air_temperature, 
                                                                    "Kelvin", "Celsius"), press = udunits2::ud.convert(met$air_pressure, "Pa", "hPa"))
      met <- cbind(met, relative_humidity = rh * 100)
    } else {
      logger.error("neither relative_humidity nor [air_temperature, air_pressure, and specific_humidity]", 
                   "are in met data")
    }
  }
  if (!"ppfd" %in% colnames(met)) {
    if ("surface_downwelling_shortwave_flux_in_air" %in% colnames(met)) {
      par <- sw2par(met$surface_downwelling_shortwave_flux_in_air)
      ppfd <- par2ppfd(par)
    } else {
      logger.error("Need either ppfd or surface_downwelling_shortwave_flux_in_air in met dataset")
    }
  }
  if (!"wind_speed" %in% colnames(met)) {
    if (all(c("northward_wind", "eastward_wind") %in% colnames(met))) {
      wind_speed <- sqrt(met$northward_wind^2 + met$eastward_wind^2)
    } else {
      logger.error("neither wind_speed nor both eastward_wind and northward_wind are present in met data")
    }
  }
  
  ## Convert RH from percent to fraction BioCro functions just to confirm
  if (met[, max(relative_humidity) > 1]) {
    met[, `:=`(relative_humidity = relative_humidity/100)]
  }
  newmet <- met[, list(year = lubridate::year(date),
                       doy = lubridate::yday(date),
                       hour = round(lubridate::hour(date) + lubridate::minute(date) / 60, 1),
                       SolarR = ppfd, 
                       Temp = udunits2::ud.convert(air_temperature, "Kelvin", "Celsius"), 
                       RH = relative_humidity, 
                       WS = wind_speed,
                       precip = udunits2::ud.convert(precipitation_flux, "s-1", "h-1"))][hour <= 23]
  return(as.data.frame(newmet))
}  # cf2biocro
