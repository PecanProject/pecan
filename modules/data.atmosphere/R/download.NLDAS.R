##' Download NLDAS met data
##'
##' Download and convert single grid point NLDAS to CF single grid point from hydro1.sci.gsfc.nasa.gov using OPENDAP interface
##'
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param site_id
##' @param lat
##' @param lon
##' @export
##'
##' @author Christy Rollinson (with help from Ankur Desai)
download.NLDAS <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in,
                           overwrite = FALSE, verbose = FALSE, ...) {

  # Date stuff
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  site_id    <- as.numeric(site_id)
  outfolder  <- paste0(outfolder, "_site_", paste0(site_id %/% 1e+09, "-", site_id %% 1e+09))

  NLDAS_start <- 1980
  if (start_year < NLDAS_start) {
    PEcAn.logger::logger.severe(sprintf('Input year range (%d:%d) exceeds the NLDAS range (%d:present)',
                                       start_year, end_year,
                                       NLDAS_start))
  }

  lat.in <- as.numeric(lat.in)
  lon.in <- as.numeric(lon.in)
  dap_base <- "http://hydro1.sci.gsfc.nasa.gov/thredds/dodsC/NLDAS_FORA0125_H.002"
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)

  ylist <- seq(start_year, end_year, by = 1)
  rows <- length(ylist)
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows),
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = "NLDAS",
                        stringsAsFactors = FALSE)

  var <- data.frame(DAP.name = c("N2-m_above_ground_Temperature", "LW_radiation_flux_downwards_surface",
                                 "Pressure", "SW_radiation_flux_downwards_surface", "N10-m_above_ground_Zonal_wind_speed",
                                 "N10-m_above_ground_Meridional_wind_speed", "N2-m_above_ground_Specific_humidity", "Precipitation_hourly_total"),
                    DAP.dim = c(2, 1, 1, 1, 2, 2, 2, 1),
                    CF.name = c("air_temperature", "surface_downwelling_longwave_flux_in_air",
                                "air_pressure", "surface_downwelling_shortwave_flux_in_air", "eastward_wind", "northward_wind",
                                "specific_humidity", "precipitation_flux"),
                    units = c("Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "g/g", "kg/m2/s"))
  time.stamps <- seq(0, 2300, by = 100)
  for (i in seq_len(rows)) {
    year <- ylist[i]

    # figure out how many days we're working with If we have multiple years and we're not in the first
    # or last year, we're taking a whole year
    nday <- PEcAn.utils::days_in_year(year)
    if (rows > 1 & i != 1 & i != rows) {
      days.use <- 1:nday
    } else if (rows == 1) {
      # if we're working with only 1 year, lets only pull what we need to
      day1 <- lubridate::yday(start_date)
      # Now we need to check whether we're ending on the right day
      day2 <- lubridate::yday(end_date)
      days.use <- day1:day2
      nday <- length(days.use)  # Update nday
    } else if (i == 1) {
      # If this is the first of many years, we only need to worry about the start date
      day1 <- lubridate::yday(start_date)
      days.use <- day1:nday
      nday <- length(days.use)  # Update nday
    } else if (i == rows) {
      # If this is the last of many years, we only need to worry about the start date
      day2 <- lubridate::yday(end_date)
      days.use <- 1:day2
      nday <- length(days.use)  # Update nday
    }
    ntime <- nday * 24  # leap year or not;time slice (hourly)

    loc.file <- file.path(outfolder, paste("NLDAS", year, "nc", sep = "."))

    ## Create dimensions
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)
    time <- ncdf4::ncdim_def(name = "time", units = "sec",
                      vals = seq((min(days.use) + 1 - 1 / 24) * 24 * 360, (max(days.use) + 1 - 1/24) * 24 * 360, length.out = ntime),
                      create_dimvar = TRUE,
                      unlim = TRUE)
    dim <- list(lat, lon, time)

    var.list <- list()
    dat.list <- list()

    # Defining our dimensions up front
    for (j in 1:nrow(var)) {
      var.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]),
                                 units = as.character(var$units[j]),
                                 dim = dim,
                                 missval = -999,
                                 verbose = verbose)
      dat.list[[j]] <- array(NA, dim = c(length(lat.in), length(lon.in), ntime))  # Go ahead and make the arrays
    }
    names(var.list) <- names(dat.list) <- var$CF.name

    ## get data off OpenDAP
    for (j in seq_along(days.use)) {
      date.now <- as.Date(days.use[j], origin = as.Date(paste0(year - 1, "-12-31")))
      mo.now <- stringr::str_pad(lubridate::month(date.now), 2, pad = "0")
      day.mo <- stringr::str_pad(lubridate::day(date.now), 2, pad = "0")
      doy <- stringr::str_pad(days.use[j], 3, pad = "0")
      for (h in seq_along(time.stamps)) {
        hr <- stringr::str_pad(time.stamps[h], 4, pad = "0")
        dap_file <- paste0(dap_base, "/", year, "/", doy, "/", "NLDAS_FORA0125_H.A", year,
                           mo.now, day.mo, ".", hr, ".002.grb.ascii?")

        # Query lat/lon
        latlon <- RCurl::getURL(paste0(dap_file, "lat[0:1:223],lon[0:1:463]"))
        lat.ind <- gregexpr("lat", latlon)
        lon.ind <- gregexpr("lon", latlon)
        lats <- as.vector(utils::read.table(con <- textConnection(substr(latlon, lat.ind[[1]][3],
                                                                  lon.ind[[1]][3] - 1)), sep = ",", fileEncoding = "\n", skip = 1))
        lons <- as.vector(utils::read.table(con <- textConnection(substr(latlon, lon.ind[[1]][3],
                                                                  nchar(latlon))), sep = ",", fileEncoding = "\n", skip = 1))

        lat.use <- which(lats - 0.125 / 2 <= lat.in & lats + 0.125 / 2 >= lat.in)
        lon.use <- which(lons - 0.125 / 2 <= lon.in & lons + 0.125 / 2 >= lon.in)

        # Set up the query for all of the met variables
        dap_query <- ""
        for (v in seq_len(nrow(var))) {
          time.string <- ""
          for (i in seq_len(var$DAP.dim[v])) {
            time.string <- paste0(time.string, "[0:1:0]")
          }
          dap_query <- paste(dap_query,
                             paste0(var$DAP.name[v], time.string, "[", lat.use, "][", lon.use, "]"), sep = ",")
        }
        dap_query <- substr(dap_query, 2, nchar(dap_query))

        dap.out <- RCurl::getURL(paste0(dap_file, dap_query))
        for (v in seq_len(nrow(var))) {
          var.now <- var$DAP.name[v]
          ind.1   <- gregexpr(paste(var.now, var.now, sep = "."), dap.out)
          end.1   <- gregexpr(paste(var.now, "time", sep = "."), dap.out)
          dat.list[[v]][, , j * 24 - 24 + h] <-
            utils::read.delim(con <- textConnection(substr(dap.out,
                                                    ind.1[[1]][1], end.1[[1]][2])), sep = ",", fileEncoding = "\n")[1, 1]
        }  # end variable loop
      }  # end hour
    }  # end day
    ## change units of precip to kg/m2/s instead of hour accumulated precip
    dat.list[["precipitation_flux"]] <- dat.list[["precipitation_flux"]] / 3600

    ## put data in new file
    loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = verbose)
    for (j in seq_len(nrow(var))) {
      ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[j]])
    }
    ncdf4::nc_close(loc)

    results$file[i] <- loc.file
    results$host[i] <- PEcAn.remote::fqdn()
    results$startdate[i] <- paste0(year, "-01-01 00:00:00")
    results$enddate[i] <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i] <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"
  }

  return(invisible(results))
} # download.NLDAS
