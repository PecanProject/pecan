#' Download MERRA data
#'
#' @inheritParams download.CRUNCEP
#' @param ... Not used -- silently soak up extra arguments from `convert_input`, etc.
#' @return `data.frame` of meteorology data metadata
#' @author Alexey Shiklomanov
#' @export
download.MERRA <- function(outfolder, start_date, end_date,
                           lat.in, lon.in,
                           overwrite = FALSE,
                           verbose = FALSE,
                           ...) {

  dates <- seq.Date(as.Date(start_date), as.Date(end_date), "1 day")

  if(!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }

  # Download all MERRA files first. This skips files that have already been downloaded.
  for (i in seq_along(dates)) {
    date <- dates[[i]]
    PEcAn.logger::logger.debug(paste0(
      "Downloading ", as.character(date), " (", i, " of ", length(dates), ")"
    ))
    get_merra_date(date, lat.in, lon.in, outfolder, overwrite = overwrite)
  }

  # Now, post-process
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  ylist <- seq(start_year, end_year)

  nyear <- length(ylist)
  results <- data.frame(
    file = character(nyear),
    host = "",
    mimetype = "",
    formatname = "",
    startdate = "",
    enddate = "",
    dbfile.name = "MERRA",
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nyear)) {
    year <- ylist[i]
    baseday <- paste0(year, "-01-01T00:00:00Z")

    # Accommodate partial years
    y_startdate <- pmax(ISOdate(year, 01, 01, 0, tz = "UTC"),
                        lubridate::as_datetime(start_date))
    y_enddate <- pmin(ISOdate(year, 12, 31, 23, 59, 59, tz = "UTC"),
                      lubridate::as_datetime(paste(end_date, "23:59:59Z")))

    timeseq <- as.numeric(difftime(
      seq(y_startdate, y_enddate, "hours"),
      baseday,
      tz = "UTC", units = "days"
    ))
    ntime <- length(timeseq)

    loc.file <- file.path(outfolder, paste("MERRA", year, "nc", sep = "."))
    results$file[i] <- loc.file
    results$host[i] <- PEcAn.remote::fqdn()
    results$startdate[i] <- paste0(year, "-01-01 00:00:00")
    results$enddate[i] <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i] <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"

    ## Create dimensions
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)
    time <- ncdf4::ncdim_def(name = "time", units = paste("Days since ", baseday),
                             vals = timeseq, create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)

    ## Create output variables
    var_list <- list()
    for (dat in list(merra_vars, merra_pres_vars, merra_flux_vars, merra_lfo_vars)) {
      for (j in seq_len(nrow(dat))) {
        var_list <- c(var_list, list(ncdf4::ncvar_def(
          name = dat[j, ][["CF_name"]],
          units = dat[j, ][["units"]],
          dim = dim,
          missval = -999
        )))
      }
    }

    # Add additional derived flux variables
    var_list <- c(var_list, list(
      # Direct PAR + Direct NIR
      ncdf4::ncvar_def(
        name = "surface_direct_downwelling_shortwave_flux_in_air",
        units = "W/m2", dim = dim, missval = -999
      ),
      # Diffuse PAR + Diffuse NIR
      ncdf4::ncvar_def(
        name = "surface_diffuse_downwelling_shortwave_flux_in_air",
        units = "W/m2", dim = dim, missval = -999
      )
    ))

    if (overwrite || !file.exists(loc.file)) {
      if (file.exists(loc.file)) {
        PEcAn.logger::logger.warn(
          "Target file ", loc.file, " already exists.",
          "It will be overwritten."
        )
      }
      
      ## Create output file
      loc <- ncdf4::nc_create(loc.file, var_list)
      on.exit(ncdf4::nc_close(loc), add = TRUE)

      # Populate output file
      dates_yr <- dates[lubridate::year(dates) == year]
      for (d in seq_along(dates_yr)) {
        date <- dates_yr[[d]]
        end <- d * 24
        start <- end - 23
        mostfile <- file.path(outfolder, sprintf("merra-most-%s.nc", as.character(date)))
        PEcAn.logger::severeifnot(paste0("File ", mostfile, " does not exist."), file.exists(mostfile))
        nc <- ncdf4::nc_open(mostfile)
        for (r in seq_len(nrow(merra_vars))) {
          x <- ncdf4::ncvar_get(nc, merra_vars[r,][["MERRA_name"]])
          ncdf4::ncvar_put(loc, merra_vars[r,][["CF_name"]], x,
                          start = c(1, 1, start), count = c(1, 1, 24))
        }
        ncdf4::nc_close(nc)
        presfile <- file.path(outfolder, sprintf("merra-pres-%s.nc", as.character(date)))
        PEcAn.logger::severeifnot(paste0("File ", presfile, " does not exist."), file.exists(presfile))
        nc <- ncdf4::nc_open(presfile)
        for (r in seq_len(nrow(merra_pres_vars))) {
          x <- ncdf4::ncvar_get(nc, merra_pres_vars[r,][["MERRA_name"]])
          ncdf4::ncvar_put(loc, merra_pres_vars[r,][["CF_name"]], x,
                          start = c(1, 1, start), count = c(1, 1, 24))
        }
        ncdf4::nc_close(nc)
        fluxfile <- file.path(outfolder, sprintf("merra-flux-%s.nc", as.character(date)))
        PEcAn.logger::severeifnot(paste0("File ", fluxfile, " does not exist."), file.exists(fluxfile))
        nc <- ncdf4::nc_open(fluxfile)
        for (r in seq_len(nrow(merra_flux_vars))) {
          x <- ncdf4::ncvar_get(nc, merra_flux_vars[r,][["MERRA_name"]])
          ncdf4::ncvar_put(loc, merra_flux_vars[r,][["CF_name"]], x,
                          start = c(1, 1, start), count = c(1, 1, 24))
        }
        ncdf4::nc_close(nc)
        lfofile <- file.path(outfolder, sprintf("merra-lfo-%s.nc", as.character(date)))
        PEcAn.logger::severeifnot(paste0("File ", lfofile, " does not exist."), file.exists(lfofile))
        nc <- ncdf4::nc_open(lfofile)
        for (r in seq_len(nrow(merra_lfo_vars))) {
          x <- ncdf4::ncvar_get(nc, merra_lfo_vars[r,][["MERRA_name"]])
          ncdf4::ncvar_put(loc, merra_lfo_vars[r,][["CF_name"]], x,
                          start = c(1, 1, start), count = c(1, 1, 24))
        }
        ncdf4::nc_close(nc)
      }

      # Add derived variables
      # Total SW diffuse = Diffuse PAR + Diffuse NIR
      sw_diffuse <-
        ncdf4::ncvar_get(loc, "surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air") +
        ncdf4::ncvar_get(loc, "surface_diffuse_downwelling_nearinfrared_radiative_flux_in_air")
      ncdf4::ncvar_put(loc, "surface_diffuse_downwelling_shortwave_flux_in_air", sw_diffuse,
                      start = c(1, 1, 1), count = c(1, 1, -1))

      # Total SW direct = Direct PAR + Direct NIR
      sw_direct <-
        ncdf4::ncvar_get(loc, "surface_direct_downwelling_photosynthetic_radiative_flux_in_air") +
        ncdf4::ncvar_get(loc, "surface_direct_downwelling_nearinfrared_radiative_flux_in_air")
      ncdf4::ncvar_put(loc, "surface_direct_downwelling_shortwave_flux_in_air", sw_direct,
                      start = c(1, 1, 1), count = c(1, 1, -1))
    }
  }

  return(results)
}

get_merra_date <- function(date, latitude, longitude, outdir, overwrite = FALSE) {
  date <- as.character(date)
  dpat <- "([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})"
  year <- as.numeric(gsub(dpat, "\\1", date))
  month <- as.numeric(gsub(dpat, "\\2", date))
  day <- as.numeric(gsub(dpat, "\\3", date))
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  version <- if (year == 2020 & month == 9) {
    401
  } else if (year >= 2011) {
    400
  } else if (year >= 2001) {
    300
  } else {
    200
  }
  base_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"

  lat_grid <- seq(-90, 90, 0.5)
  lon_grid <- seq(-180, 180, 0.625)
  ilat <- which.min(abs(lat_grid - latitude))
  ilon <- which.min(abs(lon_grid - longitude))
  idxstring <- sprintf("[0:1:23][%d][%d]", ilat, ilon)

  # Standard variables
  url <- glue::glue(
    "{base_url}/{merra_prod}/{year}/{sprintf('%02d', month)}/",
    "MERRA2_{version}.{merra_file}.",
    "{year}{sprintf('%02d', month)}{sprintf('%02d', day)}.nc4.nc4"
  )
  qvars <- sprintf("%s%s", merra_vars$MERRA_name, idxstring)
  qstring <- paste(qvars, collapse = ",")
  outfile <- file.path(outdir, sprintf("merra-most-%d-%02d-%02d.nc",
                                       year, month, day))
  if (overwrite || !file.exists(outfile)) {
    req <- httr::GET(
      paste(url, qstring, sep = "?"),
      httr::authenticate(user = "pecanproject", password = "Data4pecan3"),
      httr::write_disk(outfile, overwrite = TRUE)
    )
  }

  # Pressure
  url <- glue::glue(
    "{base_url}/{merra_pres_prod}/{year}/{sprintf('%02d', month)}/",
    "MERRA2_{version}.{merra_pres_file}.",
    "{year}{sprintf('%02d', month)}{sprintf('%02d', day)}.nc4.nc4"
  )
  qvars <- sprintf("%s%s", merra_pres_vars$MERRA_name, idxstring)
  qstring <- paste(qvars, collapse = ",")
  outfile <- file.path(outdir, sprintf("merra-pres-%d-%02d-%02d.nc",
                                       year, month, day))
  if (overwrite || !file.exists(outfile)) {
    req <- httr::GET(
      paste(url, qstring, sep = "?"),
      httr::authenticate(user = "pecanproject", password = "Data4pecan3"),
      httr::write_disk(outfile, overwrite = TRUE)
    )
  }

  # Flux
  url <- glue::glue(
    "{base_url}/{merra_flux_prod}/{year}/{sprintf('%02d', month)}/",
    "MERRA2_{version}.{merra_flux_file}.",
    "{year}{sprintf('%02d', month)}{sprintf('%02d', day)}.nc4.nc4"
  )
  qvars <- sprintf("%s%s", merra_flux_vars$MERRA_name, idxstring)
  qstring <- paste(qvars, collapse = ",")
  outfile <- file.path(outdir, sprintf("merra-flux-%d-%02d-%02d.nc",
                                       year, month, day))
  if (overwrite || !file.exists(outfile)) {
    req <- PEcAn.utils::robustly(httr::GET, n = 10)(
      paste(url, qstring, sep = "?"),
      httr::authenticate(user = "pecanproject", password = "Data4pecan3"),
      httr::write_disk(outfile, overwrite = TRUE)
    )
  }

  # Land forcing
  url <- glue::glue(
    "{base_url}/{merra_lfo_prod}/{year}/{sprintf('%02d', month)}/",
    "MERRA2_{version}.{merra_lfo_file}.",
    "{year}{sprintf('%02d', month)}{sprintf('%02d', day)}.nc4.nc4"
  )
  qvars <- sprintf("%s%s", merra_lfo_vars$MERRA_name, idxstring)
  qstring <- paste(qvars, collapse = ",")
  outfile <- file.path(outdir, sprintf("merra-lfo-%d-%02d-%02d.nc",
                                       year, month, day))
  if (overwrite || !file.exists(outfile)) {
    req <- PEcAn.utils::robustly(httr::GET, n = 10)(
      paste(url, qstring, sep = "?"),
      httr::authenticate(user = "pecanproject", password = "Data4pecan3"),
      httr::write_disk(outfile, overwrite = TRUE)
    )
  }
}

# For more on MERRA variables, see:
# - The MERRA2 readme -- https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2T1NXRAD.5.12.4/doc/MERRA2.README.pdf
# - The MERRA2 file spec -- https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
# Page numbers below correspond to pages in the file spec.

# Surface flux diagnostics (pg. 33)
merra_prod <- "M2T1NXFLX.5.12.4"
merra_file <- "tavg1_2d_flx_Nx"
merra_vars <- tibble::tribble(
  ~CF_name, ~MERRA_name, ~units,
  # TLML - Surface air temperature
  "air_temperature", "TLML", "Kelvin",
  # ULML - Surface eastward wind
  "eastward_wind", "ULML", "m/s",
  # VLML - Surface northward wind
  "northward_wind", "VLML", "m/s",
  # QSH - Effective surface specific humidity
  "specific_humidity", "QSH", "g/g",
  # PRECTOT - Total precipitation from atmospheric model physics
  "precipitation_flux", "PRECTOT", "kg/m2/s",
  # NIRDF - Surface downwelling nearinfared diffuse flux
  "surface_diffuse_downwelling_nearinfrared_radiative_flux_in_air", "NIRDF", "W/m2",
  # NIRDR - Surface downwelling nearinfrared beam flux
  "surface_direct_downwelling_nearinfrared_radiative_flux_in_air", "NIRDR", "W/m2"
)

# Single-level diagnostics (pg. 17)
merra_pres_prod <- "M2I1NXASM.5.12.4"
merra_pres_file <- "inst1_2d_asm_Nx"
merra_pres_vars <- tibble::tribble(
  ~CF_name, ~MERRA_name, ~units,
  # PS - Surface pressure
  "air_pressure", "PS", "Pascal",
)

# Radiation diagnostics (pg. 43)
# NOTE: Downwelling longwave is calculated as Net + Absorbed + Emitted (because
# Net = Downwelling - Absorbed - Emitted).
merra_flux_prod <- "M2T1NXRAD.5.12.4"
merra_flux_file <- "tavg1_2d_rad_Nx"
merra_flux_vars <- tibble::tribble(
  ~CF_name, ~MERRA_name, ~units,
  # LWGAB is 'Surface absorbed longwave radiation' -- In MERRA, surface net
  # longwave flux is `absorbed - emitted`, so we assume this is the correct variable
  "surface_downwelling_longwave_flux_in_air", "LWGAB", "W/m2",
  # SWGDN is 'Surface incoming shortwave flux'
  "surface_downwelling_shortwave_flux_in_air", "SWGDN", "W/m2"
)


# Land surface forcings (pg. 39)
merra_lfo_prod <- "M2T1NXLFO.5.12.4"
merra_lfo_file <- "tavg1_2d_lfo_Nx"
merra_lfo_vars <- tibble::tribble(
  ~CF_name, ~MERRA_name, ~units,
  # Surface downwelling PAR diffuse flux, PARDF
  "surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air", "PARDF", "W/m2",
  # Surface downwelling PAR beam flux, PARDR
  "surface_direct_downwelling_photosynthetic_radiative_flux_in_air", "PARDR", "W/m2"
)
