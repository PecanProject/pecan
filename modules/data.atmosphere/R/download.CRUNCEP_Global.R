##' Download CRUNCEP data
##'
##' Download and convert to CF CRUNCEP single grid point from MSTIMIP server using OPENDAP interface
##' @param outfolder Directory where results should be written
##' @param start_date,end_date Range of years to retrieve. Format is YYYY-MM-DD,
##'   but only the year portion is used and the resulting files always contain a full year of data.
##' @param lat.in site latitude in decimal degrees
##' @param lon.in site longitude in decimal degrees
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param verbose logical. Passed on to \code{\link[ncdf4]{ncvar_def}} and \code{\link[ncdf4]{nc_create}}
##'   to control printing of debug info
##' @param maxErrors Maximum times to re-try folloing an error accessing netCDF data through THREDDS
##' @param sleep Wait time between attempts following a THREDDS or other error
##' @param method (string) Data access method. `opendap` (default)
##'   attempts to directly access files via OpenDAP. `ncss` (NetCDF
##'   subset) subsets the file on the server, downloads the subsetted
##'   file to `tempfile` and then reads it locally. `opendap` is
##'   faster when it works, but often fails because of server issues.
##'   `ncss` can be much slower, but is more reliable.
##' @param ... Other arguments, currently ignored
##' @export
##'
##' @author James Simkins, Mike Dietze, Alexey Shiklomanov
download.CRUNCEP <- function(outfolder, start_date, end_date, lat.in, lon.in,
                             overwrite = FALSE, verbose = FALSE, maxErrors = 10, sleep = 2,
                             method = "ncss", ...) {

  if (is.null(method)) method <- "ncss"
  if (!method %in% c("opendap", "ncss")) {
    PEcAn.logger::logger.severe(glue::glue(
      "Bad method '{method}'. Currently, only 'opendap' or 'ncss' are supported."
    ))
  }

  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  # Check that the start and end date are within bounds
  CRUNCEP_start <- 1901
  CRUNCEP_end <- 2010
  if (start_year < CRUNCEP_start | end_year > CRUNCEP_end) {
    PEcAn.logger::logger.severe(sprintf('Input year range (%d:%d) exceeds the CRUNCEP range (%d:%d)',
                                        start_year, end_year,
                                        CRUNCEP_start, CRUNCEP_end))
  }

  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)

  lat.in <- as.numeric(lat.in)
  lon.in <- as.numeric(lon.in)

  # Convert lat-lon to grid row and column
  lat_grid <- floor(2 * (90 - lat.in)) + 1
  lon_grid <- floor(2 * (lon.in + 180)) + 1

  # Check against land-sea mask
  maskfile <- file.path(outfolder, "cruncep_landwater_mask.nc")
  mask_url <- paste0(
    "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1220/",
    "mstmip_driver_global_hd_landwatermask_v1.nc4",
    "?var=land_water_mask&disableLLSubset=on&disableProjSubset=on&horizStride=1&",
    "accept=netcdf"
  )
  utils::download.file(mask_url, maskfile)

  mask_nc <- ncdf4::nc_open(maskfile)
  on.exit(ncdf4::nc_close(mask_nc), add = TRUE)

  # Set search radius to up to 2 pixels (1 degree) in any direction
  mask_minlat <- lat_grid - 2
  mask_minlon <- lon_grid - 2
  mask_lats <- ncdf4::ncvar_get(mask_nc, "lat", start = mask_minlat, count = 5)
  mask_lons <- ncdf4::ncvar_get(mask_nc, "lon", start = mask_minlon, count = 5)
  mask_values <- ncdf4::ncvar_get(
    mask_nc,
    "land_water_mask",
    start = c(mask_minlon, mask_minlat),
    count = c(5, 5)
  )

  # Build a lat-lon grid matrix and calculate distance from target coords
  mask_grid <- as.matrix(expand.grid(lon = mask_lons, lat = mask_lats))
  mask_igrid <- as.matrix(expand.grid(lon = seq_along(mask_lons), lats = seq_along(mask_lats)))
  mask_dist <- (lon.in - mask_grid[, 1])^2 + (lat.in - mask_grid[, 2])^2
  # Order by increasing distance (closest first)
  mask_order <- order(mask_dist)
  mask_igrido <- mask_igrid[mask_order,]
  on_land <- as.logical(mask_values[mask_igrido])
  if (!any(on_land)) {
    PEcAn.logger::logger.severe(glue::glue(
      "Coordinates {lat.in} latitude, {lon.in} longitude ",
      "are not within 2 pixels (1 degree) of any land."
    ))
  }
  igrid <- which(on_land)[1]
  if (igrid > 1) {
    lon.in.orig <- lon.in
    lat.in.orig <- lat.in
    lon.in <- mask_grid[mask_order[igrid], 1]
    lat.in <- mask_grid[mask_order[igrid], 2]
    PEcAn.logger::logger.warn(glue::glue(
      "Coordinates {lat.in.orig} latitude, {lon.in.orig} longitude ",
      "are not on land, so using closest land pixel within 1 degree. ",
      "New coordinates are {lat.in} latitude, {lon.in} longitude."
    ))
  }

  if (method == "opendap") {
    # Convert lat-lon to grid row and column
    lat_grid <- floor(2 * (90 - lat.in)) + 1
    lon_grid <- floor(2 * (lon.in + 180)) + 1
  } else if (method == "ncss") {
    # Only downloading point data, so only one point
    lat_grid <- 1
    lon_grid <- 1
  }

  ylist <- seq(start_year, end_year, by = 1)
  rows <- length(ylist)
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows),
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = "CRUNCEP",
                        stringsAsFactors = FALSE)

  var <- tibble::tribble(
    ~DAP.name, ~CF.name, ~units,
    "tair", "air_temperature", "Kelvin",
    "lwdown", "surface_downwelling_longwave_flux_in_air", "W/m2",
    "press", "air_pressure", "Pascal",
    "swdown", "surface_downwelling_shortwave_flux_in_air", "W/m2",
    "uwind", "eastward_wind", "m/s",
    "vwind", "northward_wind", "m/s",
    "qair", "specific_humidity", "g/g",
    "rain", "precipitation_flux", "kg/m2/s"
  )

  for (i in seq_len(rows)) {
    year <- ylist[i]
    ntime <- PEcAn.utils::days_in_year(year) * 4

    loc.file <- file.path(outfolder, paste("CRUNCEP", year, "nc", sep = "."))
    results$file[i] <- loc.file
    results$host[i] <- PEcAn.remote::fqdn()
    results$startdate[i] <- paste0(year, "-01-01 00:00:00")
    results$enddate[i] <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i] <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"

    if (file.exists(loc.file) && !isTRUE(overwrite)) {
     PEcAn.logger::logger.error("File already exists. Skipping to next year")
      next
    }

    PEcAn.logger::logger.info(paste("Downloading", loc.file))
    ## Create dimensions
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)

    days_elapsed <- (1:ntime) * 6/24 - 3/24 # data are 6-hourly, with timestamp at center of interval
    time <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", year, "-01-01T00:00:00Z"),
                             vals = as.array(days_elapsed), create_dimvar = TRUE, unlim = TRUE)

    dim <- list(lat, lon, time)

    var.list <- list()
    dat.list <- list()

    file_name <- "mstmip_driver_global_hd_climate_%1$s_%2$d_v1.nc4"
    ## get data off OpenDAP

    dap_base <- switch(
      method,
      opendap = paste0("https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/", file_name),
      ncss = paste0("https://thredds.daac.ornl.gov/thredds/ncss/grid/ornldaac/1220/", file_name, "/dataset.html")
    )

    for (j in seq_len(nrow(var))) {
      current_var <- var$DAP.name[j]
      url <- sprintf(dap_base, current_var, year)
      PEcAn.logger::logger.info("Attempting to access file at: ", url)
      if (method == "opendap") {
        dap <- PEcAn.utils::retry.func(ncdf4::nc_open(url, verbose=verbose), maxErrors=maxErrors, sleep=sleep)
      } else if (method == "ncss") {
        ncss_query <- glue::glue(
          url, "?",
          "var={current_var}&",
          "south={lat.in}&",
          "west={lon.in}&",
          # Add tiny amount to latitude and longitude to satisfy
          # non-point condition, but still be within grid cell.
          "north={lat.in + 5e-6}&",
          "east={lon.in + 5e-6}&",
          # Year starts at 00:00:00 and ends at 21:00:00
          "time_start={year}-01-01T00:00:00Z&",
          "time_end={year}-12-31T21:00:00Z&",
          "accept=netcdf"
        )
        tmp_file <- tempfile()
        utils::download.file(ncss_query, tmp_file)
        dap <- ncdf4::nc_open(tmp_file)
      }

      # confirm that timestamps match
      if (dap$dim$time$len != ntime) {
        PEcAn.logger::logger.severe("Expected", ntime, "observations, but", url,  "contained", dap$dim$time$len)
      }
      dap_time <- udunits2::ud.convert(dap$dim$time$vals,
                                       dap$dim$time$units,
                                       time$units)
      if (!isTRUE(all.equal(dap_time, time$vals))){
        PEcAn.logger::logger.severe("Timestamp mismatch.",
                                    "Expected", min(time$vals), '..', max(time$vals), time$units,
                                    "but got", min(dap_time), "..", max(dap_time))
      }

      dat.list[[j]] <- PEcAn.utils::retry.func(
        ncdf4::ncvar_get(
          dap,
          as.character(var$DAP.name[j]),
          c(lon_grid, lat_grid, 1),
          c(1, 1, ntime)
        ),
        maxErrors=maxErrors, sleep=sleep)

      var.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]),
                                        units = as.character(var$units[j]),
                                        dim = dim,
                                        missval = -999,
                                        verbose = verbose)
      ncdf4::nc_close(dap)
    }
    ## change units of precip to kg/m2/s instead of 6 hour accumulated precip
    dat.list[[8]] <- dat.list[[8]] / 21600

    ## put data in new file
    loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = verbose)
    for (j in seq_len(nrow(var))) {
      ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[j]])
    }
    ncdf4::nc_close(loc)
  }

  return(invisible(results))
} # download.CRUNCEP
