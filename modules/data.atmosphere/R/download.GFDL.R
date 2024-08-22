#' Download GFDL CMIP5 outputs for a single grid point using OPeNDAP and convert to CF 
#'
#' @export
#' @param outfolder Directory for storing output
#' @param start_date Start date for met (will be converted via [base::as.POSIXlt])
#' @param end_date End date for met (will be converted via [base::as.POSIXlt])
#' @param lat.in Latitude coordinate for met
#' @param lon.in Longitude coordinate for met
#' @param overwrite Logical: Download a fresh version even if a local file with
#'    the same name already exists?
#' @param verbose Logical, passed on to \code{\link[ncdf4]{ncvar_def}} and
#'    \code{\link[ncdf4]{nc_create}} to control printing of debug info
#' @param model Which GFDL model to run (options are CM3, ESM2M, ESM2G)
#' @param scenario Which scenario to run (options are rcp26, rcp45, rcp60, rcp85)
#' @param ensemble_member Which ensemble_member to initialize the run (options are r1i1p1, r3i1p1, r5i1p1)
#' @param ... further arguments, currently ignored
#'
#' @author James Simkins, Alexey Shiklomanov, Ankur Desai
download.GFDL <- function(outfolder, start_date, end_date, lat.in, lon.in,
                          overwrite = FALSE, verbose = FALSE,
                          model = "CM3", scenario = "rcp45", ensemble_member = "r1i1p1", ...) {

  if(is.null(model))           model <- "CM3"
  if(is.null(scenario))        scenario <- "rcp45"
  if(is.null(ensemble_member)) ensemble_member <- "r1i1p1"

  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  obs_per_year <- 365 * 24 /3 # 3-hr intervals, leap days ignored

  #Fix Outfolder to include model and scenario
  folder_name <- paste0("GFDL_", model, "_", scenario, "_", ensemble_member)
  source_id_foldername <- basename(outfolder)
  source_all_foldername <- gsub("GFDL", folder_name, source_id_foldername)
  outfolder <- file.path(paste0(outfolder, source_all_foldername))

  lat.in     <- as.numeric(lat.in)
  lat_floor  <- floor(lat.in)
  lon.in     <- as.numeric(lon.in)
  lon_floor  <- floor(lon.in)
  if (lon_floor < 0) {
    lon_floor <- 360 + lon_floor
  }
  lat_GFDL <- lat_floor * (0.5) + 45
  lat_GFDL <- floor(lat_GFDL) + 1
  lon_GFDL <- lon_floor / 2.5
  lon_GFDL <- floor(lon_GFDL) + 1

  dap_base <- "http://nomads.gfdl.noaa.gov:9192/opendap/CMIP5/output1/NOAA-GFDL/GFDL"

  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)

  ylist <- seq(start_year, end_year, by = 1)
  rows <- length(ylist)

  results <- data.frame(
    file = character(rows),
    host = character(rows),
    mimetype = character(rows),
    formatname = character(rows),
    startdate = character(rows),
    enddate = character(rows),
    dbfile.name = paste("GFDL", model, scenario, ensemble_member, sep = "."),   # 'GFDL',
    stringsAsFactors = FALSE
  )

  var <- tibble::tribble(
    ~DAP.name, ~CF.name, ~units,
    "tas", "air_temperature", "Kelvin",
    "rlds", "surface_downwelling_longwave_flux_in_air", "W/m2",
    "ps", "air_pressure", "Pascal",
    "rsds", "surface_downwelling_shortwave_flux_in_air", "W/m2",
    "uas", "eastward_wind", "m/s",
    "vas", "northward_wind", "m/s",
    "huss", "specific_humidity", "g/g",
    "pr", "precipitation_flux", "kg/m2/s"
  )

  for (i in seq_len(rows)) {
    year <- ylist[i]
    # find start position of currently-wanted year in the 5-year DAP file
    time_offset <- 1 + ((year-1) %% 5) * obs_per_year

    PEcAn.logger::logger.debug(
      sprintf(
        "Downloading GFDL year %d (%d of %d)",
        year, i, rows
      )
    )

    loc.file <- file.path(
      outfolder,
      paste("GFDL", model, scenario, ensemble_member, year, "nc", sep = ".")
    )

    results$file[i]       <- loc.file
    results$host[i]       <- PEcAn.remote::fqdn()
    results$startdate[i]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[i]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i]   <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"
    
    if (file.exists(loc.file) && !isTRUE(overwrite)) {
      PEcAn.logger::logger.error("File already exists. Skipping to next year")
      next
    }
    
    met_start <- 2006
    met_block <- 5
    url_year  <- met_start + floor((year - met_start) / met_block) * met_block
    start_url <- paste0(url_year, "0101")
    end_url   <- paste0(url_year + met_block - 1, "1231")

    ## Create dimensions
    lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
    lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)
    time <- ncdf4::ncdim_def(
      name = "time",
      units = paste("seconds since", results$startdate[i]),
      vals = (1:obs_per_year) * 10800, # 3 hr interval * 3600 sec/hr
      create_dimvar = TRUE,
      unlim = TRUE
    )
    dim <- list(lat = lat, lon = lon, time = time)

    var.list <- list()
    dat.list <- list()

    ## get data off OpenDAP
    for (j in seq_len(nrow(var))) {
      PEcAn.logger::logger.debug(
        sprintf(
          "Downloading GFDL var %s (%d of %d)",
          var$DAP.name[j], j, nrow(var)
        )
      )
      dap_end <- paste0(
        "-", model, "/",
        scenario, "/3hr/atmos/3hr/",
        ensemble_member, "/v20110601/",
        var$DAP.name[j], "/",
        var$DAP.name[j], "_3hr_GFDL-",
        model, "_",
        scenario, "_",
        ensemble_member, "_",
        start_url, "00-", end_url, "23.nc"
      )
      dap_file <- paste0(dap_base, dap_end)
      dap <- ncdf4::nc_open(dap_file, suppress_dimvals = TRUE)

      # Sanity check:
      # We're saving the data with timestamps at the end of the interval,
      # while GFDL-supplied timestamps vary slightly -- some vars are
      # timestamped in middle of interval, others at end.
      # But if these disagree by more than 3 hours, we have a problem.
      raw_time <- ncdf4::ncvar_get(dap, "time", start = time_offset, count = obs_per_year)
      converted_time <- PEcAn.utils::ud_convert(raw_time, dap$dim$time$units, dim$time$units)
      if(!all(diff(converted_time) == 3 * 60 * 60)){
        PEcAn.logger::logger.error(
          "Expected timestamps at 3-hour intervals, got",
          paste(range(diff(converted_time)), collapse = "-"),
          "seconds")
      }
      if(!all(abs(dim$time$vals - converted_time) < (3 * 60 * 60))){
        PEcAn.logger::logger.error(
          "Timestamps in GFDL source file differ from expected by more than 3 hours:",
          "Expected", paste(range(dim$time$vals), collapse = "-"),
          dim$time$units,
          ", got", paste(range(converted_time), collapse = "-"),
          ". Greatest difference from expected:",
            max(abs(dim$time$vals - converted_time)), "seconds")
      }

      dat.list[[j]] <- ncdf4::ncvar_get(dap, as.character(var$DAP.name[j]),
                                 start = c(lon_GFDL, lat_GFDL, time_offset),
                                 count = c(1, 1, obs_per_year))
      var.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]),
                                 units = as.character(var$units[j]),
                                 dim = dim,
                                 missval = -999,
                                 verbose = verbose)
      ncdf4::nc_close(dap)
    }


    ## put data in new file
    loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = verbose)
    for (j in seq_len(nrow(var))) {
      ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[j]])
    }
    ncdf4::nc_close(loc)

  }

  return(invisible(results))
} # download.GFDL
