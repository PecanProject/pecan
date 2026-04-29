#' Download Cal-Adapt WRF CMIP6 outputs for a single site and convert to CF
#'
#' Fetches hourly WRF dynamically downscaled data from the Cal-Adapt Analytics
#' Engine (CADCAT S3 bucket) via caladaptaer, extracts the nearest grid cell to
#' the site, converts units to CF-1.8, and writes one NetCDF per year.
#'
#' WRF grids are cached in tempdir() so that when met.process calls this for
#' multiple sites in the same R session, each grid is only fetched from S3 once.
#' For 200 sites x 8 vars x 20 years that cuts S3 round trips from 32,000 to 160.
#'
#' Available models (all at 45 km, ssp370): CESM2, CNRM-ESM2-1, EC-Earth3,
#' EC-Earth3-Veg, FGOALS-g3, MPI-ESM1-2-HR, MIROC6, TaiESM1.
#' Only CESM2 has ssp245 and ssp585 in addition to ssp370.
#'
#' Precipitation for MPI-ESM1-2-HR, MIROC6, and TaiESM1 is derived from
#' rainc + rainnc components; caladaptaer handles this transparently.
#'
#' @param outfolder Directory for storing output
#' @param start_date Start date for met data
#' @param end_date End date for met data
#' @param site_id BETY site id
#' @param lat.in Latitude of site (decimal degrees, WGS84)
#' @param lon.in Longitude of site (decimal degrees, WGS84)
#' @param model WRF GCM name, default "CESM2"
#' @param scenario SSP experiment id, default "ssp370"
#' @param resolution WRF nested-domain id: "d01" (45 km, default and only
#'   one with full coverage), "d02" (9 km), or "d03" (3 km). Higher
#'   resolutions are only partially released; check
#'   \code{caladaptaer::cae_check_variables()} for availability.
#' @param overwrite Overwrite existing files? Default FALSE
#' @param verbose Extra debug output? Default FALSE
#' @param ... further arguments, currently ignored
#'
#' @return invisible data.frame with file info for BETY registration
#'
#' @importFrom rlang .data
#' @export
#' @author Akash B V
download.CalAdaptWRF <- function(outfolder, start_date, end_date,
                                 site_id, lat.in, lon.in,
                                 model = "CESM2", scenario = "ssp370",
                                 resolution = "d01",
                                 overwrite = FALSE, verbose = FALSE, ...) {

  if (!requireNamespace("caladaptaer", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "caladaptaer package required but not installed. ",
      "Install with: remotes::install_github('lebauerapproach/caladaptaer')")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    PEcAn.logger::logger.severe("sf package required for CRS transform")
  }
  if (!requireNamespace("stars", quietly = TRUE)) {
    PEcAn.logger::logger.severe("stars package required for grid extraction")
  }

  # null guard, convert_input sometimes passes NULL for optional args
  if (is.null(model))      model <- "CESM2"
  if (is.null(scenario))   scenario <- "ssp370"
  if (is.null(resolution)) resolution <- "d01"

  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  # BETY site id formatting
  site_id <- tryCatch(as.numeric(site_id),
                       warning = function(w) as.character(site_id))
  if (is.numeric(site_id) && site_id > 1e9) {
    siteid_str <- paste0(site_id %/% 1e9, "-", site_id %% 1e9)
  } else {
    siteid_str <- as.character(site_id)
  }
  outfolder <- paste0(outfolder, "_site_", siteid_str)

  lat.in <- as.numeric(lat.in)
  lon.in <- as.numeric(lon.in)

  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)

  ##variable mapping from the central met table
  wrf_tbl <- pecan_standard_met_table |>
    dplyr::filter(!is.na(.data$caladapt_wrf) & nzchar(.data$caladapt_wrf))

  # separate direct-fetch vars from derived ones (wind_speed = CALC)
  fetch_tbl <- wrf_tbl |>
    dplyr::filter(!grepl("^CALC", .data$caladapt_wrf))
  derived_tbl <- wrf_tbl |>
    dplyr::filter(grepl("^CALC", .data$caladapt_wrf))

  wrf_to_cf <- stats::setNames(fetch_tbl$cf_standard_name, fetch_tbl$caladapt_wrf)

  ylist <- seq(start_year, end_year, by = 1)
  rows  <- length(ylist)

  results <- data.frame(
    file = character(rows),
    host = character(rows),
    mimetype = character(rows),
    formatname = character(rows),
    startdate = character(rows),
    enddate = character(rows),
    dbfile.name = paste("CalAdaptWRF", model, scenario, sep = "."),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(rows)) {
    year <- ylist[i]

    loc.file <- file.path(
      outfolder,
      paste("CalAdaptWRF", model, scenario, year, "nc", sep = ".")
    )

    results$file[i]       <- loc.file
    results$host[i]       <- PEcAn.remote::fqdn()
    results$startdate[i]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[i]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i]   <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"

    if (file.exists(loc.file) && !isTRUE(overwrite)) {
      PEcAn.logger::logger.debug("File '", loc.file, "' already exists, skipping")
      next
    }

    PEcAn.logger::logger.info(
      "CalAdaptWRF: fetching ", model, " ", scenario,
      " year ", year, " (", i, " of ", rows, ")"
    )

    year_start <- paste0(year, "-01-01T00:00:00")
    year_end   <- paste0(year, "-12-31T23:00:00")

    ##fetch each variable, using session cache to avoid redundant S3 reads
    # WRF 45km grid is small (~20-30 MB per var per year). We stash
    # the full grid in tempdir() so that subsequent sites in the same
    # met.process/papply session reuse it instead of hitting S3 again
    dat.list <- list()
    time_vals <- NULL
    pt_native <- NULL  # build once after first grid fetch sets the CRS

    for (wrf_var in names(wrf_to_cf)) {
      cache_key  <- paste("caladapt_grid", model, scenario,
                          resolution, wrf_var, year, sep = "_")
      cache_file <- file.path(tempdir(), paste0(cache_key, ".rds"))

      if (file.exists(cache_file)) {
        if (verbose) {
          PEcAn.logger::logger.debug("  cache hit: ", wrf_var, " ", year)
        }
        grid <- readRDS(cache_file)
      } else {
        PEcAn.logger::logger.info("  fetching ", wrf_var, " from S3")
        grid <- caladaptaer::cae_fetch(
          variable   = wrf_var,
          model      = model,
          scenario   = scenario,
          timescale  = "1hr",
          resolution = resolution,
          start_time = year_start,
          end_time   = year_end
        )
        saveRDS(grid, cache_file)
      }

      # grab time dimension and build the projected point once
      if (is.null(time_vals)) {
        time_vals <- stars::st_get_dimension_values(grid, "time")
        pt_wgs84 <- sf::st_as_sf(
          data.frame(lon = lon.in, lat = lat.in),
          coords = c("lon", "lat"), crs = 4326
        )
        pt_native <- sf::st_transform(pt_wgs84, sf::st_crs(grid))
      }

      # extract nearest grid cell
      extracted <- stars::st_extract(grid, pt_native)

      vals <- extracted[[1]]
      if (inherits(vals, "units")) vals <- units::drop_units(vals)
      dat.list[[wrf_var]] <- as.numeric(vals)
    }

    ##unit conversions
    # precip: WRF hourly accumulation (mm) -> CF flux (kg/m2/s)
    # 1 mm water = 1 kg/m2, divide by 3600s for hourly timestep
    if ("prec" %in% names(dat.list)) {
      dat.list[["prec"]] <- dat.list[["prec"]] / 3600
    }

    # specific humidity: WRF stores mixing ratio q (kg/kg)
    # q_specific = q / (1 + q)
    if ("q2" %in% names(dat.list)) {
      q <- dat.list[["q2"]]
      dat.list[["q2"]] <- q / (1 + q)
    }

    ##derived variables
    # wind speed from u10 and v10 components
    wind_speed <- NULL
    if (nrow(derived_tbl) > 0 &&
        all(c("u10", "v10") %in% names(dat.list))) {
      wind_speed <- sqrt(dat.list[["u10"]]^2 + dat.list[["v10"]]^2)
    }

    ##write CF NetCDF, one file per year
    time_secs <- as.numeric(difftime(
      time_vals,
      as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = "UTC"),
      units = "secs"
    ))

    lat_dim  <- ncdf4::ncdim_def("latitude", "degree_north",
                                  lat.in, create_dimvar = TRUE)
    lon_dim  <- ncdf4::ncdim_def("longitude", "degree_east",
                                  lon.in, create_dimvar = TRUE)
    time_dim <- ncdf4::ncdim_def("time",
                                  paste("seconds since", results$startdate[i]),
                                  time_secs,
                                  create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat_dim, lon_dim, time_dim)

    # build ncdf4 variable defs from met table
    var.list <- list()
    var.names <- character()
    for (j in seq_len(nrow(fetch_tbl))) {
      cf_name <- fetch_tbl$cf_standard_name[j]
      var.list[[j]] <- ncdf4::ncvar_def(
        name    = cf_name,
        units   = fetch_tbl$units[j],
        dim     = dim,
        missval = -9999.0,
        verbose = verbose
      )
      var.names[j] <- fetch_tbl$caladapt_wrf[j]
    }

    # wind speed as derived variable
    if (!is.null(wind_speed) && nrow(derived_tbl) > 0) {
      ws_row <- derived_tbl[derived_tbl$cf_standard_name == "wind_speed", ]
      if (nrow(ws_row) > 0) {
        idx <- length(var.list) + 1
        var.list[[idx]] <- ncdf4::ncvar_def(
          name    = "wind_speed",
          units   = ws_row$units[1],
          dim     = dim,
          missval = -9999.0,
          verbose = verbose
        )
      }
    }

    loc <- ncdf4::nc_create(loc.file, var.list, verbose = verbose)
    for (j in seq_len(nrow(fetch_tbl))) {
      ncdf4::ncvar_put(loc, var.list[[j]], dat.list[[var.names[j]]])
    }
    if (!is.null(wind_speed)) {
      ncdf4::ncvar_put(loc, var.list[[length(var.list)]], wind_speed)
    }
    ncdf4::nc_close(loc)

    PEcAn.logger::logger.info("  wrote ", loc.file)
  }

  return(invisible(results))
} ##download.CalAdaptWRF
