#' Download NARR time series for a single site
#'
#' @param outfolder Target directory for storing output
#' @param start_date Start date for met data
#' @param end_date End date for met data
#' @param lat.in Site latitude coordinate
#' @param lon.in Site longitude coordinate
#' @param overwrite Overwrite existing files?  Default=FALSE
#' @param verbose Turn on verbose output? Default=FALSE
#' @param progress Whether or not to show a progress bar.
#' Requires the `progress` package to be installed.
#' @param parallel Download in parallel? Default = TRUE
#' @param ncores Number of cores for parallel download. Default is
#' `parallel::detectCores()`
#' @param ... further arguments, currently ignored
#'
#' @examples
#'
#' \dontrun{
#' download.NARR_site(tempdir(), "2001-01-01", "2001-01-12", 43.372, -89.907)
#' }
#'
#'
#' @export
#' @importFrom rlang .data
#'
#' @author Alexey Shiklomanov
download.NARR_site <- function(outfolder,
                               start_date, end_date,
                               lat.in, lon.in,
                               overwrite = FALSE,
                               verbose = FALSE,
                               progress = TRUE,
                               parallel = TRUE,
                               ncores = if (parallel) parallel::detectCores() else NULL,
                               ...) {

  if (verbose) PEcAn.logger::logger.info("Downloading NARR data")
  narr_data <- get_NARR_thredds(
    start_date, end_date, lat.in, lon.in,
    progress = progress,
    parallel = parallel,
    ncores = ncores
  )
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)

  date_limits_chr <- strftime(range(narr_data$datetime), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  narr_byyear <- narr_data %>%
    dplyr::mutate(year = lubridate::year(.data$datetime)) %>%
    dplyr::group_by(.data$year) %>%
    tidyr::nest()

  # Prepare result data frame
  result_full <- narr_byyear %>%
    dplyr::mutate(
      file = file.path(outfolder, paste("NARR", .data$year, "nc", sep = ".")),
      host = PEcAn.remote::fqdn(),
      start_date = date_limits_chr[1],
      end_date = date_limits_chr[2],
      mimetype = "application/x-netcdf",
      formatname = "CF Meteorology",
    )

  lat <- ncdf4::ncdim_def(
    name = "latitude",
    units = "degree_north",
    vals = lat.in,
    create_dimvar = TRUE
  )
  lon <- ncdf4::ncdim_def(
    name = "longitude",
    units = "degree_east",
    vals = lon.in,
    create_dimvar = TRUE
  )

  narr_proc <- result_full %>%
    dplyr::mutate(
      data_nc = purrr::map2(.data$data, .data$file, prepare_narr_year, lat = lat, lon = lon)
    )

  results <- dplyr::select(result_full, -"data")
  return(invisible(results))
} # download.NARR_site

#' Write NetCDF file for a single year of data
#'
#' @param dat NARR tabular data for a single year ([get_NARR_thredds])
#' @param file Full path to target file
#' @param lat_nc `ncdim` object for latitude
#' @param lon_nc `ncdim` object for longitude
#' @param verbose logical: ask`ncdf4` functions to be very chatty while they work?
#' @return List of NetCDF variables in data. Creates NetCDF file containing
#' data as a side effect
prepare_narr_year <- function(dat, file, lat_nc, lon_nc, verbose = FALSE) {
  starttime <- min(dat$datetime)
  starttime_f <- strftime(starttime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  time <- difftime(dat$datetime, starttime) %>%
    as.numeric() %>%
    PEcAn.utils::ud_convert("seconds", "hours")
  time_nc <- ncdf4::ncdim_def(
    name = "time",
    units = paste0("hours since ", starttime_f),
    vals = time,
    create_dimvar = TRUE,
    unlim = TRUE
  )
  nc_values <- dplyr::select(dat, narr_all_vars$CF_name)
  ncvar_list <- purrr::map(
    colnames(nc_values),
    col2ncvar,
    dims = list(lat_nc, lon_nc, time_nc)
  )
  nc <- ncdf4::nc_create(file, ncvar_list, verbose = verbose)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  purrr::iwalk(nc_values, ~ncdf4::ncvar_put(nc, .y, .x, verbose = verbose))
  invisible(ncvar_list)
}

#' Create `ncvar` object from variable name
#'
#' @param variable CF variable name
#' @param dims List of NetCDF dimension objects (passed to
#' `ncdf4::ncvar_def(..., dim)`)
#' @return `ncvar` object (from `ncvar_def`)
col2ncvar <- function(variable, dims) {
  var_info <- narr_all_vars %>% dplyr::filter(.data$CF_name == variable)
  ncdf4::ncvar_def(
    name = variable,
    units = var_info$units,
    dim = dims,
    missval = -999,
  )
}

#' Retrieve NARR data using thredds
#'
#' @param start_date Start date for meteorology
#' @param end_date End date for meteorology
#' @param lat.in Latitude coordinate
#' @param lon.in Longitude coordinate
#' @param progress Whether or not to show a progress bar (default = `TRUE`).
#' Requires the `progress` package to be installed.
#' @param drop_outside Whether or not to drop dates outside of `start_date` to
#' `end_date` range (default = `TRUE`).
#' @inheritParams download.NARR_site
#' @return `tibble` containing time series of NARR data for the given site
#' @author Alexey Shiklomanov
#' @examples
#'
#' \dontrun{
#' dat <- get_NARR_thredds("2008-01-01", "2008-01-15", 43.3724, -89.9071)
#' }
#'
#' @export
get_NARR_thredds <- function(start_date, end_date, lat.in, lon.in,
                             progress = TRUE,
                             drop_outside = TRUE,
                             parallel = TRUE,
                             ncores = 1
                             ) {

  PEcAn.logger::severeifnot(
    length(start_date) == 1,
    msg = paste("Start date must be a scalar, but has length", length(start_date))
  )

  PEcAn.logger::severeifnot(
    length(end_date) == 1,
    msg = paste("End date must be a scalar, but has length", length(end_date))
  )

  PEcAn.logger::severeifnot(
    length(lat.in) == 1,
    msg = paste("Latitude must be a scalar, but has length", length(latitude))
  )

  PEcAn.logger::severeifnot(
    length(lon.in) == 1,
    msg = paste("Longitude must be a scalar, but has length", length(longitude))
  )

  narr_start <- lubridate::ymd("1979-01-01")
  # NARR is updated monthly
  # Set end day to the last day of the previous month
  # (e.g. March 31 if today is April 10)
  today <- lubridate::as_date(Sys.Date())
  narr_end <- today - lubridate::days(lubridate::mday(today))

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  PEcAn.logger::severeifnot(
    start_date >= narr_start,
    msg = paste0(
      "Start date ", start_date,
      " is before NARR start date ", narr_start
    )
  )

  PEcAn.logger::severeifnot(
    end_date <= narr_end,
    msg = paste0(
      "End date ", end_date,
      "is after NARR end date ", narr_end
    )
  )

  dates <- seq(start_date, end_date, by = "1 day")
  flx_df <- generate_narr_url(dates, TRUE)
  sfc_df <- generate_narr_url(dates, FALSE)

  # Load dimensions, etc. from first netCDF file
  nc1 <- PEcAn.utils::robustly(ncdf4::nc_open, n = 20, timeout = 0.5)(flx_df$url[1])
  on.exit(ncdf4::nc_close(nc1), add = TRUE)
  xy <- latlon2narr(nc1, lat.in, lon.in)

  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)
        || !requireNamespace("doParallel", quietly = TRUE)) {
      PEcAn.logger::logger.severe(
        "Could not find all packages needed for simultaneous NARR downloads. ",
        "Either run `install.packages(c(\"parallel\", \"doParallel\"))`, ",
        "or call get_NARR_thredds with `parallel = FALSE`.")
    }

    # Load in parallel
    PEcAn.logger::logger.info("Downloading in parallel")
    flx_df$flx <- TRUE
    sfc_df$flx <- FALSE
    get_dfs <- dplyr::bind_rows(flx_df, sfc_df)
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    flx <- NULL
    get_dfs$data <- foreach::`%dopar%`(
      foreach::foreach(
        url = get_dfs$url, flx = get_dfs$flx,
        .packages = c("PEcAn.data.atmosphere", "dplyr"),
        .export = c("get_narr_url", "robustly")
      ),
        PEcAn.utils::robustly(get_narr_url)(url, xy = xy, flx = flx)
    )
    flx_data_raw <- dplyr::filter(get_dfs, .data$flx)
    sfc_data_raw <- dplyr::filter(get_dfs, !.data$flx)
  } else {

    # Retrieve remaining variables by iterating over URLs
    npb <- nrow(flx_df) * nrow(narr_flx_vars) +
      nrow(sfc_df) * nrow(narr_sfc_vars)
    if (progress && requireNamespace("progress")) {
      pb <- progress::progress_bar$new(
        total = npb,
        format = "[:bar] :current/:total ETA: :eta"
      )
    } else {
      pb <- NULL
    }

    flx_data_raw <- flx_df %>%
      dplyr::mutate(
        data = purrr::map(
          url,
          PEcAn.utils::robustly(get_narr_url, n = 20, timeout = 1),
          xy = xy,
          flx = TRUE,
          pb = pb
        )
      )

    sfc_data_raw <- sfc_df %>%
      dplyr::mutate(
        data = purrr::map(
          url,
          PEcAn.utils::robustly(get_narr_url, n = 20, timeout = 1),
          xy = xy,
          flx = FALSE,
          pb = pb
        )
      )
  }
  flx_data <- post_process(flx_data_raw) %>%
    dplyr::select("datetime", narr_flx_vars$CF_name)
  sfc_data <- post_process(sfc_data_raw) %>%
    dplyr::select("datetime", narr_sfc_vars$CF_name)
  met_data <- dplyr::full_join(flx_data, sfc_data, by = "datetime") %>%
    dplyr::arrange(.data$datetime)

  if (drop_outside) {
    met_data <- met_data %>%
      dplyr::filter(.data$datetime >= start_date, .data$datetime < (end_date + lubridate::days(1)))
  }

  met_data
}

#' Post process raw NARR downloaded data frame
#'
#' @param dat Nested `tibble` from mapped call to [get_narr_url]
post_process <- function(dat) {
  dat %>%
    tidyr::unnest(.data$data) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(datetime = .data$startdate + lubridate::dhours(.data$dhours)) %>%
    dplyr::select(-"startdate", -"dhours") %>%
    dplyr::select("datetime", dplyr::everything()) %>%
    dplyr::select(-"url", "url")
}

#' Generate NARR url from a vector of dates
#'
#' Figures out file names for the given dates, based on NARR's convoluted and
#' inconsistent naming scheme.
#'
#' @param dates Vector of dates for which to generate URL
#' @param flx (Logical) If `TRUE`, format for `flx` variables. Otherwise,
#' format for `sfc` variables. See [narr_flx_vars].
#' @author Alexey Shiklomanov
generate_narr_url <- function(dates, flx) {
  ngroup <- if (flx) 8 else 10
  tag <- if (flx) "flx" else "sfc"
  base_url <- paste(
    # Have to login, so use Alexey Shiklomanov's account
    "http://ashiklom%40bu.edu:Thisis4theNARR@rda.ucar.edu",
    "thredds", "dodsC", "files", "g", "ds608.0", "3HRLY",
    sep = "/"
  )
  tibble::tibble(date = dates) %>%
    dplyr::mutate(
      year = lubridate::year(.data$date),
      month = lubridate::month(.data$date),
      daygroup = daygroup(.data$date, flx)
    ) %>%
    dplyr::group_by(.data$year, .data$month, .data$daygroup) %>%
    dplyr::summarize(
      startdate = min(.data$date),
      url = sprintf(
        "%s/%d/NARR%s_%d%02d_%s.tar",
        base_url,
        unique(.data$year),
        tag,
        unique(.data$year),
        unique(.data$month),
        unique(.data$daygroup)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("startdate", "url")
}

# Assign daygroup tag for a given date
daygroup <- function(date, flx) {
  mday <- lubridate::mday(date)
  mmax <- lubridate::days_in_month(date)
  if (flx) {
    dplyr::case_when(
      mday %in% 1:8 ~ "0108",
      mday %in% 9:16 ~ "0916",
      mday %in% 17:24 ~ "1724",
      mday >= 25 ~ paste0(25, mmax)
    )
  } else {
    dplyr::case_when(
      mday %in% 1:9 ~ "0109",
      mday %in% 10:19 ~ "1019",
      mday >= 20 ~ paste0(20, mmax)
    )
  }
}

#' Retrieve NARR data from a given URL
#'
#' @param url Full URL to NARR thredds file
#' @param xy Vector length 2 containing NARR coordinates
#' @param pb Progress bar R6 object (default = `NULL`)
#' @inheritParams generate_narr_url
#' @author Alexey Shiklomanov
get_narr_url <- function(url, xy, flx, pb = NULL) {
  stopifnot(length(xy) == 2, length(url) == 1, is.character(url))
  nc <- ncdf4::nc_open(url)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  timevar <- if (flx) "time" else "reftime"
  dhours <- ncdf4::ncvar_get(nc, timevar)
  # HACK: Time variable seems inconsistent.
  # Sometimes starts at 0, sometimes offset by 3.
  # This is a hack to make it always start at zero
  if (dhours[1] == 3) dhours <- dhours - 3
  narr_vars <- if (flx) narr_flx_vars else narr_sfc_vars
  result <- purrr::pmap(
    narr_vars %>% dplyr::select(variable = "NARR_name", unit = "units"),
    read_narr_var,
    nc = nc, xy = xy, flx = flx, pb = pb
  )
  names(result) <- narr_vars$CF_name
  dplyr::bind_cols(dhours = dhours, result)
}

#' Read a specific variable from a NARR NetCDF file
#'
#' @param nc `ncdf4` connection object
#' @param variable NARR name of variable to retrieve
#' @param unit Output unit of variable to retrieve
#' @inheritParams get_narr_url
#' @author Alexey Shiklomanov
read_narr_var <- function(nc, xy, variable, unit, flx, pb = NULL) {
  if (flx) {
    # Third dimension is height above ground -- first index is 2m above ground
    start <- c(xy, 1, 1)
    count <- c(1, 1, 1, -1)
  } else {
    # Third dimension is reference time; only has one index
    start <- c(xy, 1, 1)
    count <- c(1, 1, -1, -1)
  }
  nc_unit <- ncdf4::ncatt_get(nc, variable, "units")$value
  out <- ncdf4::ncvar_get(nc, variable, start = start, count = count)
  # Precipitation is a special case -- unit is actually precipitation per 3 hours
  # So, divide by seconds in 3 hours and change unit accordingly
  if (variable == "Total_precipitation_surface_3_Hour_Accumulation") {
    nc_unit <- paste0(nc_unit, "/s")
    out <- out / PEcAn.utils::ud_convert(3, "hours", "seconds")
  }
  final <- PEcAn.utils::ud_convert(out, nc_unit, unit)
  if (!is.null(pb)) pb$tick()
  final
}

#' NARR flux and sfc variables
narr_flx_vars <- tibble::tribble(
  ~CF_name, ~NARR_name, ~units,
  "air_temperature", "Temperature_height_above_ground", "Kelvin",
  "air_pressure", "Pressure_height_above_ground", "Pascal",
  "eastward_wind", "u-component_of_wind_height_above_ground", "m/s",
  "northward_wind", "v-component_of_wind_height_above_ground", "m/s",
  "specific_humidity", "Specific_humidity_height_above_ground", "g/g"
)

#' @rdname narr_flx_vars
narr_sfc_vars <- tibble::tribble(
  ~CF_name, ~NARR_name, ~units,
  "surface_downwelling_longwave_flux_in_air", "Downward_longwave_radiation_flux_surface_3_Hour_Average", "W/m2",
  "surface_downwelling_shortwave_flux_in_air", "Downward_shortwave_radiation_flux_surface_3_Hour_Average", "W/m2",
  "precipitation_flux", "Total_precipitation_surface_3_Hour_Accumulation", "kg/m2/s",
)

#' @rdname narr_flx_vars
narr_all_vars <- dplyr::bind_rows(narr_flx_vars, narr_sfc_vars)

#' Convert latitude and longitude coordinates to NARR indices
#'
#' @inheritParams read_narr_var
#' @inheritParams get_NARR_thredds
#' @return Vector length 2 containing NARR `x` and `y` indices, which can be
#' used in `ncdf4::ncvar_get` `start` argument.
#' @author Alexey Shiklomanov
latlon2narr <- function(nc, lat.in, lon.in) {
  narr_x <- ncdf4::ncvar_get(nc, "x")
  narr_y <- ncdf4::ncvar_get(nc, "y")
  ptrans <- latlon2lcc(lat.in, lon.in)
  x_ind <- which.min((ptrans$x - narr_x) ^ 2)
  y_ind <- which.min((ptrans$y - narr_y) ^ 2)
  c(x = x_ind, y = y_ind)
}

#' Convert latitude and longitude to x-y coordinates (in km) in Lambert
#' conformal conic projection (used by NARR)
#'
#' @inheritParams get_NARR_thredds
#' @return `sp::SpatialPoints` object containing transformed x and y
#' coordinates, in km, which should match NARR coordinates
#' @importFrom sf st_crs
  # ^not used directly here, but needed by sp::CRS.
  # sp lists sf in Suggests rather than Imports,
  # so importing it here to ensure it's available at run time
#' @author Alexey Shiklomanov
#' @export
latlon2lcc <- function(lat.in, lon.in) {
  pll <- sp::SpatialPoints(list(x = lon.in, y = lat.in), sp::CRS("+proj=longlat +datum=WGS84"))
  CRS_narr_string <- paste(
    "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96",
    "+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"
  )
  CRS_narr <- sp::CRS(CRS_narr_string)
  ptrans <- sp::spTransform(pll, CRS_narr)
  ptrans
}
