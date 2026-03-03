#' Prepare ERA5 fallback CF meteorological data
#'
#' Downloads ERA5 data (if required) and converts it to a CF-compliant
#' NetCDF file for use as a fallback dataset in coverage-based gap-filling
#' workflows.
#'
#' This helper performs dataset-level preparation only. It does not
#' perform CF-level merging or gap-filling; those steps are handled
#' separately by \code{metgapfill_with_fallback()}.
#'
#' @param fill_vars character vector. Variables requiring fallback.
#'   If empty, the function returns \code{NULL} and no ERA5 data are
#'   downloaded or prepared.
#' @param start_date character or Date. Start date in "YYYY-MM-DD" format.
#' @param end_date character or Date. End date in "YYYY-MM-DD" format.
#' @param site_id character. Site identifier used for naming ERA5 output.
#' @param site_lat numeric. Latitude of the site.
#' @param site_lon numeric. Longitude of the site.
#' @param dirs named list. Must contain directory paths:
#'   \code{era5_downloads} (raw ERA5 data) and
#'   \code{era5_cf} (CF-converted output).
#' @param dataset character. CDS dataset name passed to
#'   \code{download.ERA5_cds()}. Default:
#'   \code{"reanalysis-era5-single-levels"}.
#' @param era5_user character. CDS user ID for authentication.
#' @param era5_key character. CDS API key for authentication.
#' @param overwrite logical. Whether to overwrite existing ERA5
#'   downloads and CF outputs.
#' @param verbose logical. Whether to print detailed log messages.
#'
#' @return Character string giving the path to the generated CF NetCDF file,
#'   or \code{NULL} if no fallback variables were requested.
#'
#' @details
#' Scope and limitations:
#' \itemize{
#'   \item This helper assumes ERA5 extraction produces a single CF NetCDF
#'         file covering the requested date range.
#'   \item Merging across multiple years of ERA5 CF files is not currently
#'         supported.
#'   \item The values in \code{fill_vars} are passed directly to
#'         \code{download.ERA5_cds()} and are therefore expected to match
#'         CDS variable names.
#'   \item Variable-name translation between CF conventions and CDS
#'         naming conventions is intentionally out of scope.
#' }
#'
#' This function is part of the staged refactor described in issue-#3605
#' and is designed to remain narrowly focused on ERA5 dataset preparation.
#'
#' @noRd

prepare_era5_fallback_cf <- function(
  fill_vars,
  start_date,
  end_date,
  site_id,
  site_lat,
  site_lon,
  dirs,
  dataset = "reanalysis-era5-single-levels",
  era5_user = NULL,
  era5_key = NULL,
  overwrite = FALSE,
  verbose = FALSE
) {
  if (is.null(fill_vars) || length(fill_vars) == 0) {
    return(NULL)
  }

  stopifnot(
    is.character(fill_vars),
    is.numeric(site_lat),
    is.numeric(site_lon),
    dir.exists(dirs$era5_downloads),
    dir.exists(dirs$era5_cf)
  )

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    stop("Invalid start_date or end_date")
  }

  if (is.null(era5_user) || is.null(era5_key)) {
    stop("ERA5 credentials (era5_user, era5_key) must be provided")
  }


  # Determine required years
  start_year <- as.integer(format(start_date, "%Y"))
  end_year <- as.integer(format(end_date, "%Y"))
  req_years <- seq(start_year, end_year)

  # Detect existing ERA5 raw files
  raw_files <- list.files(
    dirs$era5_downloads,
    pattern = "^ERA5_\\d{4}\\.nc$",
    full.names = TRUE
  )

  existing_years <- integer(0)

  if (length(raw_files) > 0) {
    year_matches <- regmatches(
      basename(raw_files),
      regexpr("\\d{4}", basename(raw_files))
    )
    existing_years <- suppressWarnings(as.integer(year_matches))
    existing_years <- existing_years[!is.na(existing_years)]
  }

  years_to_download <- if (overwrite) {
    req_years
  } else {
    setdiff(req_years, existing_years)
  }

  # Download missing ERA5 data
  if (length(years_to_download) > 0) {
    if (verbose) {
      PEcAn.logger::logger.info(
        paste(
          "Downloading ERA5 years:",
          paste(years_to_download, collapse = ", ")
        )
      )
    }

    # ERA5 variables must be CDS names (not CF names)
    cds_variables <- fill_vars

    # minimal bounding box around site
    extent <- c(
      site_lon - 0.1,
      site_lon + 0.1,
      site_lat - 0.1,
      site_lat + 0.1
    )

    PEcAn.data.atmosphere::download.ERA5_cds(
      outfolder  = dirs$era5_downloads,
      start_date = paste0(min(years_to_download), "-01-01"),
      end_date   = paste0(max(years_to_download), "-12-31"),
      extent     = extent,
      variables  = cds_variables,
      dataset    = dataset,
      user       = era5_user,
      key        = era5_key
    )
  }

  # Convert ERA5 raw → CF
  if (verbose) {
    PEcAn.logger::logger.info("Converting ERA5 raw files to CF format")
  }

  cf_output_dirs <- PEcAn.data.atmosphere::extract.nc.ERA5(
    slat       = site_lat,
    slon       = site_lon,
    in.path    = dirs$era5_downloads,
    in.prefix  = "ERA5_",
    start_date = start_date,
    end_date   = end_date,
    outfolder  = dirs$era5_cf,
    newsite    = paste0(site_id, "_ERA5"),
    overwrite  = overwrite,
    verbose    = verbose
  )

  # Strict CF file discovery
  cf_files <- unlist(lapply(cf_output_dirs, function(d) {
    list.files(
      d,
      pattern = "\\.nc$",
      full.names = TRUE,
      recursive = TRUE
    )
  }))

  if (length(cf_files) == 0) {
    stop("No ERA5 CF files found after conversion")
  }

  if (length(cf_files) > 1) {
    stop("Multiple ERA5 CF files found; merging across years not yet supported")
  }

  fallback_cf <- cf_files[1]

  if (!file.exists(fallback_cf)) {
    stop("Resolved fallback CF file does not exist")
  }

  if (verbose) {
    PEcAn.logger::logger.info(
      paste("Prepared ERA5 fallback CF file:", fallback_cf)
    )
  }

  return(fallback_cf)
}
