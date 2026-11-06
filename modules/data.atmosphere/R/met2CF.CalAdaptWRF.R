#' Convert staged Cal-Adapt WRF inputs to CF meteorology
#'
#' Reads source manifests from [download.CalAdaptWRF()] and delegates extraction,
#' unit conversion, provenance, and validated CF output reuse to caladaptaer.
#' Native source calendars and missing values are retained. No gap filling is
#' performed. Keep the source cache with its manifests until conversion finishes.
#'
#' @param in.path Directory containing annual JSON source manifests.
#' @param in.prefix Manifest filename prefix, as returned in dbfile.name.
#' @param outfolder Directory for CF files.
#' @param start_date,end_date Requested dates; only years are used.
#' @param overwrite Replace matching CF outputs?
#' @param verbose Print progress messages?
#' @param ... Additional arguments, currently ignored.
#' @return Invisibly, a data frame of CF file information for PEcAn.
#' @md
#' @export
met2CF.CalAdaptWRF <- function(in.path, in.prefix, outfolder, start_date, end_date,
                               overwrite = FALSE, verbose = FALSE, ...) {
  .caladapt_require("cae_build_met_drivers")
  years <- .caladapt_years(start_date, end_date)
  files <- character(length(years))
  for (i in seq_along(years)) {
    path <- file.path(in.path, paste(in.prefix, years[i], "json", sep = "."))
    manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
    required <- c("model", "scenario", "resolution", "year", "sites", "sources")
    if (!all(required %in% names(manifest)) || !identical(as.integer(manifest$year), years[i])) {
      PEcAn.logger::logger.severe("Invalid WRF source manifest: ", path)
    }
    expected <- paste("CalAdaptWRF", manifest$model, manifest$scenario, manifest$resolution, sep = ".")
    if (!identical(in.prefix, expected) || nrow(manifest$sites) != 1L ||
      !all(file.exists(unlist(manifest$sources)))) {
      PEcAn.logger::logger.severe("Source manifest identity or cache does not match: ", path)
    }
    # The parent directory preserves product identity; the builder owns output reuse.
    built <- caladaptaer::cae_build_met_drivers(
      sites = manifest$sites, model = manifest$model, scenario = manifest$scenario,
      start_year = years[i], end_year = years[i],
      outdir = file.path(outfolder, in.prefix), resolution = manifest$resolution,
      overwrite = overwrite, verbose = verbose, source = as.list(manifest$sources)
    )
    files[i] <- file.path(outfolder, paste(in.prefix, years[i], "nc", sep = "."))
    # Keep the product identity in the prefix consumed by met2model functions.
    if (!file.exists(files[i]) || overwrite ||
      unname(tools::md5sum(files[i])) != unname(tools::md5sum(built$file[1]))) {
      tmp <- tempfile(tmpdir = outfolder, fileext = ".nc")
      tryCatch(
        {
          if (!file.copy(built$file[1], tmp) || !file.rename(tmp, files[i])) {
            PEcAn.logger::logger.severe("Could not publish CF file: ", files[i])
          }
        },
        finally = unlink(tmp)
      )
    }
  }
  .caladapt_file_info(files, in.prefix, years, "CF Meteorology", "application/x-netcdf")
}

.caladapt_require <- function(api) {
  if (!requireNamespace("caladaptaer", quietly = TRUE) ||
    !all(c(api, "cae_stage_zarr") %in% getNamespaceExports("caladaptaer"))) {
    PEcAn.logger::logger.severe("Install caladaptaer with the WRF source-staging and bulk-export APIs")
  }
}

.caladapt_years <- function(start_date, end_date) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  if (length(start) != 1L || length(end) != 1L || is.na(start) || is.na(end) || start > end) {
    PEcAn.logger::logger.severe("Provide ordered start_date and end_date")
  }
  seq.int(lubridate::year(start), lubridate::year(end))
}

.caladapt_file_info <- function(files, prefix, years, formatname, mimetype) {
  invisible(data.frame(
    file = normalizePath(files, mustWork = TRUE),
    host = PEcAn.remote::fqdn(), mimetype = mimetype, formatname = formatname,
    startdate = paste0(years, "-01-01 00:00:00"),
    enddate = paste0(years, "-12-31 23:59:59"), dbfile.name = prefix
  ))
}
