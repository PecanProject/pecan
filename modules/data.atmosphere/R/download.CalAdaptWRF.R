#' Stage native Cal-Adapt WRF inputs for a site
#'
#' Downloads source Zarr chunks without converting their values and writes one
#' JSON source manifest per year. Requires the source-staging API in caladaptaer
#' and R-linked GDAL >= 3.9. Use [met2CF.CalAdaptWRF()] for CF conversion.
#'
#' @param outfolder Directory for source manifests and cached chunks.
#' @param start_date,end_date Requested dates; complete calendar years are staged.
#' @param site_id Unique site identifier, retained as a character string.
#' @param lat.in,lon.in Site coordinates in WGS84 decimal degrees.
#' @param model,scenario WRF source and experiment identifiers.
#' @param resolution Native WRF domain: d01, d02, or d03.
#' @param cache_dir Persistent source cache; defaults to a subdirectory of outfolder.
#' @param overwrite Replace existing manifests? Cached source chunks are reused.
#' @param verbose Print progress messages?
#' @param ... Additional arguments, currently ignored.
#' @return Invisibly, a data frame of file information for PEcAn.
#' @md
#' @export
#' @author Akash B V, David LeBauer
download.CalAdaptWRF <- function(outfolder, start_date, end_date,
                                 site_id, lat.in, lon.in,
                                 model = "CESM2", scenario = "ssp370",
                                 resolution = "d03", cache_dir = NULL,
                                 overwrite = FALSE, verbose = FALSE, ...) {
  .caladapt_require("cae_stage_zarr")
  site_id <- as.character(site_id)
  if (length(site_id) != 1L || is.na(site_id) || !nzchar(site_id) ||
    grepl("[/\\\\]", site_id) || site_id %in% c(".", "..")) {
    PEcAn.logger::logger.severe("Provide one safe, nonempty site_id")
  }
  points <- data.frame(site_id = site_id, lon = as.numeric(lon.in), lat = as.numeric(lat.in))
  if (nrow(points) != 1L || anyNA(points) ||
    !is.finite(points$lon) || !is.finite(points$lat)) {
    PEcAn.logger::logger.severe("Provide one finite latitude and longitude")
  }
  for (id in list(model, scenario)) {
    if (length(id) != 1L || is.na(id) || !grepl("^[[:alnum:]_-]+$", id)) {
      PEcAn.logger::logger.severe("Provide one model and scenario identifier")
    }
  }
  resolution <- match.arg(resolution, c("d01", "d02", "d03"))
  prefix <- paste("CalAdaptWRF", model, scenario, resolution, sep = ".")
  years <- .caladapt_years(start_date, end_date)
  dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
  if (is.null(cache_dir)) cache_dir <- file.path(outfolder, "source-cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir)
  available <- caladaptaer::cae_check_variables(model, scenario, resolution = resolution)
  variables <- c("t2", "psfc", "q2", "swdnb", "lwdnb", "u10", "v10")
  precip <- if (any(available$variable == "prec" & available$available)) "prec" else c("rainc", "rainnc")
  variables <- c(variables, precip)
  if (!all(variables %in% available$variable[available$available])) {
    PEcAn.logger::logger.severe("Required WRF variables are unavailable for this product")
  }
  files <- file.path(outfolder, paste(prefix, years, "json", sep = "."))
  for (i in seq_along(years)) {
    year <- years[i]
    start <- as.POSIXct(paste0(year, "-01-01"), tz = "UTC")
    end <- as.POSIXct(paste0(year, "-12-31 23:00:00"), tz = "UTC")
    sources <- lapply(variables, function(variable) {
      if (verbose) PEcAn.logger::logger.info("Staging ", variable, " for ", year)
      caladaptaer::cae_stage_zarr(variable, model, scenario, points, cache_dir,
        start_time = start - if (variable %in% c("rainc", "rainnc")) 3600 else 0,
        end_time = end, resolution = resolution
      )
    }) |>
      stats::setNames(variables)
    manifest <- list(
      model = model, scenario = scenario, resolution = resolution,
      year = year, sites = points, sources = sources
    )
    # Stage first even on resume, so missing source chunks are repaired.
    if (!file.exists(files[i]) || overwrite ||
      !identical(tryCatch(jsonlite::read_json(files[i], simplifyVector = TRUE),
        error = function(e) NULL
      ), jsonlite::fromJSON(jsonlite::toJSON(manifest, auto_unbox = TRUE, digits = NA)))) {
      tmp <- tempfile(tmpdir = outfolder)
      tryCatch(
        {
          jsonlite::write_json(manifest, tmp, auto_unbox = TRUE, digits = NA)
          if (!file.rename(tmp, files[i])) PEcAn.logger::logger.severe("Could not publish source manifest")
        },
        finally = unlink(tmp)
      )
    }
  }
  .caladapt_file_info(files, prefix, years, "CalAdaptWRF source manifest", "application/json")
}
