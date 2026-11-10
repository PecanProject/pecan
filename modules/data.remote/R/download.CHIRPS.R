#' Download CHIRPS 2.0 daily precipitation NetCDF files
#'
#' Downloads one global NetCDF per calendar year from the Climate Hazards Center
#' ([CHIRPS 2.0 daily p05](https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/)).
#' Files are global; subset to a region in a separate step. Skips years that
#' already exist under `outdir`.
#'
#' @param outdir Directory for `chirps-v2.0.YEAR.days_RESOLUTION.nc`.
#' @param years Integer vector of calendar years.
#' @param resolution Character resolution code (default `"p05"`).
#' @param base_url CHC HTTP root for daily NetCDF (override for tests).
#' @return Character vector of output file paths (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' download.CHIRPS(tempdir(), 2023)
#' download.CHIRPS("/data/chirps", c(2023, 2024))
#' }
download.CHIRPS <- function(
    outdir,
    years,
    resolution = "p05",
    base_url = sprintf(
      "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/%s",
      resolution
    )
) {
  if (missing(outdir) || !nzchar(outdir)) {
    stop("outdir is required")
  }
  years <- sort(unique(as.integer(years)))
  if (!length(years)) {
    stop("years must be a non-empty integer vector")
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  options(timeout = max(7200, getOption("timeout")))

  outfiles <- character(length(years))
  for (i in seq_along(years)) {
    year <- years[[i]]
    fname <- sprintf("chirps-v2.0.%d.days_%s.nc", year, resolution)
    dest <- file.path(outdir, fname)
    url <- file.path(base_url, fname)
    outfiles[[i]] <- dest
    if (file.exists(dest) && isTRUE(file.info(dest)$size > 0)) {
      PEcAn.logger::logger.info("[CHIRPS] skip (exists): ", dest)
      next
    }
    PEcAn.logger::logger.info("[CHIRPS] downloading ", year, " -> ", dest)
    status <- tryCatch(
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      error = function(e) e
    )
    if (inherits(status, "error")) {
      stop("[CHIRPS] download failed for ", year, ": ", conditionMessage(status))
    }
    if (!file.exists(dest) || isTRUE(file.info(dest)$size == 0)) {
      stop("[CHIRPS] download failed for ", year, " (missing or empty file)")
    }
  }
  invisible(outfiles)
}
