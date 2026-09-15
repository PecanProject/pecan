#' Download spatial CIMIS daily grids
#'
#' Pulls daily ASCII grids from the [California spatial CIMIS file
#' server](https://spatialcimis.water.ca.gov/cimis/). Writes
#' `outdir/YYYY/MM/DD/<variable>.asc.gz`, matching the layout on the public
#' server. Skips files that already exist.
#'
#' @param outdir Root output directory.
#' @param years Integer vector of calendar years.
#' @param variable CIMIS variable file stem (default `"ETo"` for reference ET).
#' @param base_url Spatial CIMIS HTTP root (override for tests).
#' @return Character vector of newly downloaded file paths (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' download.CIMIS(tempdir(), 2023)
#' download.CIMIS("/data/cimis", c(2023, 2024))
#' }
download.CIMIS <- function(
    outdir,
    years,
    variable = "ETo",
    base_url = "https://spatialcimis.water.ca.gov/cimis"
) {
  if (missing(outdir) || !nzchar(outdir)) {
    stop("outdir is required")
  }
  years <- sort(unique(as.integer(years)))
  if (!length(years)) {
    stop("years must be a non-empty integer vector")
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  options(timeout = max(600, getOption("timeout")))

  downloaded <- character(0)
  for (year in years) {
    dates <- seq(
      as.Date(sprintf("%d-01-01", year)),
      as.Date(sprintf("%d-12-31", year)),
      by = "day"
    )
    PEcAn.logger::logger.info("[CIMIS] year ", year, ": ", length(dates), " day(s)")
    for (i in seq_along(dates)) {
      d <- dates[[i]]
      y <- format(d, "%Y")
      m <- format(d, "%m")
      day <- format(d, "%d")
      dest_dir <- file.path(outdir, y, m, day)
      dest <- file.path(dest_dir, paste0(variable, ".asc.gz"))
      if (file.exists(dest) && isTRUE(file.info(dest)$size > 0)) {
        next
      }
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      url <- sprintf("%s/%s/%s/%s/%s.asc.gz", base_url, y, m, day, variable)
      ok <- tryCatch({
        status <- utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
        isTRUE(status == 0) && file.exists(dest) && file.info(dest)$size > 0
      }, error = function(e) {
        PEcAn.logger::logger.warn("[CIMIS] failed ", url, ": ", conditionMessage(e))
        FALSE
      })
      if (!ok) {
        if (file.exists(dest)) {
          unlink(dest)
        }
        stop("[CIMIS] download failed: ", url)
      }
      downloaded <- c(downloaded, dest)
    }
  }
  PEcAn.logger::logger.info("[CIMIS] done; ", length(downloaded), " new file(s)")
  invisible(downloaded)
}
