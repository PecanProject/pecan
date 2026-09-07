#!/usr/bin/env Rscript
# =============================================================================
# Download CDL (Cropland Data Layer) GeoTIFF from NASS and clip to California.
#
# NASS publishes a national 30 m zip per year. 
#
# Saves: $CDL_DIR/cdl_YYYY.tif
#
# Usage:
#   Rscript download_cdl_nass.R 2017
#   Rscript download_cdl_nass.R 2023,2024
#   Rscript download_cdl_nass.R 2016 2017 2018
#   CDL_DIR=/path/to/dir Rscript download_cdl_nass.R 2017
#
# Env:
#   CDL_DIR  -- directory to save GeoTIFFs (default: $CCMMF_ROOT/CDL)
#   CCMMF_ROOT -- used when CDL_DIR is unset
#   LANDIQ_GAPFILL_ROOT -- optional; auto-detected from script path
# =============================================================================

suppressPackageStartupMessages(library(terra))

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "..", "R", "pkg_root.R"))
load_landiq_gapfill()
path_cdl_dir <- path_cdl_rasters()

# Padded WGS84 box covering California (includes a little ocean / NV / AZ).
CA_WGS84 <- c(xmin = -124.6, xmax = -114.0, ymin = 32.4, ymax = 42.2)

nass_cdl_zip_url <- function(year) {
  sprintf(
    "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/datasets/%d_30m_cdls.zip",
    as.integer(year)
  )
}

ca_extent_in_crs <- function(crs) {
  v <- terra::vect(terra::ext(CA_WGS84), crs = "EPSG:4326")
  terra::ext(terra::project(v, crs))
}

find_cdl_raster <- function(root) {
  hits <- list.files(
    root,
    pattern = "_30m_cdls\\.(tif|img)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hits) == 0L) {
    stop("No *_30m_cdls.tif/img in unzipped NASS archive under ", root)
  }
  hits[[1L]]
}

download_cdl_year <- function(year, outdir = path_cdl_dir) {
  year <- as.integer(year)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(outdir, paste0("cdl_", year, ".tif"))

  if (file.exists(outfile) && isTRUE(file.info(outfile)$size > 0)) {
    message(year, ": already exists ", outfile, " (skip or delete to re-download)")
    return(invisible(outfile))
  }

  url <- nass_cdl_zip_url(year)
  work <- tempfile(sprintf("cdl_%d_", year))
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  zip_path <- file.path(work, sprintf("%d_30m_cdls.zip", year))

  message(year, ": downloading national 30 m CDL from NASS...")
  message(year, ": ", url)
  options(timeout = max(7200, getOption("timeout")))
  status <- tryCatch(
    download.file(url, destfile = zip_path, mode = "wb", quiet = FALSE),
    error = function(e) e
  )
  if (inherits(status, "error")) {
    stop(year, ": download failed: ", conditionMessage(status))
  }
  if (!file.exists(zip_path) || isTRUE(file.info(zip_path)$size == 0)) {
    stop(year, ": download failed (no file or empty)")
  }

  message(year, ": unzipping...")
  utils::unzip(zip_path, exdir = work)
  src <- find_cdl_raster(work)

  message(year, ": clipping to California...")
  r <- terra::rast(src)
  e <- terra::align(ca_extent_in_crs(terra::crs(r)), r, snap = "out")
  r_ca <- terra::crop(r, e)
  tmp_out <- paste0(outfile, ".tmp.tif")
  if (file.exists(tmp_out)) unlink(tmp_out)
  terra::writeRaster(
    r_ca,
    tmp_out,
    overwrite = TRUE,
    datatype = "INT1U",
    gdal = c("COMPRESS=LZW", "TILED=YES")
  )
  if (!file.exists(tmp_out) || isTRUE(file.info(tmp_out)$size == 0)) {
    if (file.exists(tmp_out)) unlink(tmp_out)
    stop(year, ": clip/write failed (no file or empty)")
  }
  file.rename(tmp_out, outfile)
  message(year, ": saved ", outfile)
  invisible(outfile)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript download_cdl_nass.R <YEARS>  (e.g. 2023,2024 or 2016 2017 2018)")
}
years <- parse_cli_gapfill_years(args)
if (any(years < 2008L)) {
  stop("CDL years must be >= 2008; got: ", paste(years, collapse = ","))
}
for (yr in years) download_cdl_year(yr, outdir = path_cdl_dir)
