#!/usr/bin/env Rscript
# =============================================================================
# Download CDL (Cropland Data Layer) GeoTIFF via CropScapeR (CropScape API)
#
# Downloads California statewide (aoi = "06"). Saves to CDL_DIR/cdl_YYYY.tif.
#
# Usage:
#   Rscript download_cdl_nass.R 2017
#   Rscript download_cdl_nass.R 2023,2024
#   Rscript download_cdl_nass.R 2016 2017 2018
#   CDL_DIR=/path/to/dir Rscript download_cdl_nass.R 2017
#
# Env:
#   CDL_DIR  -- directory to save GeoTIFFs (default: ccmmf/CDL)
#   CCMMF_ROOT -- ccmmf repo root (when CDL_DIR is unset)
#   LANDIQ_GAPFILL_ROOT -- optional; auto-detected from script path
# =============================================================================

# Install CropScapeR if not present (skip if already installed)
if (!requireNamespace("CropScapeR", quietly = TRUE)) {
  message("Installing CropScapeR...")
  install.packages("CropScapeR", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(library(CropScapeR))

.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "..", "R", "pkg_root.R"))
load_landiq_gapfill()
path_cdl_dir <- path_cdl_rasters()

download_cdl_year <- function(year, outdir = path_cdl_dir) {
  year <- as.integer(year)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(outdir, paste0("cdl_", year, ".tif"))

  if (file.exists(outfile)) {
    message(year, ": already exists ", outfile, " (skip or delete to re-download)")
    return(invisible(outfile))
  }

  message(year, ": downloading California statewide from CropScape...")
  GetCDLData(aoi = "06", year = year, type = "f", format = "raster",
             save_path = outfile, readr = FALSE, tol_time = 300)
  if (!file.exists(outfile) || file.info(outfile)$size == 0) {
    if (file.exists(outfile)) unlink(outfile)
    stop(year, ": download failed (no file or empty)")
  }
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
