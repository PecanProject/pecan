#!/usr/bin/env Rscript

options(
  clustermq.scheduler = "sge",
  clustermq.template  = ".clustermq_sge.tmpl"
)

library(terra)
library(progress)
library(arrow)
library(clustermq)

n_workers <- 20
walltime <- "02:00:00"

outdir <- "_results_v2/daily-raw"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

W <- readRDS("_results_v2/spatial_weights.rds")

cimis_manifest <- "cimis_files.txt"
years <- seq(2015, 2024)
if (!file.exists(cimis_manifest)) {
  get_cimis_files <- function(year) {
    ydir <- file.path(cimis_root, year)
    stopifnot(dir.exists(ydir))
    list.files(
      ydir,
      pattern = "ETo\\.asc\\.gz$",
      full.names = TRUE,
      recursive = TRUE
    )
  }
  ylist <- map(years, get_cimis_files, .progress = TRUE)
  cimis_files <- sort(unlist(ylist))
  writeLines(cimis_files, cimis_manifest)
} else {
  cimis_files <- readLines(cimis_manifest)
}

# Extract
process_file <- function(fname, W, outdir) {
  day <- basename(dirname(fname))
  month <- basename(dirname(dirname(fname)))
  year <- basename(dirname(dirname(dirname(fname))))
  datestr <- paste(year, month, day, sep = "-")
  outfile <- file.path(outdir, paste0(datestr, ".parquet"))
  if (file.exists(outfile)) {
    return(outfile)
  }
  r <- tryCatch({
    terra::rast(file.path("/vsigzip", fname))
  }, error = function(e) {
    # Some files aren't actually zipped. Try them this way.
    message("Error reading zipped. Trying unzipped. --- ", fname)
    terra::rast(fname)
  })
  rsize <- terra::size(r)
  if (rsize == 285600) {
    # Alternate size files -- use alternate weights
    W <- readRDS("_results_v2/spatial_weights_alt.rds")
  } else if (rsize != 276000) {
    stop("File ", fname, " has unexpected size ", rsize)
  }
  terra::crs(r) <- "EPSG:3310"
  v <- terra::values(r, mat = FALSE)
  date <- as.Date(datestr)
  na_mask <- is.na(v)
  v[na_mask] <- 0
  valid_mask <- as.numeric(!na_mask)
  weight_sums <- as.numeric(W %*% valid_mask)
  et_vals <- as.numeric(W %*% v)
  et_vals[weight_sums == 0] <- NA_real_   # All values are NA
  result <- tibble::tibble(
    parcel_id = as.numeric(rownames(W)),
    date = date,
    etref_mm_day = et_vals
  )
  arrow::write_parquet(result, outfile)
  invisible(outfile)
}

# outfiles_raw <- purrr::map(
#   cimis_files,
#   process_file,
#   W = W,
#   outdir = outdir,
#   .progress = TRUE
# )

cimis_long <- Q(
  fun     = process_file,
  fname = cimis_files,
  const   = list(W = W, outdir = outdir),
  n_jobs  = n_workers,         # SGE array size — persistent worker processes
  template = list(cores = 1, walltime = walltime),
  fail_on_error = FALSE
)
