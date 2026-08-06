#!/usr/bin/env Rscript
# =============================================================================
# Extract CDL (Cropland Data Layer) fractions by parcel
#
# For each parcel polygon, computes the area fraction in each CDL crop code
# (fractions per categorical crop type, not just mode). Outputs w_total and
# sum_w2 per parcel for uncertainty propagation (n_eff = w_total^2/sum_w2;
# SE(frac) ~ sqrt(frac*(1-frac)/n_eff)). Used for gap-filling years with no
# LandIQ data (e.g. 2017) and for comparison with 2016/2018.
#
# Usage:
#   Rscript extract_cdl_fractions_by_parcel.R <year> [path_to_cdl_geotiff]
#   Rscript extract_cdl_fractions_by_parcel.R 2017
#   CDL_PATH=/path/to/cdl_2017.tif Rscript extract_cdl_fractions_by_parcel.R 2017
#
# Env:
#   CDL_PATH      -- path to CDL GeoTIFF for the year (overrides default)
#   CDL_DIR       -- directory containing CDL GeoTIFFs (default: ccmmf/CDL_data)
#   CDL_OUT_DIR   -- output dir for parquet (default: landiq-gapfill/cdl)
#   LANDIQ_GAPFILL_ROOT, CCMMF_LANDIQ_V4 -- paths (see scripts/R/paths.R)
#   CDL_CHUNK_SIZE -- parcel chunk size for extraction (default 5000)
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(arrow)
  library(exactextractr)
})
sf::sf_use_s2(FALSE)

# --- Paths ---
.fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L])
source(file.path(dirname(normalizePath(.fa, mustWork = FALSE)), "..", "R", "pkg_root.R"))
load_landiq_gapfill()
path_parcels_gpkg <- path_parcels_gpkg()
path_cdl_dir <- path_cdl_rasters()
path_out_root <- path_cdl_fractions()
chunk_size <- max(1L, suppressWarnings(as.integer(Sys.getenv("CDL_CHUNK_SIZE", "5000"))))

# --- Helpers ---
# exact_extract callback (summarize_df = TRUE): receives single df with value + coverage_fraction cols.
# Parcel ID is attached via append_cols (not include_cols) so each output row is labeled when FUN returns
# multiple rows per polygon (exactextractr 0.10+).
# w_total and sum_w2 support uncertainty propagation (same as NDTI/MSLSP): n_eff = w_total^2/sum_w2;
# SE(frac) ~ sqrt(frac*(1-frac)/n_eff) for proportion uncertainty. Multiple rows per polygon (one per CDL code).
summarize_cdl_fractions <- function(df, ...) {
  vcol <- names(df)[1]
  cfcol <- "coverage_fraction"
  if (!cfcol %in% names(df)) cfcol <- "coverage_area"
  values <- df[[vcol]]
  coverage_fractions <- df[[cfcol]]
  ok <- !is.na(values) & !is.na(coverage_fractions) & coverage_fractions > 0
  if (!any(ok)) return(data.frame(cdl_code = NA_integer_, frac = NA_real_, w_total = NA_real_, sum_w2 = NA_real_))
  w_total <- sum(coverage_fractions[ok])
  sum_w2  <- sum(coverage_fractions[ok]^2)
  if (w_total <= 0) return(data.frame(cdl_code = NA_integer_, frac = NA_real_, w_total = NA_real_, sum_w2 = NA_real_))
  dt <- data.table(cdl_code = as.integer(values[ok]), w = coverage_fractions[ok])
  dt <- dt[, .(w = sum(w)), by = cdl_code]
  dt[, frac := w / w_total]
  dt[, `:=`(w_total = w_total, sum_w2 = sum_w2)]
  as.data.frame(dt[, .(cdl_code, frac, w_total, sum_w2)])
}

# Load parcel polygons (optionally subset by parcel_ids). Returns sf in same CRS as raster.
load_parcels_for_cdl <- function(raster_path, parcel_ids = NULL) {
  layer_name <- st_layers(path_parcels_gpkg)$name[1]
  if (is.null(parcel_ids)) {
    parcels <- st_read(path_parcels_gpkg, layer = layer_name, quiet = TRUE)
  } else {
    parcel_ids <- unique(as.character(parcel_ids))
    ids_sql <- paste0("'", gsub("'", "''", parcel_ids, fixed = TRUE), "'", collapse = ",")
    parcels <- st_read(
      path_parcels_gpkg,
      query = sprintf('SELECT * FROM "%s" WHERE parcel_id IN (%s)', layer_name, ids_sql),
      quiet = TRUE
    )
  }
  parcels <- st_zm(parcels, drop = TRUE, what = "ZM")
  parcels <- parcels[!st_is_empty(st_geometry(parcels)), ]
  r_crs <- st_crs(terra::crs(terra::rast(raster_path)))
  st_transform(parcels, r_crs)
}

# Resolve path to CDL GeoTIFF for a given year.
get_cdl_path <- function(year, path_override = NULL) {
  if (nzchar(Sys.getenv("CDL_PATH"))) return(Sys.getenv("CDL_PATH"))
  if (!is.null(path_override) && nzchar(path_override)) return(path_override)
  yr <- as.integer(year)
  candidates <- c(
    file.path(path_cdl_dir, paste0("cdl_", yr, ".tif")),
    file.path(path_cdl_dir, paste0("CDL_", yr, ".tif")),
    file.path(path_cdl_dir, paste0("ca_", yr, "_30m_cdls.tif"))
  )
  for (p in candidates) if (file.exists(p)) return(p)
  pat <- list.files(path_cdl_dir, pattern = paste0(".*", yr, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  if (length(pat) > 0) return(pat[1])
  NULL
}

# --- Main ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript extract_cdl_fractions_by_parcel.R <year> [path_to_cdl_geotiff]")
}
year_arg <- as.integer(args[1])
cdl_path_arg <- if (length(args) >= 2) args[2] else NULL

cdl_path <- get_cdl_path(year_arg, cdl_path_arg)
if (is.null(cdl_path) || !file.exists(cdl_path)) {
  stop("CDL GeoTIFF not found for year ", year_arg, ". Set CDL_PATH or CDL_DIR, or pass path as second argument. See landiq-gapfill/README.md (CDL section).")
}

message("CDL: ", cdl_path)
message("Loading parcels and reprojecting to CDL CRS...")
parcels_sf <- load_parcels_for_cdl(cdl_path)
if (nrow(parcels_sf) == 0) stop("No parcel geometries loaded.")
parcels_sf$parcel_id <- as.character(parcels_sf$parcel_id)

n_parcels <- nrow(parcels_sf)
message("Parcels: ", n_parcels, " (chunk size ", chunk_size, ")")

cdl_rast <- terra::rast(cdl_path)
dir.create(path_out_root, recursive = TRUE, showWarnings = FALSE)

# Process in chunks to limit memory and allow progress.
chunks <- split(seq_len(n_parcels), ceiling(seq_len(n_parcels) / chunk_size))
results <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  idx <- chunks[[i]]
  sub <- parcels_sf[idx, ]
  message("Chunk ", i, "/", length(chunks), " (parcels ", min(idx), "-", max(idx), ")")
  # summarize_df with multiple rows per polygon: exactextractr stacks rows and repeats
  # append_cols on each row (include_cols does NOT; see exactextractr >= 0.9 docs).
  extracted <- exactextractr::exact_extract(
    cdl_rast, sub, summarize_cdl_fractions,
    progress = FALSE, summarize_df = TRUE, append_cols = "parcel_id"
  )
  out_dt <- as.data.table(extracted)
  if (!"parcel_id" %in% names(out_dt)) {
    stop("exact_extract: missing parcel_id; use append_cols='parcel_id' with summarize_df.")
  }
  out_dt[, parcel_id := as.character(parcel_id)]
  out_dt[, year := year_arg]
  results[[i]] <- out_dt
}

result <- rbindlist(results)
# Drop NA cdl_code rows (no data)
result <- result[!is.na(cdl_code)]

out_file <- file.path(path_out_root, paste0("cdl_fractions_year=", year_arg, ".parquet"))
arrow::write_parquet(result, out_file)
message("Wrote ", nrow(result), " rows to ", out_file)
