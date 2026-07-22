#!/usr/bin/env Rscript
# Extract pixel-level MSLSP cycle 1 and 2 from .nc for specific parcels.
# Usage: Rscript mslsp_pixel_extract_parcels.R
# Output: CSV tables per parcel-year + analysis

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
})
sf::sf_use_s2(FALSE)

# --- Config ---
path_management   <- "/projectnb/dietzelab/ccmmf/management"
path_landiq_v4    <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1"
path_parcels_gpkg <- file.path(path_landiq_v4, "parcels-consolidated.gpkg")
path_tilemap      <- file.path(path_management, "hls_parcel_tile_map_v4.1.rds")
mslsp_legacy      <- "/projectnb/dietzelab/ccmmf/HLS_data"
mslsp_new_base    <- "/projectnb/dietzelab/ccmmf/data_phen/output"
out_dir           <- file.path(path_management, "phenology/mslsp_pixel_tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Target parcels: (parcel_id, year)
targets <- data.table(
  parcel_id = c("162617", "162618", "162618", "162674", "162677"),
  year      = c(2020L,   2020L,    2023L,    2022L,    2023L)
)

mslsp_nc_path <- function(tile_id, year) {
  candidates <- c(
    file.path(mslsp_legacy, paste0("MSLSP_", tile_id, "_", year, ".nc")),
    file.path(mslsp_new_base, tile_id, "phenoMetrics", paste0("MSLSP_", tile_id, "_", year, ".nc"))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# Load tile map and parcel geometry
tm <- as.data.table(readRDS(path_tilemap))
layer <- st_layers(path_parcels_gpkg)$name[1]
pids <- unique(targets$parcel_id)
esc <- gsub("'", "''", pids, fixed = TRUE)
q <- sprintf("SELECT * FROM \"%s\" WHERE parcel_id IN (%s)", layer, paste0("'", esc, "'", collapse = ","))
parcels_sf <- st_read(path_parcels_gpkg, query = q, quiet = TRUE)
parcels_sf$parcel_id <- as.character(parcels_sf$parcel_id)

# Metrics to extract
cy1 <- c("OGI", "50PCGI", "OGMx", "Peak", "OGD", "50PCGD", "OGMn", "EVImax", "EVIamp", "EVIarea", "NumCycles", "numObs")
cy2 <- paste0(c("OGI", "50PCGI", "OGMx", "Peak", "OGD", "50PCGD", "OGMn", "EVImax", "EVIamp", "EVIarea"), "_2")

all_results <- list()
for (i in seq_len(nrow(targets))) {
  pid <- targets$parcel_id[i]
  yr  <- targets$year[i]
  row_tm <- tm[parcel_id == pid]
  if (nrow(row_tm) == 0) {
    message("No tile map for ", pid, " ", yr)
    next
  }
  tiles <- unlist(strsplit(row_tm$tileIDs[1], ",", fixed = TRUE))
  poly <- parcels_sf[parcels_sf$parcel_id == pid, ]
  if (nrow(poly) == 0) {
    message("No geometry for ", pid)
    next
  }

  pixels_list <- list()
  for (tile_id in tiles) {
    nc_path <- mslsp_nc_path(tile_id, yr)
    if (is.na(nc_path) || !file.exists(nc_path)) {
      message("NC not found: ", tile_id, " ", yr)
      next
    }
    r <- try(rast(nc_path), silent = TRUE)
    if (inherits(r, "try-error")) { message("Could not read ", nc_path); next }

    poly_tr <- st_transform(poly, st_crs(r))
    v <- vect(poly_tr)
    # Extract all cell values; use cells=TRUE to get cell index
    ex <- terra::extract(r, v, cells = TRUE, xy = TRUE, exact = TRUE)
    if (is.null(ex) || nrow(ex) == 0) next

    ex <- as.data.table(ex)
    ex[, parcel_id := pid]
    ex[, year := yr]
    ex[, tile_id := tile_id]
    # Remove ID, cell, x, y from merge - keep for pixel ID
    setnames(ex, "ID", "poly_id")
    pixels_list[[length(pixels_list) + 1]] <- ex
  }
  if (length(pixels_list) == 0) next
  px <- rbindlist(pixels_list)
  # If parcel spans tiles, deduplicate by (x,y) - same pixel might be on boundary
  if (length(tiles) > 1) {
    px <- unique(px, by = c("x", "y"))
  }
  all_results[[paste0(pid, "_", yr)]] <- px
}

# Write per parcel-year and combined
combined <- rbindlist(all_results, fill = TRUE)

# Write CSVs
for (k in names(all_results)) {
  f <- file.path(out_dir, paste0("mslsp_pixels_", k, ".csv"))
  fwrite(all_results[[k]], f)
  message("Wrote ", f, " (", nrow(all_results[[k]]), " pixels)")
}
fwrite(combined, file.path(out_dir, "mslsp_pixels_all.csv"))
message("Wrote ", file.path(out_dir, "mslsp_pixels_all.csv"))

# --- Analysis: overlap at pixel vs parcel level ---
# For each parcel-year, compute:
# 1. Pixel-level: fraction of pixels where OGMn_cy1 > OGI_cy2 (cycle 1 ends after cycle 2 starts = overlap)
# 2. Parcel-mean cycle 1 vs 2 - do parcel means overlap?

overlap_analysis <- function(dt) {
  nms <- names(dt)
  has_ogi1  <- "OGI" %in% nms
  has_ogmn1 <- "OGMn" %in% nms
  has_ogi2  <- "OGI_2" %in% nms
  has_ogmn2 <- "OGMn_2" %in% nms
  if (!all(has_ogi1, has_ogmn1, has_ogi2, has_ogmn2)) return(NULL)

  # Valid pixels: non-NA for key metrics
  dt <- dt[!is.na(OGI) & !is.na(OGMn) & !is.na(OGI_2) & !is.na(OGMn_2)]
  if (nrow(dt) == 0) return(NULL)

  # Overlap: intervals [OGI, OGMn] and [OGI_2, OGMn_2] intersect
  dt[, overlap := (OGI_2 < OGMn) & (OGI < OGMn_2)]

  list(
    n_pixels = nrow(dt),
    n_overlap = sum(dt$overlap, na.rm = TRUE),
    pct_overlap = 100 * mean(dt$overlap, na.rm = TRUE),
    parcel_mean_OGI1 = mean(dt$OGI, na.rm = TRUE),
    parcel_mean_OGMn1 = mean(dt$OGMn, na.rm = TRUE),
    parcel_mean_OGI2 = mean(dt$OGI_2, na.rm = TRUE),
    parcel_mean_OGMn2 = mean(dt$OGMn_2, na.rm = TRUE),
    parcel_mean_overlap = mean(dt$OGMn, na.rm = TRUE) > mean(dt$OGI_2, na.rm = TRUE)
  )
}

analyses <- list()
for (k in names(all_results)) {
  a <- overlap_analysis(all_results[[k]])
  if (!is.null(a)) {
    a$parcel_year <- k
    analyses[[k]] <- a
  }
}

if (length(analyses) > 0) {
  adf <- rbindlist(lapply(names(analyses), function(k) {
    x <- analyses[[k]]
    x$parcel_year <- k
    as.data.table(x)
  }))
  fwrite(adf, file.path(out_dir, "overlap_analysis.csv"))
  message("\n--- Overlap analysis ---")
  print(adf)
}
