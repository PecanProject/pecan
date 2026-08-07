# -----------------------------------------------------------------------------
# Build HLS tile extent (one-time, shared by all HLS workflows)
# -----------------------------------------------------------------------------
#
# Creates polygon boundaries for each HLS tile in the same CRS as the parcels
# gpkg so other scripts can assign parcels to tiles without reprojecting.
#
# Output: management/hls_tile_extent.rds (list: tile_extent_sf, used_crs)
# Usage:  Rscript build_hls_tile_extent.R
#
# Paths can be overridden via environment variables (see Configuration).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(stringr)
})
# Disable s2 for projected CRS operations (default sf::sf_use_s2 is TRUE).
sf::sf_use_s2(FALSE)

# --- Configuration ---
# Override via env: CCMMF_MANAGEMENT, CCMMF_LANDIQ_V4, HLSL_BASE, HLSS_BASE
path_management   <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
path_landiq_v4 <- trimws(Sys.getenv("CCMMF_LANDIQ_V4", ""))
if (!nzchar(path_landiq_v4)) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set CCMMF_LANDIQ_V4 or CCMMF_ROOT.")
  path_landiq_v4 <- file.path(.root, "LandIQ-harmonized-v4.1")
}
path_parcels_gpkg <- {
  gpkg <- trimws(Sys.getenv("CCMMF_LANDIQ_GPKG", ""))
  if (nzchar(gpkg)) gpkg else file.path(path_landiq_v4, "parcels-consolidated.gpkg")
}
hlsl_base         <- Sys.getenv("HLSL_BASE", "")
hlss_base         <- Sys.getenv("HLSS_BASE", "")
if (!nzchar(trimws(hlsl_base)) || !nzchar(trimws(hlss_base))) {
  stop("Set HLSL_BASE and HLSS_BASE to HLS L30/S30 imagery roots (no lab default).")
}

path_hls_tile_extent <- file.path(path_management, "hls_tile_extent.rds")

# --- CRS: use parcels so tile extent matches LandIQ / NDTI ---
parcels_layer  <- st_layers(path_parcels_gpkg)$name[1]
parcels_sample <- st_read(
  path_parcels_gpkg,
  query = paste0("SELECT * FROM \"", parcels_layer, "\" LIMIT 1"),
  quiet = TRUE
)
used_crs <- st_crs(parcels_sample)

# --- Helpers: tile ID from basename; list year dirs under HLS roots ---
tile_id_from_basename <- function(basename) {
  matched <- str_extract(basename, "T[0-9A-Z]{5}\\.")
  sub("\\.+$", "", sub("^T|\\.$", "", matched))
}

list_year_dirs <- function(base_path) {
  if (!dir.exists(base_path)) return(integer())
  dirs <- basename(list.dirs(base_path, recursive = FALSE, full.names = TRUE))
  years <- suppressWarnings(as.integer(dirs))
  years[!is.na(years)]
}

# --- Collect one raster path per unique tile (HLSL B06 + HLSS B11) ---
years_available <- sort(unique(c(list_year_dirs(hlsl_base), list_year_dirs(hlss_base))))
if (length(years_available) == 0) stop("No year directories found in HLS roots.")

all_paths <- character()
for (year in years_available) {
  hlsl_dir  <- file.path(hlsl_base, year)
  hlss_dir  <- file.path(hlss_base, year)
  paths_year <- character()
  if (dir.exists(hlsl_dir)) {
    paths_year <- c(paths_year, list.files(hlsl_dir, pattern = ".*B06.*\\.tif$", full.names = TRUE))
  }
  if (dir.exists(hlss_dir)) {
    paths_year <- c(paths_year, list.files(hlss_dir, pattern = ".*B11.*\\.tif$", full.names = TRUE))
  }
  if (length(paths_year) > 0) {
    all_paths <- paths_year
    break
  }
}
if (length(all_paths) == 0) stop("No HLS files found in any available year directory.")

tile_ids          <- tile_id_from_basename(basename(all_paths))
tile_ids          <- sub("\\.+$", "", tile_ids)
keep_first_per_tile <- !duplicated(tile_ids)
paths_one_per_tile <- all_paths[keep_first_per_tile]
tile_ids          <- tile_ids[keep_first_per_tile]

# --- Raster extent to polygon per tile, reprojected to parcels CRS ---
extent_to_polygon_in_crs <- function(raster_path, target_crs) {
  r <- terra::rast(raster_path)
  e <- terra::ext(r)
  bbox <- c(
    xmin = unname(e[1]),
    ymin = unname(e[3]),
    xmax = unname(e[2]),
    ymax = unname(e[4])
  )
  raster_crs <- st_crs(terra::crs(r))
  box_sfc <- st_as_sfc(st_bbox(bbox, crs = raster_crs))
  st_transform(box_sfc, target_crs)
}

tile_polygons <- lapply(paths_one_per_tile, function(p) {
  extent_to_polygon_in_crs(p, used_crs)
})
tile_extent_sf <- st_sf(tile_id = tile_ids, geometry = do.call(c, tile_polygons))

# --- Write ---
dir.create(path_management, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(tile_extent_sf = tile_extent_sf, used_crs = used_crs), path_hls_tile_extent)
