# -----------------------------------------------------------------------------
# Build HLS tile extent (one-time, shared by all HLS workflows)
# -----------------------------------------------------------------------------
#
# Creates polygon boundaries for each HLS tile in the same CRS as the parcels
# gpkg so other scripts can assign parcels to tiles without reprojecting.
#
# Imagery: phenology layout under HLS_IMAGERY_ROOT
#   <root>/<tile>/images/<scene>/*.tif
#
# Output: management/hls_tile_extent.rds (list: tile_extent_sf, used_crs)
# Usage:  Rscript build_hls_tile_extent.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(stringr)
})
sf::sf_use_s2(FALSE)

path_inventory   <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(path_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_inventory <- file.path(.root, "products", "inventory")
}
path_landiq_v4 <- trimws(Sys.getenv("LANDIQ_HARMONIZED", ""))
if (!nzchar(path_landiq_v4)) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set LANDIQ_HARMONIZED or CCMMF_ROOT.")
  path_landiq_v4 <- file.path(.root, "LandIQ", "harmonized")
}
path_parcels_gpkg <- {
  gpkg <- trimws(Sys.getenv("LANDIQ_GPKG", ""))
  if (nzchar(gpkg)) gpkg else file.path(path_landiq_v4, "parcels-consolidated.gpkg")
}
imagery_root <- trimws(Sys.getenv("HLS_IMAGERY_ROOT", ""))
if (!nzchar(imagery_root)) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set HLS_IMAGERY_ROOT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  imagery_root <- file.path(.root, "HLS", "imagery")
}
if (!dir.exists(imagery_root)) {
  stop("HLS imagery root not found: ", imagery_root)
}

path_hls_tile_extent <- file.path(path_inventory, "hls_tile_extent.rds")

# --- CRS: use parcels so tile extent matches LandIQ / NDTI ---
parcels_layer  <- st_layers(path_parcels_gpkg)$name[1]
parcels_sample <- st_read(
  path_parcels_gpkg,
  query = paste0("SELECT * FROM \"", parcels_layer, "\" LIMIT 1"),
  quiet = TRUE
)
used_crs <- st_crs(parcels_sample)

# One sample raster path per tile from phenology layout
# (<root>/<tile>/images/<scene>/*B06*.tif or *B11*.tif).
tile_dirs <- list.dirs(imagery_root, recursive = FALSE, full.names = TRUE)
tile_dirs <- tile_dirs[grepl("^[0-9]", basename(tile_dirs))]
if (length(tile_dirs) == 0L) {
  stop("No tile directories under HLS_IMAGERY_ROOT: ", imagery_root)
}

sample_path_for_tile <- function(tile_dir) {
  img_dir <- file.path(tile_dir, "images")
  if (!dir.exists(img_dir)) {
    return(NA_character_)
  }
  for (sc in list.dirs(img_dir, recursive = FALSE, full.names = TRUE)) {
    hits <- list.files(
      sc,
      pattern = "\\.(B06|B11)\\.tif$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(hits) > 0L) {
      return(hits[[1L]])
    }
  }
  NA_character_
}

tile_ids <- basename(tile_dirs)
paths_one_per_tile <- vapply(tile_dirs, sample_path_for_tile, character(1))
ok <- !is.na(paths_one_per_tile)
if (!any(ok)) {
  stop("No B06/B11 rasters found under tiles in: ", imagery_root)
}
tile_ids <- tile_ids[ok]
paths_one_per_tile <- paths_one_per_tile[ok]
message(
  "[hls_tile_extent] ", length(tile_ids), " tiles with sample rasters under ",
  imagery_root
)

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
dir.create(path_inventory, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(tile_extent_sf = tile_extent_sf, used_crs = used_crs), path_hls_tile_extent)
message("[hls_tile_extent] wrote ", path_hls_tile_extent)
