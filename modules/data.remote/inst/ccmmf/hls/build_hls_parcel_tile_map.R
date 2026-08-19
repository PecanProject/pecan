#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# One-time prep: harmonized LandIQ parcels x HLS tiles (CSV).
# -----------------------------------------------------------------------------
#
# Parcel polygons are stable after Session 1 harmonize. Tile overlap is
# geometry only. Which parcels are agricultural varies by year and is applied
# later in MSLSP / NDTI extract (filter $LANDIQ_GAPFILLED crops_all_years.parq).
#
# 1) Static MGRS grid ($HLS_S2_MGRS_GRID) x tileids.txt ($MSLSP_TILE_LIST)
#    -> reproject to LandIQ CRS
# 2) All parcels in parcels-consolidated.gpkg -> intersect tiles -> CSV
#
# Prerequisites (flat under $HLS_ROOT unless overridden):
#   - $HLS_ROOT/s2_mgrs_grid_ca.gpkg (or $HLS_S2_MGRS_GRID)
#   - HLS_Phenology tileids.txt (or $MSLSP_TILE_LIST)
#   - $LANDIQ_HARMONIZED/parcels-consolidated.gpkg
#
# Output: $HLS_ROOT/parcel_tiles.csv  (parcel_id, tile_id)
#   (override path with HLS_PARCEL_TILEMAP, or dir with HLS_PARCEL_TILES_DIR)
#
# Usage:
#   Rscript build_hls_parcel_tile_map.R
#   Rscript build_hls_parcel_tile_map.R overwrite
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})
sf::sf_use_s2(FALSE)

script_dir <- if (length(fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))) {
  dirname(sub("^--file=", "", fa[1L]))
} else "."
source(file.path(script_dir, "R", "parcel_tilemap.R"))

path_s2_mgrs_grid <- function() {
  env <- trimws(Sys.getenv("HLS_S2_MGRS_GRID", ""))
  if (nzchar(env)) return(env)
  p <- file.path(hls_root_dir(), "s2_mgrs_grid_ca.gpkg")
  if (file.exists(p)) return(p)
  stop("Missing $HLS_ROOT/s2_mgrs_grid_ca.gpkg (set HLS_S2_MGRS_GRID).")
}

path_hls_tile_list <- function() {
  for (env_name in c("MSLSP_TILE_LIST", "HLS_TILE_LIST")) {
    env <- trimws(Sys.getenv(env_name, ""))
    if (nzchar(env)) return(normalizePath(env, mustWork = FALSE))
  }
  phen <- trimws(Sys.getenv("HLS_PHENOLOGY_ROOT", ""))
  if (nzchar(phen)) {
    p <- file.path(phen, "tileids.txt")
    if (file.exists(p)) return(normalizePath(p, mustWork = FALSE))
  }
  p <- file.path(hls_root_dir(), "tileids.txt")
  if (file.exists(p)) return(normalizePath(p, mustWork = FALSE))
  stop("tileids.txt not found (set MSLSP_TILE_LIST or clone HLS_Phenology).")
}

read_hls_tile_list <- function(path = path_hls_tile_list()) {
  tiles <- trimws(readLines(path, warn = FALSE))
  tiles[nzchar(tiles)]
}

path_landiq_parcels_gpkg <- function() {
  env <- trimws(Sys.getenv("LANDIQ_GPKG", ""))
  if (nzchar(env)) return(env)
  harm <- trimws(Sys.getenv("LANDIQ_HARMONIZED", ""))
  if (!nzchar(harm)) {
    root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(root)) stop("Set LANDIQ_GPKG, LANDIQ_HARMONIZED, or CCMMF_ROOT.")
    harm <- file.path(root, "LandIQ", "work", "03-final")
  }
  file.path(harm, "parcels-consolidated.gpkg")
}

load_tile_extents_landiq <- function() {
  canonical <- read_hls_tile_list()
  g <- st_read(path_s2_mgrs_grid(), quiet = TRUE)
  if (!"tile_id" %in% names(g) && "Name" %in% names(g)) {
    g$tile_id <- as.character(g$Name)
  }
  g$tile_id <- as.character(g$tile_id)
  g <- g[g$tile_id %in% canonical, "tile_id", drop = FALSE]
  missing <- setdiff(canonical, g$tile_id)
  if (length(missing) > 0L) {
    stop("MGRS grid missing tiles: ", paste(utils::head(missing, 10L), collapse = ", "))
  }
  g <- g[match(canonical, g$tile_id), , drop = FALSE]
  g <- st_zm(g, drop = TRUE)

  gpkg <- path_landiq_parcels_gpkg()
  if (!file.exists(gpkg)) stop("Parcels GPKG not found: ", gpkg)
  layer <- st_layers(gpkg)$name[1L]
  sample <- st_read(
    gpkg,
    query = sprintf('SELECT * FROM "%s" LIMIT 1', layer),
    quiet = TRUE
  )
  used_crs <- st_crs(sample)
  list(tile_extent_sf = st_transform(g, used_crs), used_crs = used_crs)
}

build_parcel_tiles <- function(chunk_size = 5000L) {
  extent <- load_tile_extents_landiq()
  tile_extent <- extent$tile_extent_sf
  used_crs <- extent$used_crs
  message("[hls prep] ", nrow(tile_extent), " tiles in LandIQ CRS")

  gpkg <- path_landiq_parcels_gpkg()
  layer <- st_layers(gpkg)$name[1L]
  id_tbl <- st_read(
    gpkg,
    query = sprintf('SELECT parcel_id FROM "%s"', layer),
    quiet = TRUE
  )
  ids <- unique(as.character(id_tbl$parcel_id))
  if (length(ids) == 0L) stop("No parcel_ids in ", gpkg)
  message("[hls prep] parcels in harmonized gpkg: ", length(ids))

  chunks <- split(ids, ceiling(seq_along(ids) / as.integer(chunk_size)))
  rows <- list()
  for (i in seq_along(chunks)) {
    chunk_ids <- chunks[[i]]
    esc <- gsub("'", "''", chunk_ids, fixed = TRUE)
    q <- sprintf(
      'SELECT * FROM "%s" WHERE parcel_id IN (%s)',
      layer, paste0("'", esc, "'", collapse = ",")
    )
    parcels <- st_read(gpkg, query = q, quiet = TRUE)
    if (nrow(parcels) == 0L) next
    parcels$parcel_id <- as.character(parcels$parcel_id)
    keep <- tryCatch(
      !st_is_empty(st_geometry(parcels)),
      error = function(e) rep(TRUE, nrow(parcels))
    )
    parcels <- parcels[keep, , drop = FALSE]
    if (nrow(parcels) == 0L) next
    parcels <- st_zm(parcels, drop = TRUE)
    parcels <- st_transform(parcels, used_crs)
    hits <- st_intersects(parcels, tile_extent)
    for (j in seq_len(nrow(parcels))) {
      ix <- hits[[j]]
      if (length(ix) == 0L) next
      rows[[length(rows) + 1L]] <- data.table(
        parcel_id = parcels$parcel_id[j],
        tile_id = as.character(tile_extent$tile_id[ix])
      )
    }
    if (i %% 10L == 0L || i == length(chunks)) {
      message("[hls prep] geometry chunk ", i, "/", length(chunks))
    }
  }
  if (length(rows) == 0L) stop("No parcel x tile rows from ", gpkg)
  unique(rbindlist(rows), by = c("parcel_id", "tile_id"))
}

# --- CLI ---

args <- commandArgs(trailingOnly = TRUE)
overwrite <- FALSE
other <- character()
for (a in args) {
  al <- tolower(a)
  if (al %in% c("overwrite", "true", "t", "1", "yes", "y")) {
    overwrite <- TRUE
  } else {
    other <- c(other, a)
  }
}
if (length(other) > 0L) {
  stop(
    "Years are not used. This map is geometry-only (harmonized parcels x HLS tiles).\n",
    "Usage: Rscript build_hls_parcel_tile_map.R [overwrite]\n",
    "Agricultural fields are selected later in extract for each year."
  )
}

out_csv <- path_parcel_tiles_csv()
out_dir <- dirname(out_csv)
message("[hls prep] overwrite=", overwrite, " out=", out_csv)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(out_csv) && !overwrite) {
  message("[hls prep] exists (use overwrite to rebuild): ", out_csv)
  quit(save = "no", status = 0)
}

message("[hls prep] building parcel x tile table from harmonized gpkg")
out <- build_parcel_tiles()
fwrite(out, out_csv)
message(
  "[hls prep] wrote ", out_csv,
  " (", uniqueN(out$parcel_id), " parcels, ",
  uniqueN(out$tile_id), " tiles)"
)
message("[hls prep] done")
