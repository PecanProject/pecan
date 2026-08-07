# -----------------------------------------------------------------------------
# Build geometry-only parcel -> HLS tile map (one-time)
# -----------------------------------------------------------------------------
#
# Harmonized LandIQ parcel polygons are stable; tile overlap depends on geometry
# only. Which parcels are agricultural varies by year and is applied later in
# phenology/extract / tillage/extract prep (filter crops_all_years.parq).
#
# Run build_hls_tile_extent.R first.
#
# Outputs (in PRODUCTS_INVENTORY):
#   hls_parcel_tile_map_v4.1.csv         parcel_id, tileIDs, n_tiles
#   hls_tile_parcel_counts_v4.1.csv      tile_id, n_parcels (static geometry counts)
#   hls_parcel_tile_map_removed_v4.1.csv dropped invalid geometries (if any)
#
# Tile -> parcels is derived from the CSV via read_tile_to_parcels().
#
# Usage: Rscript build_hls_parcel_tile_map.R [overwrite]
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})
sf::sf_use_s2(FALSE)

script_dir <- if (length(fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))) {
  dirname(sub("^--file=", "", fa[1L]))
} else "."
source(file.path(script_dir, "R", "parcel_tilemap.R"))

path_landiq_v4  <- Sys.getenv("LANDIQ_HARMONIZED", "")
if (!nzchar(trimws(path_landiq_v4))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set LANDIQ_HARMONIZED or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_landiq_v4 <- file.path(.root, "LandIQ", "harmonized")
}
path_inventory <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(path_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_inventory <- file.path(.root, "products", "inventory")
}
path_parcels    <- file.path(path_landiq_v4, "parcels-consolidated.gpkg")
path_tiles      <- file.path(path_inventory, "hls_tile_extent.rds")
path_out        <- path_inventory

args <- commandArgs(trailingOnly = TRUE)
overwrite <- length(args) >= 1L && tolower(args[1L]) %in% c("overwrite", "true", "t", "1", "yes", "y")

out_parcel_file <- file.path(path_out, "hls_parcel_tile_map_v4.1.csv")
out_counts_file <- file.path(path_out, "hls_tile_parcel_counts_v4.1.csv")
out_removed_file <- file.path(path_out, "hls_parcel_tile_map_removed_v4.1.csv")

if (file.exists(out_parcel_file) && !overwrite) {
  message("Parcel-tile map exists (use overwrite to rebuild): ", out_parcel_file)
  quit(save = "no", status = 0)
}
if (!file.exists(path_tiles)) {
  stop("Tile extent not found. Run: Rscript scripts/hls/build_hls_tile_extent.R")
}
if (!file.exists(path_parcels)) {
  stop("Parcels GPKG not found: ", path_parcels)
}

tile_prep   <- readRDS(path_tiles)
tile_extent <- tile_prep$tile_extent_sf
used_crs    <- tile_prep$used_crs

layer <- sf::st_layers(path_parcels)$name[1L]
id_tbl <- sf::st_read(
  path_parcels,
  query = sprintf('SELECT parcel_id FROM "%s"', layer),
  quiet = TRUE
)
ids <- unique(as.character(id_tbl$parcel_id))
message("Parcels in harmonized GPKG: ", length(ids))

chunks <- split(ids, ceiling(seq_along(ids) / 5000L))
geom_chunks <- lapply(chunks, function(x) {
  esc <- gsub("'", "''", x, fixed = TRUE)
  q   <- sprintf(
    'SELECT * FROM "%s" WHERE parcel_id IN (%s)',
    layer, paste0("'", esc, "'", collapse = ",")
  )
  sf::st_read(path_parcels, query = q, quiet = TRUE)
})
parcels <- do.call(rbind, geom_chunks)
parcels$parcel_id <- as.character(parcels$parcel_id)

removed_log <- data.table(parcel_id = character())

valid <- tryCatch(
  !sf::st_is_empty(sf::st_geometry(parcels)),
  error = function(e) {
    message("Bulk geometry check failed; checking row-by-row for corrupt geometries.")
    vapply(seq_len(nrow(parcels)), function(i) {
      tryCatch(!sf::st_is_empty(sf::st_geometry(parcels)[i]), error = function(e) FALSE)
    }, logical(1))
  }
)
if (any(!valid)) {
  removed_log <- rbind(removed_log, data.table(parcel_id = parcels$parcel_id[!valid]))
  parcels <- parcels[valid, ]
}

parcels_tr <- tryCatch(sf::st_transform(parcels, used_crs), error = function(e) NULL)
if (is.null(parcels_tr)) {
  message("Bulk st_transform failed; checking row-by-row.")
  chunk_size <- 5000L
  n <- nrow(parcels)
  good <- logical(n)
  for (start in seq(1L, n, by = chunk_size)) {
    end <- min(start + chunk_size - 1L, n)
    chk <- tryCatch(sf::st_transform(parcels[start:end, ], used_crs), error = function(e) NULL)
    if (!is.null(chk)) {
      good[start:end] <- TRUE
    } else {
      for (i in start:end) {
        good[i] <- tryCatch({
          sf::st_transform(parcels[i, ], used_crs)
          TRUE
        }, error = function(e) FALSE)
      }
    }
  }
  if (any(!good)) {
    removed_log <- rbind(removed_log, data.table(parcel_id = parcels$parcel_id[!good]))
  }
  parcels <- parcels[good, ]
  parcels <- sf::st_transform(parcels, used_crs)
} else {
  parcels <- parcels_tr
}

hits <- tryCatch(sf::st_intersects(parcels, tile_extent), error = function(e) NULL)
if (is.null(hits)) {
  message("Bulk st_intersects failed; checking row-by-row.")
  n <- nrow(parcels)
  good <- logical(n)
  for (i in seq_len(n)) {
    good[i] <- tryCatch({
      hi <- sf::st_intersects(parcels[i, ], tile_extent)
      length(hi[[1L]]) >= 0L
      TRUE
    }, error = function(e) FALSE)
  }
  if (any(!good)) {
    removed_log <- rbind(removed_log, data.table(parcel_id = parcels$parcel_id[!good]))
  }
  parcels <- parcels[good, ]
  hits <- sf::st_intersects(parcels, tile_extent)
}

keep <- lengths(hits) > 0L
if (any(!keep)) {
  removed_log <- rbind(removed_log, data.table(parcel_id = parcels$parcel_id[!keep]))
}
parcels <- parcels[keep, ]
hits <- hits[keep]

if (nrow(removed_log) > 0L) {
  removed_log <- unique(removed_log)
  dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
  fwrite(removed_log, out_removed_file)
  message("Dropped ", nrow(removed_log), " parcels with invalid/no-tile geometry; log: ", out_removed_file)
}

parcel_tilemap <- data.table(
  parcel_id = parcels$parcel_id,
  tileIDs   = vapply(hits, function(i) paste(tile_extent$tile_id[i], collapse = ","), character(1)),
  n_tiles   = lengths(hits)
)
setorder(parcel_tilemap, parcel_id)

tile_to_parcels <- parcel_tilemap_to_tile_list(parcel_tilemap)
tile_counts <- data.table(
  tile_id = names(tile_to_parcels),
  n_parcels = vapply(tile_to_parcels, length, integer(1))
)
setorder(tile_counts, tile_id)

dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
fwrite(parcel_tilemap, out_parcel_file)
fwrite(tile_counts, out_counts_file)

message("Wrote parcel->tiles: ", out_parcel_file, " (", nrow(parcel_tilemap), " parcels)")
message("Wrote tile counts:   ", out_counts_file, " (", nrow(tile_counts), " tiles)")
