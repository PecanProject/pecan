# Geometry-only parcel <-> HLS tile map helpers.
# Built once by build_hls_parcel_tile_map.R; year filtering happens at extract time.

parcel_tilemap_default_path <- function() {
  for (env_name in c("HLS_PARCEL_TILEMAP", "NDTI_PARCEL_TILEMAP", "mslsp_parcel_tilemap")) {
    env <- trimws(Sys.getenv(env_name, ""))
    if (nzchar(env)) {
      return(env)
    }
  }
  mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "hls_parcel_tile_map_v4.1.rds")
}

tile_to_parcels_default_path <- function() {
  env <- trimws(Sys.getenv("HLS_TILE_TO_PARCELS", ""))
  if (nzchar(env)) {
    return(env)
  }
  mgmt <- dirname(parcel_tilemap_default_path())
  file.path(mgmt, "hls_tile_to_parcels_v4.1.rds")
}

read_parcel_tilemap <- function(path = parcel_tilemap_default_path()) {
  if (!file.exists(path)) {
    stop("Parcel-tile map not found: ", path,
         "\nBuild it: Rscript scripts/hls/build_hls_parcel_tile_map.R overwrite")
  }
  dt <- data.table::as.data.table(readRDS(path))
  if ("year" %in% names(dt)) {
    stop(
      "Legacy year-keyed tile map at: ", path, "\n",
      "Rebuild the geometry-only map:\n",
      "  Rscript scripts/hls/build_hls_parcel_tile_map.R overwrite"
    )
  }
  if (!all(c("parcel_id", "tileIDs") %in% names(dt))) {
    stop("Unexpected tile map columns at ", path, "; need parcel_id, tileIDs")
  }
  dt[, parcel_id := as.character(parcel_id)]
  dt[, tileIDs := as.character(tileIDs)]
  if (!"n_tiles" %in% names(dt) && "tileIDs" %in% names(dt)) {
    dt[, n_tiles := lengths(strsplit(tileIDs, ",", fixed = TRUE))]
  }
  data.table::setkey(dt, parcel_id)
  dt
}

subset_parcel_tilemap <- function(dt, parcel_ids) {
  parcel_ids <- unique(as.character(parcel_ids))
  if (length(parcel_ids) == 0) {
    return(dt[0])
  }
  data.table::setkey(dt, parcel_id)
  dt[parcel_ids, nomatch = 0]
}

parcel_tilemap_to_tile_list <- function(dt) {
  long <- dt[, .(tile_id = unlist(strsplit(tileIDs, ",", fixed = TRUE))), by = parcel_id]
  long <- long[nzchar(tile_id)]
  if (nrow(long) == 0) {
    return(stats::setNames(list(), character()))
  }
  split(as.character(long$parcel_id), long$tile_id)
}

read_tile_to_parcels <- function(path = tile_to_parcels_default_path()) {
  if (!file.exists(path)) {
    stop("Tile-to-parcels map not found: ", path,
         "\nBuild it: Rscript scripts/hls/build_hls_parcel_tile_map.R overwrite")
  }
  obj <- readRDS(path)
  if (is.list(obj) && !data.table::is.data.table(obj)) {
    return(obj)
  }
  stop("Unexpected tile-to-parcels object at: ", path)
}

ag_parcel_ids_for_year <- function(year,
                                   crops_parq = NULL,
                                   cropcode_csv = NULL) {
  if (is.null(crops_parq) || !nzchar(trimws(crops_parq))) {
    landiq <- trimws(Sys.getenv("CCMMF_LANDIQ_V4", ""))
    if (!nzchar(landiq)) {
      root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
      if (!nzchar(root)) {
        stop("Set CCMMF_LANDIQ_V4 or CCMMF_ROOT.")
      }
      landiq <- file.path(root, "LandIQ-harmonized-v4.1")
    }
    crops_parq <- file.path(landiq, "crops_all_years.parq")
  }
  if (is.null(cropcode_csv) || !nzchar(trimws(cropcode_csv))) {
    mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
    if (!nzchar(mgmt)) {
      root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
      if (!nzchar(root)) {
        stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT.")
      }
      mgmt <- file.path(root, "management")
    }
    cropcode_csv <- file.path(mgmt, "LandIQ_cropCode_lookup_table.csv")
  }
  yr <- as.integer(year)
  lookup <- data.table::fread(cropcode_csv)
  ag_pairs <- unique(lookup[lookup$is_agricultural == TRUE,
    .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS))])
  ag_classes <- unique(ag_pairs$CLASS)

  pq <- arrow::read_parquet(
    crops_parq,
    col_select = c("parcel_id", "CLASS", "SUBCLASS", "year")
  )
  pq <- data.table::as.data.table(pq)
  pq <- pq[as.integer(year) == yr & CLASS %in% ag_classes,
           .(parcel_id, CLASS = trimws(as.character(CLASS)), SUBCLASS = as.character(SUBCLASS))]
  pq <- merge(pq, ag_pairs, by = c("CLASS", "SUBCLASS"))
  unique(as.character(pq$parcel_id))
}

load_parcel_tilemap_for_year <- function(year, path = parcel_tilemap_default_path()) {
  ag_ids <- ag_parcel_ids_for_year(year)
  subset_parcel_tilemap(read_parcel_tilemap(path), ag_ids)
}
