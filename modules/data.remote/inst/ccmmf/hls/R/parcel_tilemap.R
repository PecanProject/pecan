# Geometry-only parcel <-> HLS tile map (built once).
# Year agricultural filtering happens in MSLSP / NDTI extract and demo match.

hls_root_dir <- function() {
  hls <- trimws(Sys.getenv("HLS_ROOT", ""))
  if (nzchar(hls)) return(hls)
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(ccmmf)) stop("Set HLS_ROOT or CCMMF_ROOT.")
  file.path(ccmmf, "HLS")
}

hls_parcel_tiles_dir <- function() {
  env <- trimws(Sys.getenv("HLS_PARCEL_TILES_DIR", ""))
  if (nzchar(env)) return(env)
  hls_root_dir()
}

path_parcel_tiles_csv <- function() {
  env <- trimws(Sys.getenv("HLS_PARCEL_TILEMAP", ""))
  if (nzchar(env)) return(env)
  file.path(hls_parcel_tiles_dir(), "parcel_tiles.csv")
}

path_cropcode_csv <- function() {
  env <- trimws(Sys.getenv("LANDIQ_CROPCODE_CSV", ""))
  if (nzchar(env) && file.exists(env)) return(env)
  gf <- trimws(Sys.getenv("LANDIQ_GAPFILL_ROOT", ""))
  if (nzchar(gf)) {
    p <- file.path(gf, "data", "LandIQ_cropCode_lookup_table.csv")
    if (file.exists(p)) return(p)
  }
  code <- trimws(Sys.getenv("CCMMF_CODE", ""))
  if (nzchar(code)) {
    p <- file.path(code, "landiq-gapfill", "data", "LandIQ_cropCode_lookup_table.csv")
    if (file.exists(p)) return(p)
  }
  stop("LandIQ_cropCode_lookup_table.csv not found. Set LANDIQ_CROPCODE_CSV, LANDIQ_GAPFILL_ROOT, or CCMMF_CODE.")
}

path_crops_parq <- function() {
  landiq <- trimws(Sys.getenv("LANDIQ_GAPFILLED", ""))
  if (!nzchar(landiq)) {
    root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(root)) stop("Set LANDIQ_GAPFILLED or CCMMF_ROOT.")
    landiq <- file.path(root, "LandIQ", "gapfilled")
  }
  file.path(landiq, "crops_all_years.parq")
}

read_parcel_tiles <- function(path = path_parcel_tiles_csv()) {
  if (!file.exists(path)) {
    stop(
      "Parcel-tile map not found: ", path, "\n",
      "Build it once from the harmonized gpkg:\n",
      "  Rscript hls/build_hls_parcel_tile_map.R overwrite"
    )
  }
  dt <- data.table::fread(path)
  if (!all(c("parcel_id", "tile_id") %in% names(dt))) {
    stop("Unexpected tile map columns at ", path, "; need parcel_id, tile_id")
  }
  dt[, parcel_id := as.character(parcel_id)]
  dt[, tile_id := as.character(tile_id)]
  dt
}

# Named list tile_id -> parcel_ids (tilewise extracts).
ag_tiles_to_tile_parcel_list <- function(ag_tiles_long) {
  if (nrow(ag_tiles_long) == 0L) return(stats::setNames(list(), character()))
  split(as.character(ag_tiles_long$parcel_id), as.character(ag_tiles_long$tile_id))
}

ag_parcel_ids_for_year <- function(year) {
  yr <- as.integer(year)
  lookup <- data.table::fread(path_cropcode_csv())
  ag_pairs <- unique(lookup[lookup$is_agricultural == TRUE,
    .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS))])
  pq <- data.table::as.data.table(arrow::read_parquet(
    path_crops_parq(),
    col_select = c("parcel_id", "CLASS", "SUBCLASS", "year")
  ))
  pq <- pq[as.integer(year) == yr & CLASS %in% unique(ag_pairs$CLASS),
           .(parcel_id, CLASS = trimws(as.character(CLASS)), SUBCLASS = as.character(SUBCLASS))]
  pq <- merge(pq, ag_pairs, by = c("CLASS", "SUBCLASS"))
  unique(as.character(pq$parcel_id))
}

# Geometry map filtered to agricultural parcels for YEAR (extract / demo match).
# Optional tile keeps that MGRS id only (training / one-tile extract).
load_parcel_tiles_for_year <- function(year, tile = NULL) {
  yr <- as.integer(year)
  map <- read_parcel_tiles()
  ag_ids <- ag_parcel_ids_for_year(yr)
  if (length(ag_ids) == 0L) {
    stop("No agricultural parcel_ids for year ", yr, " in ", path_crops_parq())
  }
  out <- map[parcel_id %in% ag_ids]
  n_ag <- length(ag_ids)
  n_hit <- data.table::uniqueN(out$parcel_id)
  miss_frac <- 1 - n_hit / n_ag
  if (n_hit == 0L || miss_frac > 0.5) {
    stop(
      "Year ", yr, " ag parcels do not match the geometry tile map at ",
      path_parcel_tiles_csv(), " (in map: ", n_hit, "/", n_ag, "). ",
      "Rebuild from the current parcels-consolidated.gpkg:\n",
      "  Rscript $CCMMF_CODE/hls/build_hls_parcel_tile_map.R overwrite"
    )
  }
  keep_tile <- if (is.null(tile) || length(tile) == 0L) "" else trimws(as.character(tile)[1L])
  if (is.na(keep_tile)) keep_tile <- ""
  if (nzchar(keep_tile)) {
    out <- out[out$tile_id == keep_tile]
    if (nrow(out) == 0L) {
      stop(
        "No year ", yr, " ag parcels with tile_id=", keep_tile,
        " in ", path_parcel_tiles_csv()
      )
    }
  }
  message(
    "[parcel tiles] year=", yr,
    if (nzchar(keep_tile)) paste0(" tile=", keep_tile) else "",
    " ag parcels=", data.table::uniqueN(out$parcel_id),
    if (!nzchar(keep_tile)) paste0("/", n_ag) else "",
    " tiles=", data.table::uniqueN(out$tile_id)
  )
  out
}
