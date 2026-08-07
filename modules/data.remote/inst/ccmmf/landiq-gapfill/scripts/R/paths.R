# Path helpers for the landiq-gapfill package.
# Defaults assume a standard CCMMF layout; override with environment variables.

landiq_gapfill_root <- function() {
  normalizePath(Sys.getenv("LANDIQ_GAPFILL_ROOT"), mustWork = FALSE)
}

path_data <- function() {
  file.path(landiq_gapfill_root(), "data")
}

path_outputs <- function() {
  file.path(landiq_gapfill_root(), "outputs")
}

path_cdl_fractions <- function() {
  env <- Sys.getenv("CDL_OUT_DIR", "")
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  file.path(landiq_gapfill_root(), "cdl")
}

path_cdl_rasters <- function() {
  env <- Sys.getenv("CDL_DIR", "")
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(ccmmf)) {
    stop("Set CDL_DIR or CCMMF_ROOT for CDL GeoTIFF location.")
  }
  file.path(ccmmf, "CDL")
}

path_landiq_root <- function() {
  # Harmonized (pre-gap-fill) product -- gap-fill input only.
  root <- trimws(Sys.getenv("LANDIQ_HARMONIZED", ""))
  if (!nzchar(root)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set LANDIQ_HARMONIZED or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    root <- file.path(ccmmf, "LandIQ", "harmonized")
  }
  root
}
path_landiq_parquet <- function() {
  file.path(path_landiq_root(), "crops_all_years.parq")
}

path_parcels_gpkg <- function() {
  file.path(path_landiq_root(), "parcels-consolidated.gpkg")
}

.consolidated_parcel_ids_cache <- new.env(parent = emptyenv())

#' Parcel IDs in parcels-consolidated.gpkg (CDL/HLS raster extraction subset).
load_consolidated_parcel_ids <- function(path_gpkg = path_parcels_gpkg()) {
  if (exists("ids", envir = .consolidated_parcel_ids_cache, inherits = FALSE)) {
    return(get("ids", envir = .consolidated_parcel_ids_cache))
  }
  if (!file.exists(path_gpkg)) {
    stop("Missing consolidated parcel geometry: ", path_gpkg)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package sf is required to read consolidated parcel_ids from ", path_gpkg)
  }
  layer <- sf::st_layers(path_gpkg)$name[1]
  raw <- sf::st_read(
    path_gpkg,
    layer = layer,
    query = sprintf('SELECT parcel_id FROM "%s"', layer),
    quiet = TRUE
  )
  ids <- unique(trimws(as.character(raw$parcel_id)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  assign("ids", ids, envir = .consolidated_parcel_ids_cache)
  ids
}

#' Keep only rows whose parcel_id appears in parcels-consolidated.gpkg.
filter_consolidated_parcels <- function(df, parcel_col = "parcel_id") {
  if (!parcel_col %in% names(df)) {
    stop("filter_consolidated_parcels requires column: ", parcel_col)
  }
  ids <- load_consolidated_parcel_ids()
  df %>%
    dplyr::mutate(!!parcel_col := trimws(as.character(.data[[parcel_col]]))) %>%
    dplyr::filter(.data[[parcel_col]] %in% ids)
}

path_crop_lookup_csv <- function() {
  file.path(path_data(), "LandIQ_cropCode_lookup_table.csv")
}

path_cdl_nass_lookup_csv <- function() {
  env <- Sys.getenv("CDL_NASS_CODE_LOOKUP_CSV", "")
  if (nzchar(env)) {
    return(env)
  }
  file.path(path_data(), "cdl_nass_cropland_code_lookup.csv")
}

path_transition_matrix <- function() {
  env <- Sys.getenv("EXTERNAL_TRANSITION_MATRIX_CSV", "")
  if (nzchar(env)) {
    return(env)
  }
  file.path(path_data(), "state_transition_matrix.csv")
}

path_county_transition_dir <- function() {
  env <- Sys.getenv("COUNTY_TRANSITION_MATRICES_DIR", "")
  if (nzchar(env)) {
    return(env)
  }
  # Prefer COUNTY_TRANSITION_MATRICES_DIR; else package data/ (see setup_env.sh).
  local <- file.path(path_data(), "county_transition_matrices")
  if (dir.exists(local)) {
    return(normalizePath(local, mustWork = FALSE))
  }
  stop(
    "Set COUNTY_TRANSITION_MATRICES_DIR to county CLASS transition matrices, ",
    "or place them under ", local, " (source documentation/setup_env.sh)."
  )
}
