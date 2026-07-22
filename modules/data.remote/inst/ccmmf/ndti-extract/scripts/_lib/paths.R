# Path helpers for ndti-extract. Override with environment variables.

ndti_extract_root <- function() {
  normalizePath(Sys.getenv("NDTI_EXTRACT_ROOT"), mustWork = FALSE)
}

hls_shared_lib_dir <- function() {
  env <- trimws(Sys.getenv("HLS_SHARED_LIB", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # PEcAn / lab layout: sibling hls/_lib next to ndti-extract
  root <- trimws(Sys.getenv("NDTI_EXTRACT_ROOT", ""))
  if (nzchar(root)) {
    sib <- file.path(dirname(root), "hls", "_lib")
    if (dir.exists(sib)) {
      return(normalizePath(sib, mustWork = FALSE))
    }
  }
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (nzchar(ccmmf)) {
    cand <- file.path(ccmmf, "management", "scripts", "hls", "_lib")
    if (dir.exists(cand)) {
      return(normalizePath(cand, mustWork = FALSE))
    }
  }
  stop(
    "HLS shared library not found. Set HLS_SHARED_LIB to .../hls/_lib ",
    "(sibling of ndti-extract under inst/ccmmf)."
  )
}

path_ndti_out_root <- function() {
  mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", "/projectnb/dietzelab/ccmmf"))
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "tillage", "ndti_v4.1")
}

path_parcel_tilemap <- function() {
  env <- trimws(Sys.getenv("NDTI_PARCEL_TILEMAP", ""))
  if (nzchar(env)) {
    return(env)
  }
  mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", "/projectnb/dietzelab/ccmmf"))
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "hls_parcel_tile_map_v4.1.rds")
}
