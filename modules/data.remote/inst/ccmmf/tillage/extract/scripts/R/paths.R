# Path helpers for tillage/extract. Override with environment variables.

ndti_extract_root <- function() {
  ndti_extract_pkg_root()
}

hls_shared_lib_dir <- function() {
  env <- trimws(Sys.getenv("HLS_SHARED_LIB", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # inst/ccmmf/hls/R: sibling of tillage/ (extract is tillage/extract)
  tillage <- trimws(Sys.getenv("TILLAGE_ROOT", ""))
  if (nzchar(tillage)) {
    sib <- file.path(dirname(tillage), "hls", "R")
    if (dir.exists(sib)) {
      return(normalizePath(sib, mustWork = FALSE))
    }
  }
  root <- ndti_extract_pkg_root()
  sib <- file.path(dirname(dirname(root)), "hls", "R")
  if (dir.exists(sib)) {
    return(normalizePath(sib, mustWork = FALSE))
  }
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (nzchar(ccmmf)) {
    cand <- file.path(ccmmf, "management", "scripts", "hls", "R")
    if (dir.exists(cand)) {
      return(normalizePath(cand, mustWork = FALSE))
    }
  }
  stop(
    "HLS shared library not found. Set HLS_SHARED_LIB to .../hls/R ",
    "(sibling of tillage/ under inst/ccmmf)."
  )
}

path_ndti_out_root <- function() {
  mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
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
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "hls_parcel_tile_map_v4.1.rds")
}
