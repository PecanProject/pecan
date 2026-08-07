# Path helpers for phenology/extract. Override with environment variables.

mslsp_extract_root <- function() {
  mslsp_extract_pkg_root()
}

hls_shared_lib_dir <- function() {
  env <- trimws(Sys.getenv("HLS_SHARED_LIB", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # inst/ccmmf/hls/R: sibling of phenology/ (extract is phenology/extract)
  pheno <- trimws(Sys.getenv("PHENOLOGY_ROOT", ""))
  if (nzchar(pheno)) {
    sib <- file.path(dirname(pheno), "hls", "R")
    if (dir.exists(sib)) {
      return(normalizePath(sib, mustWork = FALSE))
    }
  }
  root <- mslsp_extract_pkg_root()
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
    "(sibling of phenology/ under inst/ccmmf)."
  )
}

path_mslsp_out_root <- function() {
  mgmt <- trimws(Sys.getenv("MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "phenology", "raw_mslsp_v4.1.2")
}

path_parcel_tilemap <- function() {
  env <- trimws(Sys.getenv("HLS_PARCEL_TILEMAP", ""))
  if (nzchar(env)) {
    return(env)
  }
  mgmt <- trimws(Sys.getenv("MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "hls_parcel_tile_map_v4.1.csv")
}

# Canonical MGRS tile list from HLS_Phenology (109 CA tiles).
# https://github.com/mrinareddy/HLS_Phenology/blob/main/tileids.txt
path_mslsp_tile_list <- function() {
  env <- trimws(Sys.getenv("MSLSP_TILE_LIST", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(ccmmf)) {
    stop("Set MSLSP_TILE_LIST or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  file.path(ccmmf, "data_phen", "tileLists", "tileids.txt")
}
