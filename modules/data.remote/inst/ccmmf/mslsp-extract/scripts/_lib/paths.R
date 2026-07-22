# Path helpers for mslsp-extract. Override with environment variables.

mslsp_extract_root <- function() {
  normalizePath(Sys.getenv("MSLSP_EXTRACT_ROOT"), mustWork = FALSE)
}

hls_shared_lib_dir <- function() {
  env <- trimws(Sys.getenv("HLS_SHARED_LIB", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # PEcAn / lab layout: sibling hls/_lib next to mslsp-extract
  root <- trimws(Sys.getenv("MSLSP_EXTRACT_ROOT", ""))
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
    "(sibling of mslsp-extract under inst/ccmmf)."
  )
}

path_mslsp_out_root <- function() {
  mgmt <- trimws(Sys.getenv("CCMMF_MANAGEMENT", ""))
  if (!nzchar(mgmt)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", "/projectnb/dietzelab/ccmmf"))
    mgmt <- file.path(ccmmf, "management")
  }
  file.path(mgmt, "phenology", "raw_mslsp_v4.1.2")
}

path_parcel_tilemap <- function() {
  env <- trimws(Sys.getenv("mslsp_parcel_tilemap", ""))
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

# Canonical MGRS tile list from HLS_Phenology (109 CA tiles).
# https://github.com/mrinareddy/HLS_Phenology/blob/main/tileids.txt
path_mslsp_tile_list <- function() {
  env <- trimws(Sys.getenv("MSLSP_TILE_LIST", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", "/projectnb/dietzelab/ccmmf"))
  file.path(ccmmf, "data_phen", "tileLists", "tileids.txt")
}
