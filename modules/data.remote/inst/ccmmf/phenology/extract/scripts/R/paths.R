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
  stop(
    "HLS shared library not found. Set HLS_SHARED_LIB to .../hls/R ",
    "(sibling of phenology/ under inst/ccmmf)."
  )
}

path_mslsp_out_root <- function() {
  env <- trimws(Sys.getenv("MSLSP_EXTRACT_ROOT", ""))
  if (nzchar(env)) return(normalizePath(env, mustWork = FALSE))
  nc <- trimws(Sys.getenv("MSLSP_NETCDF_ROOT", ""))
  if (nzchar(nc)) return(file.path(nc, "raw_mslsp_v4.1.2"))
  hls <- trimws(Sys.getenv("HLS_ROOT", ""))
  if (nzchar(hls)) return(file.path(hls, "MSLSP", "raw_mslsp_v4.1.2"))
  ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(ccmmf)) {
    stop("Set MSLSP_EXTRACT_ROOT, MSLSP_NETCDF_ROOT, HLS_ROOT, or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  file.path(ccmmf, "HLS", "MSLSP", "raw_mslsp_v4.1.2")
}

# Canonical MGRS tile list from HLS_Phenology (CA tiles).
# https://github.com/mrinareddy/HLS_Phenology/blob/main/tileids.txt
path_mslsp_tile_list <- function() {
  env <- trimws(Sys.getenv("MSLSP_TILE_LIST", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  phen <- trimws(Sys.getenv("HLS_PHENOLOGY_ROOT", ""))
  if (nzchar(phen)) {
    p <- file.path(phen, "tileids.txt")
    if (file.exists(p)) return(normalizePath(p, mustWork = FALSE))
  }
  hls <- trimws(Sys.getenv("HLS_ROOT", ""))
  if (!nzchar(hls)) {
    ccmmf <- trimws(Sys.getenv("CCMMF_ROOT", ""))
    if (!nzchar(ccmmf)) {
      stop("Set MSLSP_TILE_LIST, HLS_PHENOLOGY_ROOT, HLS_ROOT, or CCMMF_ROOT (source documentation/setup_env.sh).")
    }
    hls <- file.path(ccmmf, "HLS")
  }
  file.path(hls, "tileids.txt")
}
