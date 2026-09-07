#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Reorganize Earthdata HLS downloads into the CCMMF tile tree (no clone edits).
# -----------------------------------------------------------------------------
#
# Replaces HLS_Phenology/conversion.R. Do not edit that file.
#
# HLS scene files (bands + Fmask) in $HLS_DOWNLOAD_OUTDIR:
#   HLS.L30.T10TEK.2020005T185139.v2.0.B02.tif
# land under:
#   $HLS_IMAGERY_ROOT/<tile>/images/<sceneID>/
# Scene files are moved (same as the upstream satellite block). Already-present
# destinations are skipped. Set HLS_CONVERSION_OVERWRITE=true to replace.
#
# Optional ancillary (water / DEM / slope / aspect) is copied -- never moved --
# from per-type directories into $HLS_IMAGERY_ROOT/<tile>/images/:
#   HLS_WATER_DIR, HLS_DEM_DIR, HLS_SLOPE_DIR, HLS_ASPECT_DIR
# Expected names: water_10TEK.tif, dem_10TEK.tif, slope_10TEK.tif, aspect_10TEK.tif
#
# Optional: HLS_CONVERSION_TILE=10TEK to restrict scenes and ancillary to one tile.
# Submit statewide convert with $CCMMF_SUBMIT (Session 0); demo can run here.
#
# Usage:
#   Rscript "$CCMMF_CODE/hls/convert_hls_scenes.R"
#   "$CCMMF_SUBMIT" -n hls-convert -- Rscript "$CCMMF_CODE/hls/convert_hls_scenes.R"
# -----------------------------------------------------------------------------

env_first <- function(...) {
  for (nm in c(...)) {
    v <- trimws(Sys.getenv(nm, ""))
    if (nzchar(v)) return(v)
  }
  ""
}

truthy <- function(name, default = "false") {
  tolower(Sys.getenv(name, default)) %in% c("1", "true", "t", "yes", "y")
}

hls_root_dir <- function() {
  hls <- env_first("HLS_ROOT")
  if (nzchar(hls)) return(hls)
  ccmmf <- env_first("CCMMF_ROOT")
  if (!nzchar(ccmmf)) stop("Set HLS_ROOT or CCMMF_ROOT.")
  file.path(ccmmf, "HLS")
}

parse_hls_tif <- function(path) {
  fname <- basename(path)
  part <- unlist(strsplit(fname, ".", fixed = TRUE))
  if (length(part) < 8L || !identical(part[[1L]], "HLS")) {
    return(NULL)
  }
  tile_tok <- part[[3L]]
  tile_id <- if (startsWith(tile_tok, "T")) substring(tile_tok, 2L) else tile_tok
  list(
    tile_id = tile_id,
    scene_id = paste(part[seq_len(6L)], collapse = "."),
    band = part[[7L]],
    file = path
  )
}

safe_place <- function(src, dest, move, overwrite) {
  if (file.exists(dest) && !overwrite) return("skip")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && overwrite) unlink(dest)
  if (move) {
    ok <- file.rename(src, dest)
    if (isTRUE(ok) && file.exists(dest)) return("move")
    if (!file.copy(src, dest, overwrite = overwrite)) {
      stop("Failed to copy ", src, " -> ", dest)
    }
    unlink(src)
    return("move")
  }
  if (!file.copy(src, dest, overwrite = overwrite)) {
    stop("Failed to copy ", src, " -> ", dest)
  }
  "copy"
}

out_dir <- env_first("HLS_IMAGERY_ROOT")
if (!nzchar(out_dir)) out_dir <- file.path(hls_root_dir(), "imagery")
in_dir <- env_first("HLS_DOWNLOAD_OUTDIR")
if (!nzchar(in_dir)) in_dir <- file.path(out_dir, "download_scratch")

one_tile <- env_first("HLS_CONVERSION_TILE")
overwrite <- truthy("HLS_CONVERSION_OVERWRITE")
# Upstream conversion.R moves scene tifs out of the download dir.
move_scenes <- !truthy("HLS_CONVERSION_COPY")

water_dir <- env_first("HLS_WATER_DIR")
dem_dir <- env_first("HLS_DEM_DIR")
slope_dir <- env_first("HLS_SLOPE_DIR")
aspect_dir <- env_first("HLS_ASPECT_DIR")

if (!dir.exists(in_dir)) {
  stop("HLS download scratch not found: ", in_dir)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

img_list <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
n_img <- length(img_list)
n_moved <- 0L
n_copied <- 0L
n_skip <- 0L
n_bad <- 0L
n_tile_skip <- 0L

message(
  "[hls convert] scenes in_dir=", in_dir,
  " out_dir=", out_dir,
  " n_tif=", n_img,
  " move=", move_scenes,
  if (nzchar(one_tile)) paste0(" tile=", one_tile) else ""
)

for (i in seq_along(img_list)) {
  parsed <- parse_hls_tif(img_list[[i]])
  if (is.null(parsed)) {
    n_bad <- n_bad + 1L
    next
  }
  if (nzchar(one_tile) && !identical(parsed$tile_id, one_tile)) {
    n_tile_skip <- n_tile_skip + 1L
    next
  }
  dest <- file.path(out_dir, parsed$tile_id, "images", parsed$scene_id, basename(parsed$file))
  st <- safe_place(parsed$file, dest, move = move_scenes, overwrite = overwrite)
  if (identical(st, "skip")) {
    n_skip <- n_skip + 1L
  } else if (identical(st, "move")) {
    n_moved <- n_moved + 1L
  } else {
    n_copied <- n_copied + 1L
  }
  if (i %% 10000L == 0L || i == n_img) {
    message(sprintf("[hls convert] scenes processed: %d / %d", i, n_img))
  }
}

copy_ancillary_dir <- function(in_dir, kind) {
  if (!nzchar(in_dir)) return(invisible(NULL))
  if (!dir.exists(in_dir)) {
    warning("[hls convert] ", kind, " dir missing (skipped): ", in_dir)
    return(invisible(NULL))
  }
  files <- list.files(in_dir, pattern = paste0("^", kind, "_.+\\.tif$"),
                      full.names = TRUE, ignore.case = TRUE)
  n_ok <- 0L
  n_skip_loc <- 0L
  for (f in files) {
    fname <- basename(f)
    stem <- sub("\\.tif$", "", fname, ignore.case = TRUE)
    tile_id <- sub(paste0("^", kind, "_"), "", stem, ignore.case = TRUE)
    if (!nzchar(tile_id)) next
    if (nzchar(one_tile) && !identical(tile_id, one_tile)) next
    dest <- file.path(out_dir, tile_id, "images", fname)
    st <- safe_place(f, dest, move = FALSE, overwrite = overwrite)
    if (identical(st, "skip")) n_skip_loc <- n_skip_loc + 1L else n_ok <- n_ok + 1L
  }
  message(
    "[hls convert] ", kind, ": copied=", n_ok,
    " skipped=", n_skip_loc, " from ", in_dir
  )
}

copy_ancillary_dir(water_dir, "water")
copy_ancillary_dir(dem_dir, "dem")
copy_ancillary_dir(slope_dir, "slope")
copy_ancillary_dir(aspect_dir, "aspect")

if (!nzchar(water_dir) && !nzchar(dem_dir) && !nzchar(slope_dir) && !nzchar(aspect_dir)) {
  message(
    "[hls convert] no ancillary dirs set (HLS_WATER_DIR / HLS_DEM_DIR / ",
    "HLS_SLOPE_DIR / HLS_ASPECT_DIR). Scene tree is enough for NDTI; ",
    "MSLSP phenology still needs water / DEM / slope / aspect per tile."
  )
}

message(
  "[hls convert] done scenes: moved=", n_moved,
  " copied=", n_copied,
  " skipped_existing=", n_skip,
  " skipped_other_tile=", n_tile_skip,
  " unparsed=", n_bad
)
