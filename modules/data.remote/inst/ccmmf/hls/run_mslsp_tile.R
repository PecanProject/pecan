#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Run MSLSP phenology for one HLS tile (no SCC / qsub).
# -----------------------------------------------------------------------------
#
# Replaces MSLSP_submitTiles_SCC.sh + MSLSP_runTile_SCC.sh for a single tile.
# Does not edit the MSLSP clone. Reads that clone's MSLSP_Parameters.json,
# overlays CCMMF paths / years, writes a per-tile JSON, then:
#   Rscript MSLSP_Script*.R TILE json runLog errorLog
# If the clone has the known imgName_strip / keep-subset bugs, a patched copy
# is written under the tile output dir and that copy is what gets run.
#
# This is the BU phenology algorithm (NetCDF writer). Parcel extract is
# phenology/run_mslsp.sh -- a different script.
#
# Usage:
#   Rscript "$CCMMF_CODE/hls/run_mslsp_tile.R" TILE
#   Rscript "$CCMMF_CODE/hls/run_mslsp_tile.R" 10TEK
#
# Env (after setup_env.sh):
#   MSLSP_ALGO_ROOT          clone (aliceni7/MSLSP or BU-LCSC/MSLSP)
#   HLS_IMAGERY_ROOT         scenes + water/dem/slope/aspect in <tile>/images/
#   MSLSP_NETCDF_ROOT        writes <tile>/phenoMetrics/MSLSP_<tile>_<year>.nc
#   PRIOR_YEAR TARGET_YEAR   phenology years (img years = those +/- 185-day buffer)
#   HLS_MSLSP_NCORES         default 8
#   HLS_MSLSP_NUM_CHUNKS     default 196 (30m operational)
#   HLS_MSLSP_DRY_RUN=true   write JSON + dirs only
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) {
  stop("Usage: Rscript run_mslsp_tile.R TILE")
}
tile <- args[[1L]]

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

trail <- function(p) {
  if (!nzchar(p)) return(p)
  if (grepl("/$", p)) p else paste0(p, "/")
}

# Write a patched copy of the MSLSP driver under the tile output dir.
# aliceni7/MSLSP _dev inconsistencies (clone left unchanged):
#   - comments out imgName_strip allocation then indexes it
#   - subsets with !is.na(keep) instead of keep
#   - lists HLS*Fmask.tif but ApplyMask_QA / runTopoCorrection still expect
#     the scene directory (qaName = dir/basename.Fmask.tif)
patch_mslsp_script <- function(src, dest) {
  txt <- readLines(src, warn = FALSE)
  orig <- txt
  txt <- sub(
    "^(\\s*)#\\s*(imgName_strip\\s*<-\\s*matrix\\(NA,\\s*length\\(imgList\\),\\s*1\\)\\s*)$",
    "\\1\\2",
    txt
  )
  for (nm in c("imgList", "yrdoy", "doys", "years", "sensor")) {
    from <- sprintf("%s <- %s[!is.na(keep)]", nm, nm)
    to <- sprintf("%s <- %s[keep]", nm, nm)
    txt <- gsub(from, to, txt, fixed = TRUE)
  }
  txt <- gsub(
    "ApplyMask_QA(imgList[j],",
    "ApplyMask_QA(dirname(imgList[j]),",
    txt,
    fixed = TRUE
  )
  txt <- gsub(
    "runTopoCorrection(subList[j],",
    "runTopoCorrection(dirname(subList[j]),",
    txt,
    fixed = TRUE
  )
  if (identical(txt, orig)) return(src)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, dest)
  dest
}

first_existing <- function(root, names) {
  for (nm in names) {
    p <- file.path(root, nm)
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  ""
}

hls_root_dir <- function() {
  hls <- env_first("HLS_ROOT")
  if (nzchar(hls)) return(hls)
  ccmmf <- env_first("CCMMF_ROOT")
  if (!nzchar(ccmmf)) stop("Set HLS_ROOT or CCMMF_ROOT.")
  file.path(ccmmf, "HLS")
}

algo_root <- function() {
  root <- env_first("MSLSP_ALGO_ROOT")
  if (nzchar(root) && dir.exists(root)) return(root)
  base <- env_first("CCMMF_BASE")
  if (nzchar(base)) {
    p <- file.path(base, "src", "MSLSP")
    if (dir.exists(p)) return(p)
  }
  phen <- env_first("HLS_PHENOLOGY_ROOT")
  if (nzchar(phen) && dir.exists(phen)) {
    hit <- first_existing(phen, c("MSLSP_Script.r", "MSLSP_Script_dev.R"))
    if (nzchar(hit)) return(phen)
  }
  stop(
    "Set MSLSP_ALGO_ROOT to your MSLSP clone ",
    "(git clone https://github.com/aliceni7/MSLSP.git $CCMMF_BASE/src/MSLSP)."
  )
}

prior <- as.integer(trimws(Sys.getenv("PRIOR_YEAR", "2023")))
target <- as.integer(trimws(Sys.getenv("TARGET_YEAR", "2024")))
if (is.na(prior) || is.na(target)) stop("PRIOR_YEAR and TARGET_YEAR must be integers.")
buffer_days <- as.integer(Sys.getenv("HLS_DOWNLOAD_BUFFER_DAYS", "185"))
if (is.na(buffer_days) || buffer_days < 0L) buffer_days <- 185L
img_start <- as.integer(format(as.Date(sprintf("%d-01-01", prior)) - buffer_days, "%Y"))
img_end <- as.integer(format(as.Date(sprintf("%d-12-31", target)) + buffer_days, "%Y"))

ncore <- as.integer(Sys.getenv("HLS_MSLSP_NCORES", "8"))
if (is.na(ncore) || ncore < 1L) ncore <- 1L
nchunk <- as.integer(Sys.getenv("HLS_MSLSP_NUM_CHUNKS", "196"))
if (is.na(nchunk) || nchunk < 1L) nchunk <- 196L

img_root <- env_first("HLS_IMAGERY_ROOT")
if (!nzchar(img_root)) img_root <- file.path(hls_root_dir(), "imagery")
nc_root <- env_first("MSLSP_NETCDF_ROOT")
if (!nzchar(nc_root)) nc_root <- file.path(hls_root_dir(), "MSLSP")

img_dir <- file.path(img_root, tile, "images")
tile_dir <- file.path(nc_root, tile)
temp_dir <- file.path(tile_dir, "temp")
chunk_dir <- file.path(tile_dir, "imageChunks")
phen_dir <- file.path(tile_dir, "phenoMetrics")
log_dir <- file.path(nc_root, "logs")

if (!dir.exists(img_dir)) {
  stop("No imagery tree: ", img_dir, "\nRun convert_hls_scenes.R first.")
}
fmask <- list.files(img_dir, pattern = "HLS.*Fmask\\.tif$", recursive = TRUE)
if (length(fmask) < 1L) {
  stop("No HLS*Fmask.tif under ", img_dir, " (download Fmask, then convert).")
}
for (kind in c("water", "dem", "slope", "aspect")) {
  p <- file.path(img_dir, paste0(kind, "_", tile, ".tif"))
  if (!file.exists(p)) {
    stop(
      "Missing ", p, "\n",
      "Conversion must copy ancillary into images/ before phenology."
    )
  }
}

root <- algo_root()
r_script <- first_existing(root, c(
  "MSLSP_Script.r", "MSLSP_Script.R",
  "MSLSP_Script_dev.R", "MSLSP_Script_dev.r"
))
if (!nzchar(r_script)) stop("No MSLSP_Script*.R in ", root)

use_dev <- grepl("dev", basename(r_script), ignore.case = TRUE)
r_fun <- if (use_dev) {
  first_existing(root, c("MSLSP_Functions_dev.R", "MSLSP_Functions_dev.r",
                         "MSLSP_Functions.r", "MSLSP_Functions.R"))
} else {
  first_existing(root, c("MSLSP_Functions.r", "MSLSP_Functions.R",
                         "MSLSP_Functions_dev.R", "MSLSP_Functions_dev.r"))
}
layers <- first_existing(root, "MSLSP_Layers.csv")
tmpl <- first_existing(root, c("MSLSP_Parameters.json", "MSLSP_parameters.json"))
if (!nzchar(r_fun) || !nzchar(layers) || !nzchar(tmpl)) {
  stop("MSLSP clone missing Functions / Layers.csv / Parameters.json in ", root)
}

params <- jsonlite::fromJSON(tmpl, simplifyVector = FALSE)
if (is.null(params$setup)) params$setup <- list()
if (is.null(params$SCC)) params$SCC <- list()
if (is.null(params$dirs)) params$dirs <- list()
if (is.null(params$phenology_parameters)) params$phenology_parameters <- list()

params$setup$AWS_or_SCC <- "SCC"
params$setup$imgStartYr <- img_start
params$setup$imgEndYr <- img_end
params$setup$phenStartYr <- prior
params$setup$phenEndYr <- target
params$setup$downloadImagery <- FALSE
params$setup$preprocessImagery <- TRUE
params$setup$runPhenology <- TRUE
params$setup$includeLandsat <- TRUE
params$setup$includeSentinel <- TRUE

params$SCC$workDir <- trail(nc_root)
params$SCC$logDir <- trail(log_dir)
params$SCC$dataDir <- trail(img_root)
params$SCC$rScript <- r_script
params$SCC$rFunctions <- r_fun
params$SCC$productTable <- layers
params$SCC$numCores <- ncore
params$SCC$numChunks <- nchunk
params$SCC$runS10 <- FALSE

params$dirs$imgDir <- trail(img_dir)
params$dirs$tempDir <- trail(temp_dir)
params$dirs$chunkDir <- trail(chunk_dir)
params$dirs$phenDir <- trail(phen_dir)

params$phenology_parameters$dormStart <- sprintf("%d-01-01", img_start)
params$phenology_parameters$dormEnd <- sprintf("%d-12-31", img_end)

dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phen_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# Match runTile: drop previous chunk dirs (names like c1/) before preprocess.
old_chunks <- list.dirs(chunk_dir, recursive = FALSE, full.names = TRUE)
old_chunks <- old_chunks[grepl("(/|\\\\)c[0-9]+$", old_chunks)]
if (length(old_chunks) > 0L) unlink(old_chunks, recursive = TRUE)

stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
json_out <- file.path(tile_dir, paste0("parameters_", stamp, ".json"))
run_log <- file.path(log_dir, paste0(tile, "_instanceInfo_", stamp, ".txt"))
err_log <- file.path(log_dir, paste0(tile, "_errorLog_", stamp, ".txt"))
jsonlite::write_json(params, json_out, auto_unbox = TRUE, pretty = TRUE, digits = NA)

writeLines(
  c(
    paste0("tile:", tile),
    paste0("time:", stamp),
    "run:local",
    paste0("num-cores:", ncore)
  ),
  run_log
)

message(
  "[mslsp tile] tile=", tile,
  " phen=", prior, "-", target,
  " img=", img_start, "-", img_end,
  " script=", r_script
)
message("[mslsp tile] imgDir=", img_dir)
message("[mslsp tile] phenDir=", phen_dir)
message("[mslsp tile] json=", json_out)

if (truthy("HLS_MSLSP_DRY_RUN")) {
  message("[mslsp tile] dry run; not calling MSLSP_Script.")
  quit(save = "no", status = 0L)
}

rscript <- file.path(R.home("bin"), "Rscript")
script_run <- patch_mslsp_script(
  r_script,
  file.path(tile_dir, paste0("MSLSP_Script_patched_", stamp, ".R"))
)
if (!identical(normalizePath(script_run, winslash = "/", mustWork = FALSE),
               normalizePath(r_script, winslash = "/", mustWork = FALSE))) {
  message("[mslsp tile] patched copy (clone unchanged): ", script_run)
}
cmd_args <- c(script_run, tile, json_out, run_log, err_log)
message("[mslsp tile] ", rscript, " ", paste(cmd_args, collapse = " "))
st <- system2(rscript, cmd_args)
if (!identical(st, 0L)) {
  stop(
    "MSLSP_Script failed for ", tile, " (exit ", st, "). See ",
    err_log, " and ", run_log
  )
}

nc <- file.path(phen_dir, sprintf("MSLSP_%s_%d.nc", tile, prior))
nc2 <- file.path(phen_dir, sprintf("MSLSP_%s_%d.nc", tile, target))
message("[mslsp tile] done; expect ", nc, " and ", nc2)
invisible(NULL)
