#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Download HLS scenes from NASA Earthdata (no hand-edits).
# -----------------------------------------------------------------------------
#
# Wraps NASA_DAAC_download() from HLS_Phenology/download_updated.R. Default
# search is the California bounding box (same as that repo). Set dates / DOI /
# paths via env. Do not edit download_updated.R.
#
# One tile (optional):
#   export HLS_DOWNLOAD_TILE=10TEK
# Uses $HLS_S2_MGRS_GRID / $HLS_ROOT/s2_mgrs_grid_ca.gpkg for that tile's bbox,
# then keeps granule names matching .T10TEK. (CMR bbox still sees neighbors).
# Unset HLS_DOWNLOAD_TILE for statewide CA.
#
# Prerequisites:
#   - ~/.netrc Earthdata Login (Session 0 Sec. 0.6)
#   - clone: git clone https://github.com/mrinareddy/HLS_Phenology.git
#   - one-tile: s2_mgrs_grid_ca.gpkg on $HLS_ROOT
#
# Usage:
#   export HLS_PHENOLOGY_ROOT=$CCMMF_BASE/src/HLS_Phenology
#   export HLS_DOWNLOAD_OUTDIR=$HLS_IMAGERY_ROOT/download_scratch
#   export HLS_CREDENTIAL_FOLDER=$HOME
# Default date window is PRIOR_YEAR minus 185 days through TARGET_YEAR
# plus 185 days (MSLSP valid DOY -181 to 548). Override with:
#   export HLS_DOWNLOAD_FROM=YYYY-MM-DD
#   export HLS_DOWNLOAD_TO=YYYY-MM-DD
#   export HLS_DOWNLOAD_BUFFER_DAYS=185
# Unset HLS_DOWNLOAD_DOI runs Sentinel-2 then Landsat. One collection:
#   export HLS_DOWNLOAD_DOI=10.5067/HLS/HLSS30.002
#   Rscript "$CCMMF_CODE/hls/download_hls_earthdata.R"
#
# Optional: HLS_DOWNLOAD_NCORE (default 8), HLS_DOWNLOAD_JUST_PATH=true
# Includes Fmask with the reflectance bands (needed by MSLSP / conversion).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(foreach)
  library(doParallel)
})

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

hls_phenology_root <- function() {
  root <- env_first("HLS_PHENOLOGY_ROOT")
  if (nzchar(root)) return(root)
  base <- env_first("CCMMF_BASE")
  if (nzchar(base)) {
    p <- file.path(base, "src", "HLS_Phenology")
    if (dir.exists(p)) return(p)
  }
  stop(
    "Set HLS_PHENOLOGY_ROOT to your HLS_Phenology clone ",
    "(e.g. $CCMMF_BASE/src/HLS_Phenology)."
  )
}

hls_root_dir <- function() {
  hls <- env_first("HLS_ROOT")
  if (nzchar(hls)) return(hls)
  ccmmf <- env_first("CCMMF_ROOT")
  if (!nzchar(ccmmf)) return("")
  file.path(ccmmf, "HLS")
}

path_s2_mgrs_grid <- function() {
  env <- env_first("HLS_S2_MGRS_GRID")
  if (nzchar(env) && file.exists(env)) return(env)
  hls <- hls_root_dir()
  if (nzchar(hls)) {
    p <- file.path(hls, "s2_mgrs_grid_ca.gpkg")
    if (file.exists(p)) return(p)
  }
  stop(
    "HLS_DOWNLOAD_TILE needs s2_mgrs_grid_ca.gpkg ",
    "(set HLS_S2_MGRS_GRID or place it under $HLS_ROOT)."
  )
}

normalize_mgrs_tile <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("^T", "", x)
  if (!nzchar(x)) stop("HLS_DOWNLOAD_TILE is empty.")
  x
}

tile_bbox_wgs84 <- function(tile_id, gpkg) {
  suppressPackageStartupMessages(library(sf))
  g <- sf::st_read(gpkg, quiet = TRUE)
  if (!"tile_id" %in% names(g) && "Name" %in% names(g)) {
    g$tile_id <- as.character(g$Name)
  }
  g$tile_id <- sub("^T", "", toupper(as.character(g$tile_id)))
  one <- g[g$tile_id == tile_id, , drop = FALSE]
  if (nrow(one) != 1L) {
    stop("Tile ", tile_id, " not in ", gpkg, " (n=", nrow(one), ").")
  }
  one <- sf::st_transform(one, 4326)
  bb <- sf::st_bbox(one)
  list(
    ul_lat = unname(as.numeric(bb["ymax"])),
    ul_lon = unname(as.numeric(bb["xmin"])),
    lr_lat = unname(as.numeric(bb["ymin"])),
    lr_lon = unname(as.numeric(bb["xmax"]))
  )
}

filter_tile_hrefs <- function(hrefs, tile_id) {
  if (length(hrefs) < 1L) return(character())
  tok <- paste0("T", tile_id)
  keep <- grepl(
    paste0("(^|\\.)", tok, "\\."),
    basename(hrefs),
    ignore.case = TRUE
  )
  hrefs[keep]
}

download_hrefs <- function(hrefs, outdir, netrc, ncore) {
  dests <- file.path(outdir, basename(hrefs))
  todo <- !file.exists(dests)
  if (!any(todo)) {
    message("[hls download] all ", length(hrefs), " file(s) already in outdir")
    return(dests)
  }
  hrefs_d <- hrefs[todo]
  dests_d <- dests[todo]
  message(
    "[hls download] downloading ", length(hrefs_d), " file(s) using ",
    ncore, " core(s)"
  )
  fetch_one <- function(href, dest) {
    httr::GET(
      href,
      httr::write_disk(dest, overwrite = TRUE),
      httr::config(netrc = TRUE, netrc_file = netrc),
      httr::set_cookies("LC" = "cookies")
    )
    if (!file.exists(dest) || file.info(dest)$size < 1) {
      stop("Download failed: ", href)
    }
    dest
  }
  if (ncore > 1L && length(hrefs_d) > 1L) {
    cl <- parallel::makeCluster(min(as.integer(ncore), length(hrefs_d)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doParallel::registerDoParallel(cl)
    foreach::foreach(
      i = seq_along(hrefs_d),
      .packages = "httr"
    ) %dopar% {
      fetch_one(hrefs_d[[i]], dests_d[[i]])
    }
    foreach::registerDoSEQ()
  } else {
    for (i in seq_along(hrefs_d)) {
      fetch_one(hrefs_d[[i]], dests_d[[i]])
    }
  }
  dests
}

source_daac_functions <- function(download_r) {
  if (!file.exists(download_r)) {
    stop("Missing download_updated.R: ", download_r)
  }
  lines <- readLines(download_r, warn = FALSE)
  # Driver block in upstream starts at California ul_lat assignment.
  cut <- which(grepl("^ul_lat\\s*<-", lines))
  if (length(cut) < 1L) {
    stop("Could not find driver block (ul_lat <-) in ", download_r)
  }
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines[seq_len(cut[[1L]] - 1L)], tmp)
  sys.source(tmp, envir = .GlobalEnv)
  if (!exists("NASA_DAAC_download", mode = "function")) {
    stop("NASA_DAAC_download() not found after sourcing ", download_r)
  }
  invisible(NULL)
}

# California bbox (HLS_Phenology download_updated.R)
ul_lat <- 42.0095082699265845
ul_lon <- -124.4820168611238245
lr_lat <- 32.5288367369123748
lr_lon <- -114.1312224747231312

# Reflectance bands used by HLS_Phenology (Landsat + Sentinel names) plus Fmask
# (HLS v2.0 ships Fmask with L30 and S30; conversion / MSLSP need it).
default_bands <- c(
  "B02", "B03", "B04", "B8A", "B11", "B12", "B05", "B06", "B07", "Fmask"
)

prior <- as.integer(trimws(Sys.getenv("PRIOR_YEAR", "2023")))
target <- as.integer(trimws(Sys.getenv("TARGET_YEAR", "2024")))
if (is.na(prior) || is.na(target)) {
  stop("PRIOR_YEAR and TARGET_YEAR must be integers.")
}
buffer_days <- as.integer(Sys.getenv("HLS_DOWNLOAD_BUFFER_DAYS", "185"))
if (is.na(buffer_days) || buffer_days < 0L) buffer_days <- 185L
from <- env_first("HLS_DOWNLOAD_FROM")
if (!nzchar(from)) {
  from <- format(as.Date(sprintf("%d-01-01", prior)) - buffer_days, "%Y-%m-%d")
}
to <- env_first("HLS_DOWNLOAD_TO")
if (!nzchar(to)) {
  to <- format(as.Date(sprintf("%d-12-31", target)) + buffer_days, "%Y-%m-%d")
}

doi_env <- env_first("HLS_DOWNLOAD_DOI")
dois <- if (nzchar(doi_env)) {
  doi_env
} else {
  c("10.5067/HLS/HLSS30.002", "10.5067/HLS/HLSL30.002")
}

outdir <- env_first("HLS_DOWNLOAD_OUTDIR")
if (!nzchar(outdir)) {
  img <- env_first("HLS_IMAGERY_ROOT")
  if (!nzchar(img)) {
    hls <- hls_root_dir()
    if (!nzchar(hls)) stop("Set HLS_DOWNLOAD_OUTDIR or HLS_IMAGERY_ROOT / HLS_ROOT / CCMMF_ROOT.")
    img <- file.path(hls, "imagery")
  }
  outdir <- file.path(img, "download_scratch")
}

cred <- env_first("HLS_CREDENTIAL_FOLDER", "EARTHDATA_NETRC_DIR")
if (!nzchar(cred)) cred <- path.expand("~")
netrc_path <- file.path(cred, ".netrc")
if (!file.exists(netrc_path)) {
  stop(
    "No .netrc at ", netrc_path, "\n",
    "Session 0 puts Earthdata credentials in ~/.netrc; ",
    "set HLS_CREDENTIAL_FOLDER to that directory (usually $HOME)."
  )
}

ncore <- as.integer(Sys.getenv("HLS_DOWNLOAD_NCORE", "8"))
if (is.na(ncore) || ncore < 1L) ncore <- 1L
just_path <- truthy("HLS_DOWNLOAD_JUST_PATH")

tile_raw <- env_first("HLS_DOWNLOAD_TILE")
tile_id <- if (nzchar(tile_raw)) normalize_mgrs_tile(tile_raw) else ""
if (nzchar(tile_id)) {
  gpkg <- path_s2_mgrs_grid()
  bb <- tile_bbox_wgs84(tile_id, gpkg)
  ul_lat <- bb$ul_lat
  ul_lon <- bb$ul_lon
  lr_lat <- bb$lr_lat
  lr_lon <- bb$lr_lon
}

src_r <- file.path(hls_phenology_root(), "download_updated.R")
message("[hls download] sourcing functions from ", src_r)
source_daac_functions(src_r)

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
scope <- if (nzchar(tile_id)) paste0("tile=", tile_id) else "CA bbox"

empty_result <- function(paths) {
  length(paths) == 0L ||
    (length(paths) == 1L && (is.na(paths) || identical(paths, 0)))
}

download_one_doi <- function(doi) {
  message(
    "[hls download] ", scope, "; from=", from, " to=", to,
    " doi=", doi, " outdir=", outdir, " ncore=", ncore,
    " credential.folder=", cred
  )

  if (!nzchar(tile_id)) {
    paths <- NASA_DAAC_download(
      ul_lat = ul_lat,
      ul_lon = ul_lon,
      lr_lat = lr_lat,
      lr_lon = lr_lon,
      ncore = ncore,
      from = from,
      to = to,
      outdir = outdir,
      band = default_bands,
      credential.folder = cred,
      doi = doi,
      just_path = just_path
    )
  } else {
    hrefs <- NASA_DAAC_download(
      ul_lat = ul_lat,
      ul_lon = ul_lon,
      lr_lat = lr_lat,
      lr_lon = lr_lon,
      ncore = ncore,
      from = from,
      to = to,
      outdir = outdir,
      band = default_bands,
      credential.folder = cred,
      doi = doi,
      just_path = TRUE
    )
    if (empty_result(hrefs)) hrefs <- character()
    hrefs <- as.character(hrefs)
    hrefs <- hrefs[nzchar(hrefs) & !is.na(hrefs)]
    n_cmr <- length(hrefs)
    hrefs <- filter_tile_hrefs(hrefs, tile_id)
    message(
      "[hls download] CMR hrefs=", n_cmr, " after tile filter=", length(hrefs)
    )
    if (just_path) {
      paths <- hrefs
    } else if (length(hrefs) < 1L) {
      paths <- hrefs
    } else {
      paths <- download_hrefs(hrefs, outdir, netrc_path, ncore)
    }
  }

  if (empty_result(paths)) {
    stop("[hls download] no files downloaded / listed (check DOI, dates, credentials, tile).")
  }
  message("[hls download] done: ", length(paths), " path(s) for ", doi)
  invisible(paths)
}

for (doi in dois) {
  download_one_doi(doi)
}
