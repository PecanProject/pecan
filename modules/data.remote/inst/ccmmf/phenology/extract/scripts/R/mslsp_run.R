# MSLSP pipeline steps -- called by atomic CLI scripts and run_mslsp.sh.

MSLSP_TIME_KEY <- 1L

mslsp_init_arrow <- function() {
  suppressPackageStartupMessages(library(arrow))
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(data.frame(x = 1L), tmp)
  arrow::read_parquet(tmp)
}

mslsp_product <- function() {
  product_mslsp()
}

mslsp_log_init <- function(year, command, tile = "") {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_dir <- file.path(mslsp_out_root, sprintf("year=%d", year), "logs")
  if (exists("tilewise_log_init", mode = "function")) {
    suffix <- if (nzchar(tile)) sprintf("_%s", tile) else ""
    tilewise_log_init(file.path(log_dir, sprintf("mslsp_%s%s_%s.log", command, suffix, ts)))
  }
}

mslsp_get_prep <- function(year, overwrite = FALSE) {
  tilewise_prep_static(year, mslsp_product(), overwrite = overwrite)
}

run_mslsp_prep_static <- function(year, overwrite = FALSE) {
  mslsp_init_arrow()
  year <- as.integer(year)
  mslsp_log_init(year, "prep-static")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "MSLSP prep-static year=", year, " overwrite=", overwrite,
           " pid=", Sys.getpid())
  }
  prep <- mslsp_get_prep(year, overwrite = overwrite)
  tiles_path <- mslsp_tiles_to_run_path(year)
  if (overwrite || !file.exists(tiles_path)) {
    write_mslsp_tiles_to_run(year, prep)
  }
  invisible(prep)
}

run_mslsp_extract <- function(year, overwrite = FALSE, tile = NULL) {
  mslsp_init_arrow()
  year <- as.integer(year)
  tile <- if (is.null(tile)) "" else as.character(tile)[1L]
  mslsp_log_init(year, "extract", tile = tile)
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "MSLSP extract year=", year,
           if (nzchar(tile)) paste0(" tile=", tile) else "",
           " overwrite=", overwrite, " pid=", Sys.getpid(),
           " TASK_ID=", Sys.getenv("TASK_ID", ""))
  }

  prep <- mslsp_get_prep(year, overwrite = overwrite)

  if (nzchar(tile) && !tile %in% names(prep$tile_to_parcel_ids)) {
    if (exists("tw_log", mode = "function")) {
      tw_log("INFO", "tile=", tile, " no ag parcels for year=", year, " -- skip")
    } else {
      message("[MSLSP] tile ", tile, " has no ag parcels for year ", year, " -- skip")
    }
    return(invisible(NULL))
  }

  old_tile <- Sys.getenv("TILEWISE_ONE_TILE", "")
  on.exit({
    if (nzchar(old_tile)) {
      Sys.setenv(TILEWISE_ONE_TILE = old_tile)
    } else {
      Sys.unsetenv("TILEWISE_ONE_TILE")
    }
  }, add = TRUE)
  if (nzchar(tile)) {
    Sys.setenv(TILEWISE_ONE_TILE = tile)
  }

  tilewise_run(prep, MSLSP_TIME_KEY, mslsp_product(), overwrite = overwrite)
}

run_mslsp_combine <- function(year, overwrite = FALSE) {
  mslsp_init_arrow()
  year <- as.integer(year)
  mslsp_log_init(year, "combine")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "MSLSP combine year=", year, " overwrite=", overwrite)
  }
  prep <- mslsp_get_prep(year, overwrite = FALSE)
  tilewise_combine(prep, MSLSP_TIME_KEY, mslsp_product(), overwrite = overwrite)
}

run_mslsp_all <- function(year, overwrite = FALSE) {
  run_mslsp_extract(year, overwrite = overwrite)
  run_mslsp_combine(year, overwrite = overwrite)
}

read_mslsp_tile_list <- function() {
  path <- path_mslsp_tile_list()
  if (!file.exists(path)) {
    stop(
      "MSLSP tile list not found: ", path, "\n",
      "  Expected HLS_Phenology tileids.txt -- set MSLSP_TILE_LIST or CCMMF_ROOT"
    )
  }
  tiles <- trimws(readLines(path, warn = FALSE))
  tiles[nzchar(tiles)]
}

# Tiles to run: canonical tileids.txt order, restricted to tiles with ag parcels
# for this year (from prep cache). Not all HLS tiles have ag land.
mslsp_tiles_to_run_path <- function(year) {
  file.path(mslsp_out_root, sprintf("year=%d", as.integer(year)), "tiles_to_run.txt")
}

mslsp_tiles_to_run <- function(prep) {
  canonical <- read_mslsp_tile_list()
  with_parcels <- names(prep$tile_to_parcel_ids)
  canonical[canonical %in% with_parcels]
}

write_mslsp_tiles_to_run <- function(year, prep) {
  tiles <- mslsp_tiles_to_run(prep)
  path <- mslsp_tiles_to_run_path(year)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(tiles, path)
  n_canonical <- length(read_mslsp_tile_list())
  msg <- paste0(
    "[MSLSP prep] tiles_to_run written: ", path,
    " (", length(tiles), " with ag parcels / ", n_canonical, " in tileids.txt)"
  )
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", msg)
  } else {
    message(msg)
  }
  invisible(path)
}

read_mslsp_tiles_to_run <- function(year) {
  path <- mslsp_tiles_to_run_path(year)
  if (!file.exists(path)) {
    stop(
      "Tile list not found: ", path,
      "\nRun: Rscript prep_static.R ", year,
      " (intersects tileids.txt with tiles that have ag parcels)"
    )
  }
  tiles <- trimws(readLines(path, warn = FALSE))
  tiles[nzchar(tiles)]
}

run_mslsp_extract_task_tile <- function(year, overwrite = FALSE) {
  year <- as.integer(year)
  task_id <- suppressWarnings(as.integer(Sys.getenv("TASK_ID", "")))
  if (is.na(task_id) || task_id < 1L) {
    stop("TASK_ID must be set for tile-array extraction (1-based line in tiles_to_run.txt)")
  }
  tiles <- read_mslsp_tiles_to_run(year)
  if (task_id > length(tiles)) {
    message("[MSLSP] TASK_ID ", task_id, " > ", length(tiles), " scheduled tiles -- nothing to do")
    return(invisible(NULL))
  }
  run_mslsp_extract(year, overwrite = overwrite, tile = tiles[[task_id]])
}
