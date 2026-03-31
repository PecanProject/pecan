# Shared tilewise orchestration for HLS-derived products (MSLSP, NDTI).
# Implements prep-static, per-tile extract to tilepieces, combine to Parquet, and optional merge.
#
# Main inputs: product object (see Product interface at bottom of file).
# Main outputs: tilepiece CSV.gz dirs and product-specific Parquet paths.
# How to run: sourced from tilewise_*_driver.R; not a standalone CLI.
# Workflow: core of monitoring workflow stages S2, S5 for raster summaries.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(stringr)
})

#### Logging
# Timestamped log to console and optional file. Driver calls tilewise_log_init()
# to enable file sink; tw_log() works console-only if not set.

.tw_state <- new.env(parent = emptyenv())
.tw_state$log_con  <- NULL
.tw_state$log_path <- NULL

tilewise_log_init <- function(log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  .tw_state$log_con  <- file(log_path, open = "at")
  .tw_state$log_path <- log_path
  do.call("on.exit", list(quote(tilewise_log_close()), add = TRUE), envir = parent.frame())
  tw_log("INFO", "=== run started  pid=", Sys.getpid(), " ===")
  message("[log] writing to: ", log_path)
  invisible(log_path)
}

tilewise_log_close <- function() {
  if (!is.null(.tw_state$log_con)) {
    tw_log("INFO", "=== run finished ===")
    close(.tw_state$log_con)
    .tw_state$log_con  <- NULL
    .tw_state$log_path <- NULL
  }
}

tw_log <- function(level = "INFO", ...) {
  msg  <- paste0(c(...), collapse = "")
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("%s [%-5s] %s", ts, level, msg)
  message(line)
  if (!is.null(.tw_state$log_con)) {
    writeLines(line, .tw_state$log_con)
    flush(.tw_state$log_con)
  }
}

#### Generic helpers

parcel_id_to_bucket <- function(parcel_ids, n_buckets) {
  x <- suppressWarnings(as.integer(parcel_ids))
  x[is.na(x)] <- 0L
  (abs(x) %% as.integer(n_buckets)) + 1L
}

append_to_csv <- function(data, filepath) {
  if (file.exists(filepath)) {
    fwrite(data, filepath, append = TRUE, col.names = FALSE)
  } else {
    fwrite(data, filepath)
  }
}

concat_gzipped_csvs <- function(input_files, output_file) {
  input_files <- sort(input_files[file.exists(input_files)])
  if (length(input_files) == 0) stop("No input files to combine")
  if (length(input_files) == 1) {
    cmd <- sprintf("zcat %s | gzip -c > %s", shQuote(input_files[1]), shQuote(output_file))
  } else {
    rest <- paste(shQuote(input_files[-1]), collapse = " ")
    cmd <- sprintf(
      "(zcat %s; for f in %s; do zcat \"$f\" | sed '1d'; done) | gzip -c > %s",
      shQuote(input_files[1]), rest, shQuote(output_file)
    )
  }
  if (system(cmd) != 0L) stop("Concatenation failed: ", output_file)
  invisible(output_file)
}

# Build a list mapping tile_id -> row indices in prep$polys.
# prep$polys must have a tile_ids list column (each element is a character vector
# of tile IDs the parcel overlaps).
build_tile_to_parcel_indices <- function(polys) {
  tile_ids_list <- polys$tile_ids
  rows  <- rep.int(seq_len(nrow(polys)), lengths(tile_ids_list))
  tiles <- unlist(tile_ids_list, use.names = FALSE)
  ok    <- !is.na(tiles) & nzchar(tiles)
  split(rows[ok], tiles[ok])
}

sanitize_tile_id <- function(id) gsub("[^0-9A-Za-z]+", "_", id)

#### tilewise_run (extract per tile and scene)

tilewise_run <- function(prep, time_key, product, overwrite = FALSE, verbose = TRUE) {
  year     <- prep$year
  time_key <- as.integer(time_key)

  scene_index_dt <- product$scene_index(year, time_key, verbose = verbose)
  if (is.null(scene_index_dt) || nrow(scene_index_dt) == 0) {
    if (verbose) tw_log("WARN", "no scenes for year=", year, " time_key=", time_key)
    return(invisible(NULL))
  }
  tile_col <- product$scene_index_tile_col
  if (!tile_col %in% names(scene_index_dt)) stop("scene_index must have column: ", tile_col)

  tile_to_indices <- build_tile_to_parcel_indices(prep$polys)
  tile_ids <- sort(names(tile_to_indices))

  # Smoke-test: restrict to one tile via env var.
  #   TILEWISE_ONE_TILE=1       -> first tile
  #   TILEWISE_ONE_TILE=10SFF   -> specific tile
  one_tile <- Sys.getenv("TILEWISE_ONE_TILE", "")
  if (nzchar(one_tile) && length(tile_ids) > 0) {
    one_tile_lower <- tolower(one_tile)
    if (one_tile_lower %in% c("1", "true", "yes", "y", "first")) {
      tile_ids <- tile_ids[1]
    } else if (one_tile %in% tile_ids) {
      tile_ids <- one_tile
    } else {
      stop("TILEWISE_ONE_TILE is set but tile not found in prep: ", one_tile)
    }
    if (verbose) tw_log("INFO", "TILEWISE_ONE_TILE -> ", tile_ids)
  }

  tilepieces_dir <- product$path_tilepieces(prep$out_dir, year, time_key)
  dir.create(tilepieces_dir, recursive = TRUE, showWarnings = FALSE)

  relevant_scene_rows <- scene_index_dt[get(tile_col) %in% tile_ids]
  if (verbose) {
    tw_log("INFO",
      "year=", year, " time_key=", time_key,
      " tiles=", length(tile_ids),
      " relevant_scenes=", nrow(relevant_scene_rows)
    )
  }

  # Per-tile timing: one row per processed tile, written to tilepieces_dir at the end.
  timing_rows <- list()

  write_empty_tilepiece <- function(tmp, gz) {
    empty <- product$empty_tilepiece_schema()
    fwrite(empty, tmp)
    gz_status <- system2("gzip", c("-f", tmp))
    if (gz_status != 0L) {
      tw_log("WARN", "gzip failed for empty tilepiece ", tmp, "; keeping .csv")
    } else {
      renamed <- file.rename(paste0(tmp, ".gz"), gz)
      if (!isTRUE(renamed)) tw_log("WARN", "rename failed for empty tilepiece ", tmp)
    }
  }

  for (tile_id in tile_ids) {
    parcel_indices <- tile_to_indices[[tile_id]]
    if (length(parcel_indices) == 0) next

    tile_safe  <- sanitize_tile_id(tile_id)
    output_gz  <- file.path(tilepieces_dir, sprintf("tile=%s.csv.gz", tile_safe))
    output_csv <- file.path(tilepieces_dir, sprintf("tile=%s.csv",    tile_safe))
    if ((file.exists(output_gz) || file.exists(output_csv)) && !overwrite) next

    output_tmp <- output_csv
    if (file.exists(output_tmp)) file.remove(output_tmp)

    tile_start  <- proc.time()[["elapsed"]]
    start_ts    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    n_rows      <- 0L

    # Scenes for this tile - fetched before geometry so prepare_tile can use CRS.
    # Use cur_tile (not tile_id) so data.table doesn't match the column of same name.
    cur_tile <- tile_id
    scenes_this_tile <- scene_index_dt[get(tile_col) == cur_tile]

    if (nrow(scenes_this_tile) == 0) {
      if (verbose) tw_log("INFO", "tile=", tile_id, " 0 scenes - writing empty tilepiece")
      write_empty_tilepiece(output_tmp, output_gz)
      timing_rows[[length(timing_rows) + 1]] <- list(
        tile_id = tile_id, n_parcels = length(parcel_indices), n_scenes = 0L,
        n_rows_written = 0L, status = "empty_no_scenes",
        elapsed_sec = round(proc.time()[["elapsed"]] - tile_start, 1),
        start_ts = start_ts, end_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      next
    }

    # Per-tile geometry context.
    # If the product defines prepare_tile, call it (e.g. NDTI loads geometry
    # lazily from the GPKG for just this tile's parcels, already reprojected).
    # Otherwise fall back to slicing prep$polys (MSLSP loads all geometry upfront).
    if (!is.null(product$prepare_tile)) {
      parcel_ids_tile <- prep$polys$parcel_id[parcel_indices]
      tile_parcels <- product$prepare_tile(prep, tile_id, parcel_ids_tile, scenes_this_tile)
    } else {
      tile_parcels <- prep$polys[parcel_indices, ]
      tile_parcels$tile_ids <- replicate(nrow(tile_parcels), tile_id, simplify = FALSE)
    }

    if (is.null(tile_parcels) || nrow(tile_parcels) == 0) {
      if (verbose) tw_log("WARN", "tile=", tile_id, " no parcel geometry - writing empty tilepiece")
      write_empty_tilepiece(output_tmp, output_gz)
      timing_rows[[length(timing_rows) + 1]] <- list(
        tile_id = tile_id, n_parcels = length(parcel_indices), n_scenes = nrow(scenes_this_tile),
        n_rows_written = 0L, status = "empty_no_geometry",
        elapsed_sec = round(proc.time()[["elapsed"]] - tile_start, 1),
        start_ts = start_ts, end_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      next
    }

    if (verbose) tw_log("INFO",
      "tile=", tile_id,
      " parcels=", nrow(tile_parcels),
      " scenes=", nrow(scenes_this_tile))

    tile_status <- "ok"
    for (scene_idx in seq_len(nrow(scenes_this_tile))) {
      scene_row <- scenes_this_tile[scene_idx]
      result    <- tryCatch(
        product$process_scene(prep, scene_row, tile_parcels, tile_id),
        error = function(e) {
          tw_log("ERROR", "tile=", tile_id, " scene=", scene_idx, " ", conditionMessage(e))
          tile_status <<- "error"
          NULL
        }
      )
      if (!is.null(result) && nrow(result) > 0) {
        result[, tile_id := tile_id]
        append_to_csv(result, output_tmp)
        n_rows <- n_rows + nrow(result)
      }
    }

    if (file.exists(output_tmp)) {
      gz_status <- system2("gzip", c("-f", output_tmp))
      if (gz_status != 0L) {
        tw_log("WARN", "gzip failed for ", output_tmp, "; keeping .csv and continuing")
      } else {
        renamed <- file.rename(paste0(output_tmp, ".gz"), output_gz)
        if (!isTRUE(renamed)) {
          tw_log("WARN", "rename to canonical .csv.gz failed for ", output_tmp)
        }
      }
    } else {
      write_empty_tilepiece(output_tmp, output_gz)
    }

    elapsed <- round(proc.time()[["elapsed"]] - tile_start, 1)
    if (verbose) tw_log("TIME", "tile=", tile_id, " done  rows=", n_rows, " elapsed=", elapsed, "s")

    timing_rows[[length(timing_rows) + 1]] <- list(
      tile_id = tile_id, n_parcels = nrow(tile_parcels), n_scenes = nrow(scenes_this_tile),
      n_rows_written = n_rows, status = tile_status,
      elapsed_sec = elapsed,
      start_ts = start_ts, end_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  }

  # Write per-tile timing summary so slow/failed tiles are easy to spot.
  if (length(timing_rows) > 0) {
    timing_dt <- rbindlist(timing_rows)
    timing_dt[, `:=`(year = year, time_key = time_key)]
    timing_path <- file.path(tilepieces_dir, "_tile_timing.csv")
    fwrite(timing_dt, timing_path)
    if (verbose) tw_log("INFO", "tile timing written to: ", timing_path)
  }

  invisible(tilepieces_dir)
}

#### tilewise_combine (aggregate tilepieces)

tilewise_combine <- function(prep, time_key, product, n_buckets = 256L,
                             overwrite = FALSE, verbose = TRUE) {
  year     <- prep$year
  time_key <- as.integer(time_key)

  # Products that define combine() bypass the generic bucket
  # scatter/aggregate path entirely.
  if (!is.null(product$combine)) {
    return(product$combine(prep, time_key, overwrite = overwrite, verbose = verbose))
  }

  #### Generic bucket combine (products without custom combine)
  tilepieces_dir <- product$path_tilepieces(prep$out_dir, year, time_key)
  parts_dir      <- product$path_combine_parts(prep$out_dir, year, time_key)
  tile_files     <- list.files(tilepieces_dir, "^tile=.*\\.csv(\\.gz)?$", full.names = TRUE)
  if (length(tile_files) == 0) stop("No tilepieces in: ", tilepieces_dir)

  dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
  done_marker <- file.path(parts_dir, "DONE.marker")
  if (file.exists(done_marker) && !overwrite) {
    if (verbose) tw_log("INFO", "[combine] already done: ", parts_dir)
    return(invisible(parts_dir))
  }

  tmp_dir      <- file.path(parts_dir, "tmp_buckets")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  bucket_paths <- file.path(tmp_dir, sprintf("bucket=%03d.csv", seq_len(n_buckets)))
  if (overwrite) for (p in bucket_paths) if (file.exists(p)) file.remove(p)

  # Pass 1: scatter rows into buckets so all rows for one parcel land together.
  if (verbose) tw_log("INFO", "[combine] pass1: scatter into ", n_buckets, " buckets")
  for (f in tile_files) {
    dt <- try(fread(f, showProgress = FALSE), silent = TRUE)
    if (inherits(dt, "try-error") || nrow(dt) == 0) next
    if (!product$validate_tilepiece(dt)) next
    dt_scatter <- product$prepare_for_scatter(dt, n_buckets)
    by_bucket  <- split(dt_scatter, dt_scatter$bucket)
    for (bn in names(by_bucket)) {
      append_to_csv(product$scatter_cols(by_bucket[[bn]]), bucket_paths[as.integer(bn)])
    }
  }

  # Pass 2: aggregate each bucket independently (bounded memory).
  if (verbose) tw_log("INFO", "[combine] pass2: aggregate buckets")
  empty_part <- product$empty_part_schema(year)
  for (b in seq_len(n_buckets)) {
    part_file <- file.path(parts_dir, sprintf("part=%03d.csv.gz", b))
    if (!file.exists(bucket_paths[b])) {
      fwrite(empty_part, sub("\\.gz$", "", part_file))
      system2("gzip", c("-f", sub("\\.gz$", "", part_file)))
      next
    }
    dt <- fread(bucket_paths[b], showProgress = FALSE)
    if (nrow(dt) == 0) {
      fwrite(empty_part, sub("\\.gz$", "", part_file))
      system2("gzip", c("-f", sub("\\.gz$", "", part_file)))
      next
    }
    agg <- product$aggregate_bucket(dt, year)
    fwrite(agg, sub("\\.gz$", "", part_file))
    system2("gzip", c("-f", sub("\\.gz$", "", part_file)))
  }
  writeLines(c(
    sprintf("year=%d", year),
    sprintf("time_key=%d", time_key),
    sprintf("n_buckets=%d", n_buckets)
  ), done_marker)
  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  invisible(parts_dir)
}

#### tilewise_merge (concatenate bucket parts)

tilewise_merge <- function(prep, time_key, product, overwrite = FALSE, verbose = TRUE) {
  year     <- prep$year
  time_key <- as.integer(time_key)

  # Products that define combine() already wrote the final output there.
  if (!is.null(product$combine)) {
    out_path <- product$path_final_output(prep$out_dir, year, time_key)
    if (file.exists(out_path)) {
      if (verbose) message("[merge] skipped - combine already produced: ", out_path)
      return(invisible(out_path))
    }
    # Final output missing - run combine now.
    if (verbose) message("[merge] final output missing, running combine")
    return(product$combine(prep, time_key, overwrite = overwrite, verbose = verbose))
  }

  # Concatenate bucket parts into final output.
  parts_dir  <- product$path_combine_parts(prep$out_dir, year, time_key)
  part_files <- sort(list.files(parts_dir, "^part=[0-9]+\\.csv\\.gz$", full.names = TRUE))
  if (length(part_files) == 0) stop("No part files in: ", parts_dir)
  output_path <- product$path_final_output(prep$out_dir, year, time_key)
  if (file.exists(output_path) && !overwrite) {
    if (verbose) tw_log("INFO", "[merge] skip (exists): ", output_path)
    return(invisible(output_path))
  }
  if (verbose) tw_log("INFO", "[merge] concatenating ", length(part_files), " parts -> ", output_path)
  concat_gzipped_csvs(part_files, output_path)
  invisible(output_path)
}

tilewise_prep_static <- function(year, product, ...) {
  product$prep_static(year, ...)
}

#### Product interface
# Required:
#   product$prep_static(year, ...)
#     -> list(year, polys, out_dir)
#        polys must have columns: parcel_id, tile_ids (list of tile ID strings)
#        polys may be an sf object (MSLSP) or a plain data.table (NDTI)
#   product$scene_index(year, time_key, verbose) -> data.table with tile column
#   product$scene_index_tile_col -> column name in scene_index result
#   product$process_scene(prep, scene_row, tile_parcels, tile_id) -> data.table or NULL
#   product$path_tilepieces(out_dir, year, time_key) -> directory path
#   product$path_final_output(out_dir, year, time_key) -> file path
#   product$empty_tilepiece_schema() -> zero-row data.table with correct columns
#
# Optional:
#   product$prepare_tile(prep, tile_id, parcel_ids, scenes_this_tile)
#     -> tile-specific context (e.g. sf geometry) passed to process_scene
#        when NULL, framework slices prep$polys[parcel_indices, ]
#   product$combine(prep, time_key, overwrite, verbose)
#     -> full combine: read tilepieces, aggregate, write final output
#        when NULL, framework uses generic bucket scatter/aggregate/merge
#
# Required only when combine is NULL (generic bucket path):
#   product$path_combine_parts(out_dir, year, time_key) -> parts directory
#   product$validate_tilepiece(dt) -> logical
#   product$prepare_for_scatter(dt, n_buckets) -> dt with bucket column
#   product$scatter_cols(dt) -> dt without bucket column
#   product$aggregate_bucket(dt, year) -> aggregated dt
#   product$empty_part_schema(year) -> zero-row data.table
