# =============================================================================
# tilewise_ndti_implementation.R - NDTI product for the tilewise framework
# =============================================================================
#
# Paths, helpers, prep, and scene extraction. SE: n_eff = w_valid^2/sum_w2;
# SE_weighted = ndti_sd/sqrt(n_eff). na_frac = fraction of parcel masked (quality flag).
# Paths overridable via env (see Configuration).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(stringr)
  library(arrow)
  library(dplyr)
  library(exactextractr)
})
sf::sf_use_s2(FALSE)

terra::terraOptions(threads = max(1L, suppressWarnings(
  as.integer(Sys.getenv("NDTI_TERRA_THREADS", "8"))
)))

# --- Configuration ---
ndti_inventory   <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(ndti_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  ndti_inventory <- file.path(.root, "products", "inventory")
}
ndti_landiq_harmonized <- Sys.getenv("LANDIQ_HARMONIZED", "")
if (!nzchar(trimws(ndti_landiq_harmonized))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set LANDIQ_HARMONIZED or CCMMF_ROOT (source documentation/setup_env.sh).")
  ndti_landiq_harmonized <- file.path(.root, "LandIQ", "harmonized")
}
ndti_parcels_gpkg <- file.path(ndti_landiq_harmonized, "parcels-consolidated.gpkg")
ndti_ccmmf_root   <- Sys.getenv("CCMMF_ROOT", "")
if (!nzchar(trimws(ndti_ccmmf_root))) {
  stop("Set CCMMF_ROOT (source documentation/setup_env.sh).")
}
# HLS reflectance for NDTI: phenology tile/images tree (HLS_Phenology layout).
ndti_imagery_root <- {
  r <- trimws(Sys.getenv("HLS_IMAGERY_ROOT", ""))
  if (nzchar(r)) r else file.path(ndti_ccmmf_root, "HLS", "imagery")
}
if (!nzchar(trimws(ndti_imagery_root))) {
  stop("Set HLS_IMAGERY_ROOT (source documentation/setup_env.sh).")
}
ndti_out_root     <- file.path(ndti_inventory, "tillage/ndti_v4.1.2")

# --- Path helpers ---

# Intermediate per-tile CSV.gz files (one per tile per month)
path_tilepieces <- function(output_dir, year, month) {
  file.path(output_dir, sprintf("tilepieces_year=%d_month=%02d", year, month))
}

# Final combined output: one parquet per month inside year= directory.
# All monthly files form a Hive-partitioned dataset: arrow::open_dataset(ndti_out_root)
path_monthly_output <- function(output_dir, year, month) {
  file.path(output_dir, sprintf("ndti_year=%d_month=%02d.parquet", year, month))
}

# =============================================================================
# Small utilities
# =============================================================================

# Parse MGRS tile ID from HLS filename (e.g. "HLS.L30.T10SFF...." -> "10SFF")
extract_tile_id_from_filename <- function(filenames) {
  matched <- str_extract(filenames, "T[0-9A-Z]{5}\\.")
  result  <- sub("^T|\\.$", "", matched)
  result[is.na(matched)] <- NA_character_
  result
}

sanitize_tile_id_for_filename <- function(tile_id) gsub("[^0-9A-Za-z]+", "_", tile_id)

# HLS filenames carry a trailing dot on the tile ID ("10SFF."); strip for matching
normalize_tile_id <- function(x) sub("\\.+$", "", trimws(as.character(x)))

# Fmask bits: cloud=1 (2), shadow=3 (8), snow=4 (16).
# SpatRaster in/out so the bit test stays in terra (no values() dump to R).
is_fmask_bad_layer <- function(fmask) {
  bit_set <- function(r, bit_value) {
    (trunc(r / bit_value) %% 2) == 1
  }
  bit_set(fmask, 2) | bit_set(fmask, 8) | bit_set(fmask, 16)
}

# Append rows to a CSV, writing header only on the first write
append_to_csv <- function(data, filepath) {
  if (file.exists(filepath)) {
    fwrite(data, filepath, append = TRUE, col.names = FALSE)
  } else {
    fwrite(data, filepath)
  }
}

# =============================================================================
# Scene index
# =============================================================================

# Build a table of all HLS scenes (date x sensor x tile) for a given year-month.
# Phenology layout: <HLS_IMAGERY_ROOT>/<tile>/images/<scene_id>/*.tif
build_scene_index <- function(year, month, verbose = TRUE) {
  build_scene_index_phenology(year, month, verbose)
}

# Phenology workflow layout: <root>/<tile>/images/<scene_id>/*.tif (all bands + Fmask).
build_scene_index_phenology <- function(year, month, verbose = TRUE) {
  month_start <- as.Date(paste(year, month, 1, sep = "-"))
  month_end   <- seq(month_start, by = "month", length.out = 2L)[2L] - 1L
  root        <- ndti_imagery_root
  if (!dir.exists(root)) {
    if (verbose) message("[scene] imagery root missing: ", root)
    return(data.table(tile_id = character(), date = as.Date(character()),
                      sensor = character(), path = character()))
  }

  tile_ids <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  tile_ids <- tile_ids[grepl("^[0-9]", tile_ids)]

  sensor_specs <- list(
    HLSL = list(prefix = "HLS.L30.", band_file = "B06.tif"),
    HLSS = list(prefix = "HLS.S30.", band_file = "B11.tif")
  )

  rows <- list()
  for (tile in tile_ids) {
    img_dir <- file.path(root, tile, "images")
    if (!dir.exists(img_dir)) next
    for (sc_dir in list.dirs(img_dir, recursive = FALSE, full.names = TRUE)) {
      sc_name <- basename(sc_dir)
      for (sensor in names(sensor_specs)) {
        spec <- sensor_specs[[sensor]]
        if (!startsWith(sc_name, spec$prefix)) next
        doy_str <- str_extract(sc_name, "\\d{7}")
        if (is.na(doy_str)) next
        dt <- as.Date(doy_str, "%Y%j")
        if (is.na(dt) || dt < month_start || dt > month_end) next
        b1 <- file.path(sc_dir, paste0(sc_name, ".", spec$band_file))
        if (!file.exists(b1)) next
        rows[[length(rows) + 1L]] <- data.table(
          date = dt, sensor = sensor, tile_id = tile, path = b1
        )
      }
    }
  }

  out <- if (length(rows) == 0L) {
    data.table(tile_id = character(), date = as.Date(character()),
               sensor = character(), path = character())
  } else {
    rbindlist(rows)
  }
  if (verbose) message("[scene] ", nrow(out), " phenology scenes for ",
                     year, "-", sprintf("%02d", month))
  out
}

# --- Geometry loading ---
load_parcel_geometries <- function(parcel_ids) {
  parcel_ids <- unique(as.character(parcel_ids))
  layer_name <- st_layers(ndti_parcels_gpkg)$name[1]
  chunks <- split(parcel_ids, ceiling(seq_along(parcel_ids) / 5000L))
  out <- lapply(chunks, function(chunk) {
    ids_sql <- paste0("'", gsub("'", "''", chunk, fixed = TRUE), "'", collapse = ",")
    st_read(ndti_parcels_gpkg,
            query = sprintf('SELECT * FROM "%s" WHERE parcel_id IN (%s)', layer_name, ids_sql),
            quiet = TRUE)
  })
  parcels <- do.call(rbind, out)
  parcels <- st_zm(parcels, drop = TRUE, what = "ZM")
  parcels[!st_is_empty(st_geometry(parcels)), ]
}

load_parcel_tiles <- function(year, tile = NULL) {
  load_ndti_parcel_tiles(year, tile = tile)
}

# LandIQ years whose ag parcels to extract. Default: the scene year.
# Session 2 sets NDTI_PARCEL_YEARS=$PRIOR_YEAR,$TARGET_YEAR so Y+1 shoulder
# months still extract PRIOR-year parcels. Year-level extract sets this to
# the job year if unset.
ndti_parcel_years <- function(scene_year) {
  raw <- trimws(Sys.getenv("NDTI_PARCEL_YEARS", ""))
  if (!nzchar(raw)) {
    return(as.integer(scene_year))
  }
  ys <- unique(suppressWarnings(as.integer(unlist(strsplit(raw, "[, ]+")))))
  ys <- ys[!is.na(ys)]
  if (length(ys) == 0L) {
    as.integer(scene_year)
  } else {
    ys
  }
}

load_ndti_parcel_tiles <- function(year, tile = NULL) {
  yrs <- ndti_parcel_years(year)
  parts <- lapply(yrs, function(y) {
    tryCatch(
      load_parcel_tiles_for_year(y, tile = tile),
      error = function(e) {
        message("[prep] skip parcel year=", y, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) {
    stop(
      "No agricultural parcels for NDTI (scene year=", as.integer(year),
      " parcel years=", paste(yrs, collapse = ","), ")"
    )
  }
  out <- unique(rbindlist(parts, use.names = TRUE, fill = TRUE),
                by = c("parcel_id", "tile_id"))
  out
}

# --- Prep: geometry parcel_tiles.csv filtered to year ag parcels ---
ndti_prep_static_tilewise <- function(year, overwrite = FALSE, verbose = TRUE, tile = NULL) {
  yr <- as.integer(year)
  output_dir <- file.path(ndti_out_root, paste0("year=", yr))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(tile) || !nzchar(trimws(as.character(tile)[1L]))) {
    env_tile <- trimws(Sys.getenv("TILEWISE_ONE_TILE", ""))
    if (nzchar(env_tile) && !tolower(env_tile) %in% c("1", "true", "yes", "y", "first")) {
      tile <- env_tile
    }
  }
  ag <- load_ndti_parcel_tiles(yr, tile = tile)
  tile_to_parcel_ids <- ag_tiles_to_tile_parcel_list(ag)

  if (isTRUE(verbose)) {
    keep_tile <- if (is.null(tile) || length(tile) == 0L) "" else trimws(as.character(tile)[1L])
    if (is.na(keep_tile)) keep_tile <- ""
    py <- ndti_parcel_years(yr)
    message(
      "[prep] parcel_tiles x ag scene_year=", yr,
      " parcel_years=", paste(py, collapse = ","),
      if (nzchar(keep_tile)) paste0(" tile=", keep_tile) else "",
      " parcels=", uniqueN(ag$parcel_id),
      " tiles=", length(tile_to_parcel_ids)
    )
  }

  prep <- list(
    year               = yr,
    out_dir            = output_dir,
    tile_to_parcel_ids = tile_to_parcel_ids,
    imagery_root       = ndti_imagery_root,
    band_other         = list(HLSL = c("B06", "B07"), HLSS = c("B11", "B12"))
  )

  # polys for tilewise_core; geometry loaded lazily per tile in prepare_tile()
  tile_ids <- names(prep$tile_to_parcel_ids)
  parcel_tile <- rbindlist(lapply(tile_ids, function(tid) {
    ids <- as.character(prep$tile_to_parcel_ids[[tid]])
    if (length(ids) == 0) return(NULL)
    data.table(parcel_id = ids, tile_id = tid)
  }), use.names = TRUE, fill = TRUE)

  if (is.null(parcel_tile) || nrow(parcel_tile) == 0) {
    prep$polys <- data.table(parcel_id = character(), tile_ids = list())
  } else {
    prep$polys <- parcel_tile[, .(tile_ids = list(sort(unique(tile_id)))), by = parcel_id]
  }
  prep
}

# --- Scene extraction ---
# parcels_sf must be reprojected to raster CRS.
extract_ndti_scene <- function(band1_path, band2_path, fmask_path,
                               parcels_sf, parcel_ids, scene_date) {
  on.exit({
    terra::tmpFiles(orphan = TRUE, remove = TRUE)
    invisible(gc(verbose = FALSE))
  }, add = TRUE)
  bbox_vect <- terra::vect(sf::st_as_sfc(sf::st_bbox(parcels_sf)))
  band1 <- try(terra::crop(terra::rast(band1_path), bbox_vect), silent = TRUE)
  band2 <- try(terra::crop(terra::rast(band2_path), bbox_vect), silent = TRUE)
  fmask <- try(terra::crop(terra::rast(fmask_path), bbox_vect), silent = TRUE)
  if (inherits(band1, "try-error") || inherits(band2, "try-error") || inherits(fmask, "try-error")) {
    return(NULL)
  }

  ndti <- (band1 - band2) / (band1 + band2)
  ndti <- terra::ifel(is_fmask_bad_layer(fmask), NA, ndti)
  names(ndti) <- "NDTI"
  # Drop intermediates before exact_extract so the scene does not keep 5 rasters.
  rm(band1, band2, fmask)
  terra::tmpFiles(orphan = TRUE, remove = TRUE)

  summarize_poly <- function(values, cov_fracs) {
    pos     <- cov_fracs > 0
    ok      <- !is.na(values) & pos
    w_total <- sum(cov_fracs[pos])
    w_valid <- sum(cov_fracs[ok])
    if (!any(ok)) {
      return(as.data.frame(list(ndti_mean = NA_real_, ndti_sd = NA_real_,
                                n_valid = 0L, w_valid = 0, sum_w2 = 0, na_frac = NA_real_)))
    }
    v  <- values[ok]
    w  <- cov_fracs[ok]
    mu <- sum(w * v) / w_valid
    as.data.frame(list(
      ndti_mean = mu,
      ndti_sd   = sqrt(sum(w * (v - mu)^2) / w_valid),
      n_valid   = sum(ok),
      w_valid   = w_valid,
      sum_w2    = sum(w^2),   # n_eff = w_valid^2 / sum_w2  ->  SE = ndti_sd / sqrt(n_eff)
      na_frac   = if (w_total > 0) 1 - w_valid / w_total else NA_real_
    ))
  }

  result <- try(
    exactextractr::exact_extract(ndti, parcels_sf, fun = summarize_poly,
                                 progress = FALSE),
    silent = TRUE
  )
  if (inherits(result, "try-error") || is.null(result)) return(NULL)

  rm(ndti)
  terra::tmpFiles(orphan = TRUE, remove = TRUE)
  invisible(gc(verbose = FALSE))

  dt <- as.data.table(result)
  # Assign parcel_id manually - exactextractr preserves row order matching parcels_sf.
  dt[, parcel_id := parcels_sf$parcel_id]
  dt[, date := scene_date]
  dt
}

# --- Product object ---
product_ndti <- function() {
  list(
    prep_static          = ndti_prep_static_tilewise,
    scene_index          = build_scene_index,
    scene_index_tile_col = "tile_id",

    # Load parcel geometry from GPKG for just this tile's parcels, reprojected
    # to the raster CRS. Called once per tile before the scene loop.
    prepare_tile = function(prep, tile_id, parcel_ids, scenes_this_tile) {
      parcels_sf <- load_parcel_geometries(as.character(parcel_ids))
      if (nrow(parcels_sf) == 0) return(NULL)
      # Some scene files can be corrupt/truncated; choose the first readable
      # scene to determine CRS so one bad file doesn't fail the whole tile.
      raster_crs <- NULL
      for (p in scenes_this_tile$path) {
        r_try <- try(terra::rast(p), silent = TRUE)
        if (inherits(r_try, "try-error")) next
        c_try <- try(sf::st_crs(terra::crs(r_try)), silent = TRUE)
        if (inherits(c_try, "try-error") || is.null(c_try) || is.na(c_try)) next
        raster_crs <- c_try
        break
      }
      if (is.null(raster_crs)) return(NULL)
      sf::st_transform(parcels_sf, raster_crs)
    },

    # Extract NDTI stats for one HLS scene.
    # tile_parcels is the sf object returned by prepare_tile (already reprojected).
    process_scene = function(prep, scene_row, tile_parcels, tile_id) {
      sensor <- scene_row$sensor[1]
      b1     <- scene_row$path[1]
      bands  <- list(HLSL = c("B06", "B07"), HLSS = c("B11", "B12"))[[sensor]]
      b2     <- sub(bands[1], bands[2], b1, fixed = TRUE)
      fmask  <- sub(paste0(bands[1], "\\.tif$"), "Fmask.tif", b1)
      if (!file.exists(b2) || !file.exists(fmask)) return(NULL)
      extract_ndti_scene(b1, b2, fmask, tile_parcels, tile_parcels$parcel_id,
                           scene_row$date[1])
    },

    # Reads all tilepieces, aggregates across tiles, writes Parquet.
    combine = function(prep, time_key, overwrite = FALSE, verbose = TRUE) {
      ndti_combine(prep, time_key, overwrite = overwrite, verbose = verbose)
    },

    path_tilepieces    = path_tilepieces,
    path_final_output  = path_monthly_output,
    path_combine_parts = function(out_dir, year, time_key) NULL,

    validate_tilepiece = function(dt) {
      all(c("parcel_id", "date", "ndti_mean", "ndti_sd",
            "n_valid", "w_valid", "sum_w2", "na_frac") %in% names(dt))
    },

    empty_tilepiece_schema = function() {
      data.table(
        parcel_id = character(),
        date      = as.Date(integer(0), origin = "1970-01-01"),
        ndti_mean = double(), ndti_sd = double(),
        n_valid   = integer(), w_valid = double(),
        sum_w2    = double(), na_frac = double()
      )
    },
    empty_part_schema = function(year) {
      data.table(
        parcel_id = character(), year = integer(),
        date      = as.Date(integer(0), origin = "1970-01-01"),
        ndti_mean = double(), ndti_sd = double(),
        n_valid   = integer(), w_valid = double(),
        sum_w2    = double(), na_frac = double()
      )
    }
  )
}
