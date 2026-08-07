# =============================================================================
# tilewise_ndti_implementation.R — NDTI product for the tilewise framework
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
ndti_management   <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(ndti_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  ndti_management <- file.path(.root, "management")
}
ndti_landiq_v4    <- Sys.getenv("CCMMF_LANDIQ_V4", "")
if (!nzchar(trimws(ndti_landiq_v4))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) stop("Set CCMMF_LANDIQ_V4 or CCMMF_ROOT (source documentation/setup_env.sh).")
  ndti_landiq_v4 <- file.path(.root, "LandIQ-harmonized-v4.1")
}
ndti_parcels_gpkg <- file.path(ndti_landiq_v4, "parcels-consolidated.gpkg")
ndti_crops_parq   <- file.path(ndti_landiq_v4, "crops_all_years.parq")
ndti_cropcode_csv <- file.path(ndti_management, "LandIQ_cropCode_lookup_table.csv")
ndti_ccmmf_root   <- Sys.getenv("CCMMF_ROOT", "")
if (!nzchar(trimws(ndti_ccmmf_root))) {
  stop("Set CCMMF_ROOT (source documentation/setup_env.sh).")
}
# HLS reflectance for NDTI — phenology workflow layout under data_phen
# (see phenology/extract/README.md — upstream HLS_Phenology repo).
ndti_imagery_layout <- tolower(Sys.getenv("HLS_IMAGERY_LAYOUT", "phenology"))
ndti_imagery_root   <- Sys.getenv(
  "HLS_IMAGERY_ROOT",
  file.path(ndti_ccmmf_root, "data_phen/HLS_data_sort/HLS30")
)
ndti_hlsl_base <- Sys.getenv("HLSL_BASE", "")
ndti_hlss_base <- Sys.getenv("HLSS_BASE", "")
if (!nzchar(trimws(ndti_hlsl_base)) || !nzchar(trimws(ndti_hlss_base))) {
  stop("Set HLSL_BASE and HLSS_BASE (no lab default).")
}
ndti_out_root     <- file.path(ndti_management, "tillage/ndti_v4.1")
ndti_parcel_tilemap <- Sys.getenv(
  "NDTI_PARCEL_TILEMAP",
  file.path(ndti_management, "hls_parcel_tile_map_v4.1.rds")
)

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

# Fmask bit flags: cloud=bit1, cloud shadow=bit3, snow=bit4
is_fmask_bad_pixel <- function(fmask_values) {
  (bitwAnd(fmask_values, 2L)  != 0) |
  (bitwAnd(fmask_values, 8L)  != 0) |
  (bitwAnd(fmask_values, 16L) != 0)
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
build_scene_index <- function(year, month, verbose = TRUE) {
  if (identical(ndti_imagery_layout, "phenology")) {
    build_scene_index_phenology(year, month, verbose)
  } else {
    build_scene_index_flat(year, month, verbose)
  }
}

# Flat year directories: HLSL/<year>/*.B06.tif, etc. (used when HLS_IMAGERY_LAYOUT=flat).
build_scene_index_flat <- function(year, month, verbose = TRUE) {
  month_start <- as.Date(paste(year, month, 1, sep = "-"))
  month_end   <- seq(month_start, by = "month", length.out = 2L)[2L] - 1L

  list_scenes <- function(dir_path, band_pattern, sensor_name) {
    if (!dir.exists(dir_path)) return(data.table())
    files   <- list.files(dir_path, pattern = band_pattern, full.names = TRUE)
    if (length(files) == 0) return(data.table())
    doy_str <- str_extract(basename(files), "\\d{7}")
    dates   <- as.Date(doy_str, "%Y%j")
    keep    <- !is.na(dates) & dates >= month_start & dates <= month_end
    if (!any(keep)) return(data.table())
    data.table(
      date    = dates[keep],
      sensor  = sensor_name,
      tile_id = extract_tile_id_from_filename(basename(files)[keep]),
      path    = files[keep]
    )
  }

  scenes <- rbindlist(list(
    list_scenes(file.path(ndti_hlsl_base, year), ".*B06.*\\.tif$", "HLSL"),
    list_scenes(file.path(ndti_hlss_base, year), ".*B11.*\\.tif$", "HLSS")
  ))
  scenes <- scenes[!is.na(tile_id)]
  scenes[, tile_id := normalize_tile_id(tile_id)]
  if (verbose) message("[scene] ", nrow(scenes), " scenes for ", year, "-", sprintf("%02d", month))
  scenes
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

load_parcel_tilemap <- function(year = NULL) {
  if (is.null(year)) {
    return(read_parcel_tilemap(ndti_parcel_tilemap))
  }
  load_parcel_tilemap_for_year(year, ndti_parcel_tilemap)
}

# --- Static prep (cached per year) ---
ndti_prep_static_tilewise <- function(year, overwrite = FALSE, verbose = TRUE) {
  yr         <- as.integer(year)
  output_dir <- file.path(ndti_out_root, paste0("year=", yr))
  cache_path <- file.path(output_dir, sprintf("ndti_prep_static_year=%d.rds", yr))

  if (file.exists(cache_path) && !overwrite) {
    if (verbose) message("[prep] using cache: ", cache_path)
    prep <- readRDS(cache_path)
    if (!is.null(prep$polys)) return(prep)
  } else {
    lookup   <- fread(ndti_cropcode_csv)
    ag_pairs <- unique(lookup[is_agricultural == TRUE,
                              .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS))])
    ag_classes_filter <- unique(ag_pairs$CLASS)

    pq_result <- arrow::open_dataset(ndti_crops_parq) |>
      dplyr::filter(year == !!yr, CLASS %in% !!ag_classes_filter) |>
      dplyr::select(parcel_id, CLASS, SUBCLASS) |>
      dplyr::collect()
    ca_crop <- as.data.table(pq_result)
    ca_crop[, CLASS    := trimws(as.character(CLASS))]
    ca_crop[, SUBCLASS := as.character(SUBCLASS)]
    ca_crop    <- merge(ca_crop, ag_pairs, by = c("CLASS", "SUBCLASS"))
    parcel_ids <- unique(as.character(ca_crop$parcel_id))
    if (length(parcel_ids) == 0) stop("No agricultural parcel_ids found for year ", yr)
    if (verbose) message("[prep] agricultural parcels: ", length(parcel_ids))

    tilemap <- load_parcel_tilemap(yr)
    if (is.null(tilemap)) stop(
      "Parcel-tile map not found: ", ndti_parcel_tilemap, "\n",
      "Build it: Rscript scripts/hls/build_hls_parcel_tile_map.R overwrite"
    )

    # tile_to_parcel_ids: kept for reference; polys drives tilewise_core
    tile_to_parcel_ids <- local({
      pid     <- unique(as.character(parcel_ids))
      map_sub <- tilemap[pid, on = "parcel_id", nomatch = 0]
      map_sub <- unique(map_sub, by = "parcel_id")
      out <- list()
      for (i in seq_len(nrow(map_sub))) {
        for (tile in strsplit(map_sub$tileIDs[i], ",", fixed = TRUE)[[1]]) {
          if (nzchar(tile)) out[[tile]] <- c(out[[tile]], map_sub$parcel_id[i])
        }
      }
      out
    })

    covered   <- unique(unlist(tile_to_parcel_ids, use.names = FALSE))
    n_missing <- length(setdiff(parcel_ids, covered))
    if (n_missing > 0)
      message("[prep] ", n_missing, " parcels missing from tile map (dropped) — rebuild map if large")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    prep <- list(
      year               = yr,
      out_dir            = output_dir,
      tile_to_parcel_ids = tile_to_parcel_ids,
      imagery_layout     = ndti_imagery_layout,
      imagery_root       = if (identical(ndti_imagery_layout, "phenology")) ndti_imagery_root else NULL,
      base_dirs          = list(HLSL = ndti_hlsl_base, HLSS = ndti_hlss_base),
      band_other         = list(HLSL = c("B06", "B07"), HLSS = c("B11", "B12")),
      fmask_suffix       = list(HLSL = "B06.tif$",     HLSS = "B11.tif$")
    )
    saveRDS(prep, cache_path)
    if (verbose) message("[prep] saved: ", cache_path)
  }

  # Build polys data.table (parcel_id + tile_ids list-column) for tilewise_core.
  # Geometry is NOT loaded here; it is loaded lazily per-tile in prepare_tile().
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
  bbox_vect <- terra::vect(sf::st_as_sfc(sf::st_bbox(parcels_sf)))
  band1 <- try(terra::crop(terra::rast(band1_path), bbox_vect), silent = TRUE)
  band2 <- try(terra::crop(terra::rast(band2_path), bbox_vect), silent = TRUE)
  fmask <- try(terra::crop(terra::rast(fmask_path), bbox_vect), silent = TRUE)
  if (inherits(band1, "try-error") || inherits(band2, "try-error") || inherits(fmask, "try-error")) {
    return(NULL)
  }

  ndti <- (band1 - band2) / (band1 + band2)
  ndti[is_fmask_bad_pixel(terra::values(fmask))] <- NA
  names(ndti) <- "NDTI"

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

  dt <- as.data.table(result)
  # Assign parcel_id manually — exactextractr preserves row order matching parcels_sf.
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
      layout <- if (!is.null(prep$imagery_layout)) prep$imagery_layout else ndti_imagery_layout
      fmask  <- if (identical(layout, "phenology")) {
        sub(paste0(bands[1], "\\.tif$"), "Fmask.tif", b1)
      } else {
        yr_str <- stringr::str_extract(basename(b1), "\\d{4}")
        file.path(
          prep$base_dirs[[sensor]], paste0(yr_str, "_Fmask"),
          sub(prep$fmask_suffix[[sensor]], "Fmask.tif", basename(b1))
        )
      }
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
