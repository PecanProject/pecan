# MSLSP product definition for the tilewise framework (prep, scenes, extract, combine).
# Sourced after extract_summary_core.R and combine_mslsp_tilepieces.R by the MSLSP driver.
#
# Main inputs: env paths for LandIQ, MSLSP NetCDF roots, parcel-tile map RDS.
# Main outputs: tilepieces and annual Parquet under phenology/raw_mslsp_v4.1.
# How to run: sourced only; use tilewise_mslsp_driver.R for CLI.
# Workflow: monitoring workflow MSLSP branch.

#### Configuration
mslsp_landiq_v4    <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
mslsp_management   <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
mslsp_parcels_gpkg <- file.path(mslsp_landiq_v4, "parcels-consolidated.gpkg")
mslsp_crops_parq   <- file.path(mslsp_landiq_v4, "crops_all_years.parq")
mslsp_cropcode_lookup <- file.path(mslsp_management, "LandIQ_cropCode_lookup_table.csv")
mslsp_legacy_dir   <- Sys.getenv("mslsp_legacy_dir", "/projectnb/dietzelab/ccmmf/HLS_data")
mslsp_new_base     <- Sys.getenv("mslsp_new_base", "/projectnb/dietzelab/ccmmf/data_phen/output")
mslsp_out_root     <- file.path(mslsp_management, "phenology/raw_mslsp_v4.1")
mslsp_parcel_tilemap <- Sys.getenv(
  "mslsp_parcel_tilemap",
  file.path(mslsp_management, "hls_parcel_tile_map_v4.1_years=2016-2024.rds")
)

mslsp_metrics   <- c("OGI", "50PCGI", "OGMx", "Peak", "OGD", "50PCGD", "OGMn",
                     "EVImax", "EVIamp", "EVIarea")
mslsp_qa_cat    <- c("gupQA", "gdownQA")
mslsp_year_fields_mode <- c("NumCycles")
mslsp_year_fields_mean <- c("numObs")

mslsp_state <- new.env(parent = emptyenv())

#### Scene index helpers

mslsp_nc_path <- function(tile_id, year) {
  yr  <- as.integer(year)
  candidates <- c(
    file.path(mslsp_legacy_dir, paste0("MSLSP_", tile_id, "_", yr, ".nc")),
    file.path(mslsp_new_base, tile_id, "phenoMetrics", paste0("MSLSP_", tile_id, "_", yr, ".nc"))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

mslsp_load_parcel_tilemap <- function(year) {
  if (!file.exists(mslsp_parcel_tilemap)) return(NULL)
  dt <- as.data.table(readRDS(mslsp_parcel_tilemap))
  if (!all(c("parcel_id", "tileIDs", "year") %in% names(dt))) return(NULL)
  yr <- as.integer(year)
  dt <- dt[as.integer(year) == yr, .(parcel_id = as.character(parcel_id), tileIDs)]
  setkey(dt, parcel_id)
  dt
}

mslsp_scene_index <- function(year, time_key, verbose = TRUE) {
  yr  <- as.integer(year)
  key <- as.character(yr)
  tiles <- if (exists(key, envir = mslsp_state, inherits = FALSE)) {
    get(key, envir = mslsp_state, inherits = FALSE)
  } else {
    tilemap <- mslsp_load_parcel_tilemap(yr)
    if (is.null(tilemap) || nrow(tilemap) == 0) character() else {
      sort(unique(unlist(strsplit(tilemap$tileIDs, ",", fixed = TRUE), use.names = FALSE)))
    }
  }
  if (length(tiles) == 0) return(data.table(tile_id = character(), path = character()))
  paths <- vapply(tiles, mslsp_nc_path, character(1), year = yr)
  ok    <- !is.na(paths) & file.exists(paths)
  out   <- data.table(tile_id = as.character(tiles[ok]), path = paths[ok])
  if (verbose) {
    missing_tiles <- as.character(tiles[!ok])
    if (length(missing_tiles) > 0) {
      sample_tiles <- paste(head(sort(unique(missing_tiles)), 12), collapse = ",")
      if (length(unique(missing_tiles)) > 12) sample_tiles <- paste0(sample_tiles, ",...")
      msg <- paste0("[MSLSP] scene_index year=", yr,
                    " missing_tile_nc=", length(unique(missing_tiles)),
                    " sample=", sample_tiles)
      if (exists("tw_log", mode = "function")) {
        tw_log("WARN", msg)
      } else {
        message(msg)
      }
    }
    if (nrow(out) > 0) message("[MSLSP] scene_index year=", yr, " tiles=", nrow(out))
  }
  out
}

#### Static prep
# PFT from lookup (CLASS, SUBCLASS -> PFT) so subclass-level differences (e.g. T19 vs T28 woody) are correct.
mslsp_prep_static_tilewise <- function(year, ...) {
  yr <- as.integer(year)
  sf::sf_use_s2(FALSE)

  lookup   <- data.table::fread(mslsp_cropcode_lookup)
  ag_pairs <- lookup[is_agricultural == TRUE,
                     .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS), PFT)]

  # Read once from the single parquet file and filter in-memory.
  # This avoids Arrow dataset URI registration issues seen on SCC for some jobs.
  pq_result <- as.data.table(arrow::read_parquet(
    mslsp_crops_parq,
    col_select = c("parcel_id", "CLASS", "SUBCLASS", "year")
  ))
  ca_crop <- pq_result[as.integer(year) == yr, .(parcel_id, CLASS, SUBCLASS)]
  ca_crop[, CLASS    := trimws(as.character(CLASS))]
  ca_crop[, SUBCLASS := as.character(SUBCLASS)]
  # Inner join: filters to agricultural only AND attaches PFT
  ca_crop <- merge(ca_crop, ag_pairs, by = c("CLASS", "SUBCLASS"))
  crop_ids <- unique(as.character(ca_crop$parcel_id))
  if (length(crop_ids) == 0) stop("No agricultural parcel_ids found for year ", yr)

  tilemap <- mslsp_load_parcel_tilemap(yr)
  if (is.null(tilemap)) {
    stop(
      "Parcel-tile map not found for MSLSP.\n",
      "  Path: ", mslsp_parcel_tilemap, "\n",
      "  Build it: Rscript scripts/hls/build_hls_parcel_tile_map.R 2016 2024 overwrite"
    )
  }
  tile_sub <- tilemap[crop_ids, on = "parcel_id", nomatch = 0]
  missing_map <- setdiff(crop_ids, tile_sub$parcel_id)
  if (length(missing_map) > 0) {
    message("[MSLSP prep] parcel_ids missing from tile map (dropped): ", length(missing_map))
    crop_ids <- intersect(crop_ids, tile_sub$parcel_id)
    tile_sub <- tilemap[crop_ids, on = "parcel_id", nomatch = 0]
  }

  # Load geometries from v4.1 GPKG in batches to avoid huge SQL IN clauses.
  layer      <- sf::st_layers(mslsp_parcels_gpkg)$name[1]
  ids        <- unique(tile_sub$parcel_id)
  chunks     <- split(ids, ceiling(seq_along(ids) / 5000L))
  geom_chunks <- lapply(chunks, function(x) {
    esc <- gsub("'", "''", x, fixed = TRUE)
    q   <- sprintf('SELECT * FROM "%s" WHERE parcel_id IN (%s)',
                   layer, paste0("'", esc, "'", collapse = ","))
    sf::st_read(mslsp_parcels_gpkg, query = q, quiet = TRUE)
  })
  polys <- do.call(rbind, geom_chunks)
  polys$parcel_id <- as.character(polys$parcel_id)
  polys <- polys[!sf::st_is_empty(sf::st_geometry(polys)), ]

  missing_geom <- setdiff(ids, polys$parcel_id)
  if (length(missing_geom) > 0) {
    message("[MSLSP prep] parcel_ids missing geometry (dropped): ", length(missing_geom))
    tile_sub <- tile_sub[!parcel_id %in% missing_geom]
  }
  tile_sub <- tile_sub[polys$parcel_id, on = "parcel_id", nomatch = 0]

  # Attach tile_ids list column so tilewise_core can build the tile->parcel index.
  polys$tile_ids <- strsplit(tile_sub$tileIDs, ",", fixed = TRUE)
  active_tiles   <- sort(unique(unlist(polys$tile_ids, use.names = FALSE)))
  mslsp_state[[as.character(yr)]] <- active_tiles

  out_dir <- file.path(mslsp_out_root, paste0("year=", yr))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  list(year = yr, polys = polys, out_dir = out_dir, active_tiles = active_tiles)
}

#### Scene extraction
mslsp_process_scene_tilewise <- function(prep, scene_row, parcels_this_tile, tile_id) {
  log_skip <- function(level = "WARN", reason, extra = "") {
    msg <- paste0("[MSLSP extract] tile=", tile_id,
                  " reason=", reason,
                  if (nzchar(extra)) paste0(" ", extra) else "")
    if (exists("tw_log", mode = "function")) {
      tw_log(level, msg)
    } else {
      message(msg)
    }
  }

  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("exactextractr is required for MSLSP extraction. Install with: install.packages('exactextractr')")
  }
  path <- scene_row$path[1]
  if (!file.exists(path)) {
    log_skip("WARN", "missing_scene_file", paste0("path=", path))
    return(NULL)
  }
  yr <- prep$year

  r_all <- try(terra::rast(path), silent = TRUE)
  if (inherits(r_all, "try-error")) {
    log_skip("WARN", "unreadable_scene_raster", paste0("path=", path))
    return(NULL)
  }

  # Identify which layers are present in this NetCDF.
  nms     <- names(r_all)
  qa1     <- intersect(mslsp_qa_cat, nms)
  qa2     <- intersect(paste0(mslsp_qa_cat, "_2"), nms)
  yr_mode <- intersect(mslsp_year_fields_mode, nms)   # e.g. NumCycles
  yr_mean <- intersect(mslsp_year_fields_mean, nms)   # e.g. numObs

  cycle1_layers <- intersect(c(mslsp_metrics, qa1, yr_mode, yr_mean), nms)
  cycle2_layers <- intersect(c(paste0(mslsp_metrics, "_2"), qa2), nms)
  if (length(cycle1_layers) == 0 && length(cycle2_layers) == 0) {
    log_skip("WARN", "no_expected_layers_in_scene", paste0("path=", path))
    return(NULL)
  }

  # Reproject parcels to the raster CRS and crop to their bounding box.
  # Both cycles share this single crop of the full-tile raster.
  target_crs <- terra::crs(r_all)
  parcels_tr <- sf::st_transform(parcels_this_tile, target_crs)
  bbox_vect  <- terra::vect(sf::st_as_sfc(sf::st_bbox(parcels_tr)))
  r_crop     <- try(terra::crop(r_all, bbox_vect), silent = TRUE)
  if (inherits(r_crop, "try-error")) {
    log_skip("WARN", "crop_extent_no_overlap", paste0("path=", path))
    return(NULL)
  }

  pid_vec <- as.character(parcels_tr$parcel_id)

  # Build an exactextractr per-polygon callback for a given set of layers.
  #
  # exactextractr calls this once per polygon with:
  #   values    - data.frame with one column per raster layer, one row per pixel
  #   cov_fracs - numeric vector of pixel coverage fractions (0-1)
  #
  # Arguments:
  #   metric_cols - layer names aggregated by weighted mean + SD
  #   mode_cols   - layer names aggregated by weighted mode + mode_frac
  #   key_col     - single layer used to define "valid" pixels:
  #                 a pixel is valid if it overlaps the polygon and key_col is not NA
  #   col_rename  - function applied to a layer name before naming the output column;
  #                 for cycle 2 this strips "_2" so both cycles share the same schema
  #                 (e.g. "OGI_2" becomes "OGI", output is "OGI_mean" not "OGI_2_mean")
  make_summarize_poly <- function(metric_cols, mode_cols, key_col, col_rename = identity) {
    function(values, cov_fracs) {
      pos         <- cov_fracs > 0
      ok          <- pos & !is.na(values[[key_col]])
      w_total     <- sum(cov_fracs[pos])
      w_valid_key <- sum(cov_fracs[ok])

      out <- list(
        n_valid = sum(ok),
        w_valid = w_valid_key,
        sum_w2  = sum(cov_fracs[ok]^2),   # for n_eff = w_valid^2 / sum_w2 -> SE = sd / sqrt(n_eff)
        na_frac = if (w_total > 0) 1 - w_valid_key / w_total else NA_real_
      )

      for (col in metric_cols) {
        s    <- weighted_stats(values[[col]], cov_fracs)
        base <- col_rename(col)
        out[[paste0(base, "_mean")]] <- s$mean
        out[[paste0(base, "_sd")]]   <- s$sd
      }

      for (col in mode_cols) {
        m    <- weighted_mode_stats(values[[col]], cov_fracs)
        base <- col_rename(col)
        out[[paste0(base, "_mode")]]      <- m$mode
        out[[paste0(base, "_mode_frac")]] <- m$mode_frac
      }

      # Return a 1-row data.frame so exactextractr combines as one row per polygon
      # (returning a raw list causes wrong combination: one row per list element).
      # check.names=FALSE is required: layer names like "50PCGI_mean" start with a digit
      # and would otherwise be silently renamed to "X50PCGI_mean" by as.data.frame().
      as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }

  out_all <- list()

  # --- Cycle 1 ---
  if (length(cycle1_layers) > 0) {
    metric1 <- intersect(c(mslsp_metrics, yr_mean), cycle1_layers)
    mode1   <- intersect(c(qa1, yr_mode), cycle1_layers)
    key1    <- if (length(metric1) > 0) metric1[1] else mode1[1]
    cb1     <- make_summarize_poly(metric1, mode1, key1)
    res1    <- try(
      exactextractr::exact_extract(r_crop[[cycle1_layers]], parcels_tr, fun = cb1, progress = FALSE),
      silent = TRUE
    )
    if (inherits(res1, "try-error")) {
      log_skip("ERROR", "exact_extract_cycle1_failed", paste0("path=", path))
    }
    if (!inherits(res1, "try-error") && !is.null(res1)) {
      dt1 <- as.data.table(res1)
      dt1[, c("parcel_id", "year", "cycle") := list(pid_vec, yr, 1L)]
      out_all[[length(out_all) + 1]] <- dt1
    }
  }

  # --- Cycle 2 ---
  # col_rename strips the trailing "_2" from layer names so output columns match the
  # cycle 1 schema. e.g. "OGI_2" -> "OGI_mean", "gupQA_2" -> "gupQA_mode".
  # The `cycle` column then distinguishes the two.
  if (length(cycle2_layers) > 0) {
    strip2  <- function(x) sub("_2$", "", x)
    metric2 <- intersect(paste0(mslsp_metrics, "_2"), cycle2_layers)
    mode2   <- intersect(qa2, cycle2_layers)
    key2    <- if (length(metric2) > 0) metric2[1] else mode2[1]
    cb2     <- make_summarize_poly(metric2, mode2, key2, col_rename = strip2)
    res2    <- try(
      exactextractr::exact_extract(r_crop[[cycle2_layers]], parcels_tr, fun = cb2, progress = FALSE),
      silent = TRUE
    )
    if (inherits(res2, "try-error")) {
      log_skip("ERROR", "exact_extract_cycle2_failed", paste0("path=", path))
    }
    if (!inherits(res2, "try-error") && !is.null(res2)) {
      dt2 <- as.data.table(res2)
      dt2[, c("parcel_id", "year", "cycle") := list(pid_vec, yr, 2L)]
      out_all[[length(out_all) + 1]] <- dt2
    }
  }

  if (length(out_all) == 0) {
    log_skip("WARN", "no_cycle_outputs_written", paste0("path=", path))
    return(NULL)
  }
  out <- rbindlist(out_all, fill = TRUE)
  out[, parcel_id := as.character(parcel_id)]

  # Harmonize EVI scaling across vintages of MSLSP NetCDF files.
  #
  # Why this is needed:
  # - 2018-2019 files (legacy path) encode EVI scaling with NetCDF attributes
  #   scale_factor/add_offset, which terra typically applies on read.
  # - 2020+ files (new path) often encode equivalent information as scale/offset.
  #   In this workflow, those can come through as raw integer-like values
  #   (~0..10000 for EVImax/EVIamp, ~0..32766 for EVIarea).
  #
  # Without harmonization, different years end up on different numeric scales
  # and downstream matching/QC behaves inconsistently.
  #
  # Strategy:
  # - Detect raw-like magnitude in extracted parcel summaries.
  # - Apply scaling exactly once only when needed.
  maybe_scale_evi <- function(dt) {
    evi_cols <- intersect(c("EVImax_mean", "EVIamp_mean", "EVIarea_mean"), names(dt))
    if (length(evi_cols) == 0) return(dt)
    med_abs <- vapply(evi_cols, function(cc) {
      v <- suppressWarnings(as.numeric(dt[[cc]]))
      stats::median(abs(v), na.rm = TRUE)
    }, numeric(1))
    # Physical EVI values should be O(1), not O(1e3-1e4).
    needs_scale <- any(is.finite(med_abs[c("EVImax_mean", "EVIamp_mean")]) &
                         med_abs[c("EVImax_mean", "EVIamp_mean")] > 10, na.rm = TRUE)
    if (!isTRUE(needs_scale)) return(dt)

    scale_f <- c(EVImax = 1e-4, EVIamp = 1e-4, EVIarea = 1e-2)
    for (nm in names(scale_f)) {
      mcol <- paste0(nm, "_mean")
      scol <- paste0(nm, "_sd")
      if (mcol %in% names(dt)) dt[[mcol]] <- suppressWarnings(as.numeric(dt[[mcol]])) * scale_f[[nm]]
      if (scol %in% names(dt)) dt[[scol]] <- suppressWarnings(as.numeric(dt[[scol]])) * scale_f[[nm]]
    }
    if (exists("tw_log", mode = "function")) {
      tw_log("WARN", "[MSLSP extract] tile=", tile_id, " applied EVI scale harmonization (raw-like magnitude detected)")
    }
    dt
  }
  out <- maybe_scale_evi(out)

  out[]
}

# =============================================================================
# Combine helpers (used by combine_mslsp_tilepieces.R and the bucket fallback)
# =============================================================================

# Core aggregation logic, shared by mslsp_combine and the bucket fallback.
# Aggregates by (parcel_id, year, cycle) using area-weighted statistics.
mslsp_aggregate_tilepieces <- function(dt, year) {
  key <- c("parcel_id", "year", "cycle")
  if (!all(key %in% names(dt))) return(dt)
  dt[, year := as.integer(year)]

  safe_weighted_mean <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(w[ok] * x[ok]) / sum(w[ok])
  }
  safe_weighted_sd <- function(mu, sd, w) {
    ok <- is.finite(mu) & is.finite(sd) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    ww <- w[ok]; mm <- mu[ok]; ss <- sd[ok]
    m_all   <- sum(ww * mm) / sum(ww)
    var_all <- sum(ww * (ss^2 + mm^2)) / sum(ww) - m_all^2
    sqrt(pmax(0, var_all))
  }
  weighted_mode <- function(x, w) {
    ok <- !is.na(x) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    dd <- data.table(x = x[ok], w = w[ok])[, .(w = sum(w)), by = x][order(-w)]
    as.numeric(dd$x[1])
  }
  weighted_mode_frac <- function(x, mode_val, w) {
    ok <- !is.na(x) & is.finite(w) & w > 0
    if (!any(ok) || is.na(mode_val)) return(NA_real_)
    denom <- sum(w[ok])
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(w[ok & x == mode_val]) / denom
  }

  agg <- dt[, {
    w <- as.numeric(w_valid)
    out <- list(
      n_valid = as.integer(sum(as.integer(n_valid), na.rm = TRUE)),
      w_valid = as.numeric(sum(w, na.rm = TRUE)),
      sum_w2  = as.numeric(sum(as.numeric(sum_w2), na.rm = TRUE)),
      na_frac = safe_weighted_mean(as.numeric(na_frac), w)
    )
    for (m in mslsp_metrics) {
      mcol <- paste0(m, "_mean"); scol <- paste0(m, "_sd")
      mu <- if (mcol %in% names(.SD)) as.numeric(get(mcol)) else rep(NA_real_, .N)
      sd <- if (scol %in% names(.SD)) as.numeric(get(scol)) else rep(NA_real_, .N)
      out[[mcol]] <- safe_weighted_mean(mu, w)
      out[[scol]] <- safe_weighted_sd(mu, sd, w)
    }
    for (q in c(mslsp_qa_cat, mslsp_year_fields_mode)) {
      qcol  <- paste0(q, "_mode"); fqcol <- paste0(q, "_mode_frac")
      qx    <- if (qcol %in% names(.SD)) as.numeric(get(qcol)) else rep(NA_real_, .N)
      qm    <- weighted_mode(qx, w)
      out[[qcol]]  <- qm
      out[[fqcol]] <- weighted_mode_frac(qx, qm, w)
    }
    for (f in mslsp_year_fields_mean) {
      mcol <- paste0(f, "_mean"); scol <- paste0(f, "_sd")
      mu   <- if (mcol %in% names(.SD)) as.numeric(get(mcol)) else rep(NA_real_, .N)
      sd   <- if (scol %in% names(.SD)) as.numeric(get(scol)) else rep(NA_real_, .N)
      out[[mcol]] <- safe_weighted_mean(mu, w)
      out[[scol]] <- safe_weighted_sd(mu, sd, w)
    }
    out
  }, by = key]
  setcolorder(agg, c(key, setdiff(names(agg), key)))
  agg
}

# =============================================================================
# Product object
# =============================================================================

product_mslsp <- function() {
  if (!exists("weighted_stats", mode = "function")) {
    stop("Source extract_summary_core.R first (provides weighted_stats, weighted_mode_stats)")
  }
  if (!exists("mslsp_combine", mode = "function")) {
    stop("Source combine_mslsp_tilepieces.R first (provides mslsp_combine)")
  }
  list(
    prep_static          = mslsp_prep_static_tilewise,
    scene_index          = mslsp_scene_index,
    scene_index_tile_col = "tile_id",
    process_scene        = mslsp_process_scene_tilewise,

    # MSLSP loads all geometry upfront in prep_static, so no prepare_tile needed.
    # tilewise_core will slice prep$polys[parcel_indices, ] per tile.
    prepare_tile = NULL,

    combine = mslsp_combine,

    path_tilepieces   = function(out_dir, year, time_key) {
      file.path(out_dir, sprintf("tilepieces_year=%d", year))
    },
    path_final_output = function(out_dir, year, time_key) {
      file.path(out_dir, sprintf("mslsp_year=%d.parquet", year))
    },
    # path_combine_parts not used (combine handles output directly)
    path_combine_parts = function(out_dir, year, time_key) NULL,

    validate_tilepiece = function(dt) {
      req      <- c("parcel_id", "year", "cycle", "n_valid", "w_valid")
      has_req  <- all(req %in% names(dt))
      has_met  <- any(paste0(mslsp_metrics, "_mean") %in% names(dt))
      has_req && has_met
    },

    # Bucket fallback (used only when combine is not available).
    prepare_for_scatter = function(dt, n_buckets) {
      dt[, bucket := parcel_id_to_bucket(parcel_id, n_buckets)]
      dt
    },
    scatter_cols = function(dt) {
      cols <- setdiff(names(dt), "bucket")
      dt[, ..cols]
    },
    aggregate_bucket = function(dt, year) mslsp_aggregate_tilepieces(dt, year),

    empty_tilepiece_schema = function() {
      base <- data.table(
        parcel_id = character(), year = integer(), cycle = integer(),
        n_valid = integer(), w_valid = double(), sum_w2 = double(), na_frac = double()
      )
      for (m in mslsp_metrics) {
        base[[paste0(m, "_mean")]] <- double()
        base[[paste0(m, "_sd")]]   <- double()
      }
      for (q in c(mslsp_qa_cat, mslsp_year_fields_mode)) {
        base[[paste0(q, "_mode")]]      <- double()
        base[[paste0(q, "_mode_frac")]] <- double()
      }
      for (f in mslsp_year_fields_mean) {
        base[[paste0(f, "_mean")]] <- double()
        base[[paste0(f, "_sd")]]   <- double()
      }
      base[, tile_id := character()]
      base
    },
    empty_part_schema = function(year) {
      base <- data.table(
        parcel_id = character(), year = integer(), cycle = integer(),
        n_valid = integer(), w_valid = double(), sum_w2 = double(), na_frac = double()
      )
      for (m in mslsp_metrics) {
        base[[paste0(m, "_mean")]] <- double()
        base[[paste0(m, "_sd")]]   <- double()
      }
      for (q in c(mslsp_qa_cat, mslsp_year_fields_mode)) {
        base[[paste0(q, "_mode")]]      <- double()
        base[[paste0(q, "_mode_frac")]] <- double()
      }
      for (f in mslsp_year_fields_mean) {
        base[[paste0(f, "_mean")]] <- double()
        base[[paste0(f, "_sd")]]   <- double()
      }
      base
    }
  )
}
