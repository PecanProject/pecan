# Assemble gap-filled crops_all_years.parq from source LandIQ + per-year gap-fill outputs.
# Adds subclass_source and adoy_source provenance columns; harmonizes SUBCLASS to 2021 RS legend.

landiq_product_root <- function() {
  env <- trimws(Sys.getenv("LANDIQ_GAPFILLED", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(root)) {
    stop("Set LANDIQ_GAPFILLED or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  file.path(root, "LandIQ", "gapfilled")
}

resolve_landiq_product_source_parquet <- function() {
  path_out <- file.path(landiq_product_root(), "crops_all_years.parq")
  if (file.exists(path_out)) {
    message("Product base: existing gap-filled product (", path_out, ")")
    return(path_out)
  }
  path_src <- path_landiq_parquet()
  message("Product base: source LandIQ (", path_src, ")")
  path_src
}

.empty_lab <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | !nzchar(x)
}


.product_adoy_source <- function(adoy) {
  adoy <- suppressWarnings(as.numeric(adoy))
  dplyr::if_else(is_valid_adoy(adoy), "observed", "unfilled")
}

.load_adoy_patch <- function(gapfill_year) {
  path <- file.path(
    path_outputs(),
    sprintf("landiq_adoy_gapfill_year=%d.parquet", as.integer(gapfill_year))
  )
  if (!file.exists(path)) {
    stop("Missing ADOY gap-fill output: ", path)
  }
  raw <- arrow::read_parquet(path, as_data_frame = TRUE)
  if (!"ADOY" %in% names(raw) || !"adoy_source" %in% names(raw)) {
    stop("ADOY gap-fill parquet must contain ADOY and adoy_source: ", path)
  }
  dplyr::tibble(
    parcel_id = trimws(as.character(raw$parcel_id)),
    year = as.integer(raw$year),
    season = as.integer(raw$season),
    ADOY_patch = suppressWarnings(as.numeric(raw$ADOY)),
    adoy_source_patch = trimws(as.character(raw$adoy_source))
  )
}

.load_within_year_crop_patch <- function(gapfill_year) {
  path <- path_within_year_gapfill(gapfill_year)
  if (!file.exists(path)) {
    return(NULL)
  }
  wy <- arrow::read_parquet(path, as_data_frame = TRUE)
  if (nrow(wy) == 0L) {
    return(NULL)
  }
  if (!"subclass_source" %in% names(wy)) {
    stop("Within-year gap-fill parquet must contain subclass_source: ", path)
  }
  wy %>%
    dplyr::transmute(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      gf_SUBCLASS = .normalize_subclass(SUBCLASS),
      gf_subclass_source = trimws(as.character(subclass_source))
    )
}

.load_full_year_crop_patch <- function(gapfill_year) {
  path <- path_subclass_assignment(gapfill_year)
  if (!file.exists(path)) {
    stop("Missing subclass assignment output: ", path)
  }
  season_use <- as.integer(Sys.getenv("LANDIQ_ADOY_DEFAULT_SEASON", "2"))
  raw <- arrow::read_parquet(path, as_data_frame = TRUE)
  if (!"subclass_source" %in% names(raw)) {
    stop("Subclass assignment parquet must contain subclass_source: ", path)
  }
  raw %>%
    dplyr::transmute(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(gapfill_year),
      season = season_use,
      gf_CLASS = trimws(as.character(pred_class)),
      gf_SUBCLASS = .normalize_subclass(pred_subclass_assignment),
      gf_COUNTY = trimws(as.character(county)),
      gf_subclass_source = trimws(as.character(subclass_source))
    )
}

.apply_crop_patch <- function(df, crop_patch) {
  if (is.null(crop_patch) || nrow(crop_patch) == 0L) {
    return(df)
  }

  out <- df %>%
    dplyr::left_join(crop_patch, by = c("parcel_id", "year", "season"))

  if (!"gf_CLASS" %in% names(out)) {
    out$gf_CLASS <- NA_character_
  }
  if (!"gf_SUBCLASS" %in% names(out)) {
    out$gf_SUBCLASS <- NA_character_
  }
  if (!"gf_subclass_source" %in% names(out)) {
    out$gf_subclass_source <- NA_character_
  }

  out <- out %>%
    dplyr::mutate(
      CLASS = dplyr::if_else(!is.na(gf_CLASS), gf_CLASS, CLASS),
      SUBCLASS = dplyr::case_when(
        !is.na(gf_CLASS) ~ gf_SUBCLASS,
        !is.na(gf_SUBCLASS) ~ gf_SUBCLASS,
        TRUE ~ SUBCLASS
      ),
      subclass_source = dplyr::case_when(
        !is.na(gf_CLASS) ~ gf_subclass_source,
        !is.na(gf_SUBCLASS) ~ gf_subclass_source,
        TRUE ~ subclass_source
      )
    )

  if ("gf_COUNTY" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        COUNTY = dplyr::if_else(
          !is.na(gf_COUNTY) & (.empty_lab(COUNTY) | is.na(COUNTY)),
          gf_COUNTY,
          COUNTY
        )
      )
  }

  out %>%
    dplyr::select(-dplyr::any_of(c("gf_CLASS", "gf_SUBCLASS", "gf_COUNTY", "gf_subclass_source")))
}

.apply_adoy_patch <- function(df, adoy_patch) {
  df %>%
    dplyr::left_join(adoy_patch, by = c("parcel_id", "year", "season")) %>%
    dplyr::mutate(
      ADOY = dplyr::if_else(
        !is.na(ADOY_patch) & adoy_source_patch != "unfilled",
        ADOY_patch,
        ADOY
      ),
      adoy_source = dplyr::if_else(
        !is.na(adoy_source_patch) & adoy_source_patch != "unfilled",
        adoy_source_patch,
        adoy_source
      )
    ) %>%
    dplyr::select(-ADOY_patch, -adoy_source_patch)
}

# LandIQ stores each parcel-year as four season rows (1-4).
LANDIQ_SEASONS <- 1:4

#' Pad a full-gap year to the long format used by observed years.
#'
#' Full-gap years (e.g. 2017) only predict the active season (season 2). Observed
#' years store one row per parcel per season, with inactive seasons present as
#' "no-crop" rows (parcel-static attributes retained, crop-instance columns NA).
#' This replicates the active-season rows across the remaining seasons so every
#' parcel has the full season grid; the padded rows blank crop identity / ADOY
#' and leave provenance NA (not a fill outcome).
.pad_full_gap_year_seasons <- function(active_rows, active_season) {
  pad_seasons <- setdiff(LANDIQ_SEASONS, as.integer(active_season)[1L])
  if (length(pad_seasons) == 0L || nrow(active_rows) == 0L) {
    return(active_rows)
  }

  crop_chr <- c(
    "CLASS", "SUBCLASS", "SPECOND", "IRR_TYP_PA", "IRR_TYP_PB", "EMRG_CROP", "SEN_CROP"
  )
  crop_num <- c("PCNT", "ADOY", "ADOY_SEN", "ADOY_EMRG")

  pads <- lapply(pad_seasons, function(s) {
    p <- active_rows
    p$season <- as.integer(s)
    for (cc in intersect(crop_chr, names(p))) p[[cc]] <- NA_character_
    for (cc in intersect(crop_num, names(p))) p[[cc]] <- NA_real_
    if ("subclass_source" %in% names(p)) p$subclass_source <- NA_character_
    if ("adoy_source" %in% names(p)) p$adoy_source <- NA_character_
    p
  })

  dplyr::bind_rows(c(list(active_rows), pads))
}

.build_full_year_rows <- function(gapfill_year, attr_donor_year = NULL) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  crop <- .load_full_year_crop_patch(gapfill_year)
  adoy <- .load_adoy_patch(gapfill_year)

  if (is.null(attr_donor_year)) {
    nbr <- resolve_gapfill_neighbors(gapfill_year)
    attr_donor_year <- nbr$neighbor_years[1L]
  }
  attr_donor_year <- as.integer(attr_donor_year)[1L]
  season_use <- crop$season[1L]

  # Carry every source attribute column from the donor year so a full-gap year
  # (e.g. 2017) ends up with the same schema as observed years. Columns we derive
  # from gap-fill (crop identity, COUNTY, ADOY, provenance) plus the join keys are
  # dropped from the donor set and recomputed below.
  derived_cols <- c(
    "CLASS", "SUBCLASS", "ADOY", "subclass_source", "adoy_source", "year", "season"
  )
  attrs <- arrow::open_dataset(path_landiq_parquet()) %>%
    dplyr::filter(year == attr_donor_year, season == season_use) %>%
    dplyr::collect() %>%
    dplyr::mutate(parcel_id = trimws(as.character(parcel_id))) %>%
    dplyr::select(-dplyr::any_of(derived_cols)) %>%
    dplyr::distinct(parcel_id, .keep_all = TRUE)

  if ("COUNTY" %in% names(attrs)) {
    attrs <- attrs %>% dplyr::rename(COUNTY_donor = COUNTY)
  }

  active_rows <- crop %>%
    dplyr::left_join(adoy, by = c("parcel_id", "year", "season")) %>%
    dplyr::left_join(attrs, by = "parcel_id") %>%
    dplyr::mutate(
      CLASS = gf_CLASS,
      SUBCLASS = gf_SUBCLASS,
      COUNTY = dplyr::coalesce(gf_COUNTY, COUNTY_donor),
      ADOY = dplyr::if_else(
        !is.na(ADOY_patch) & adoy_source_patch != "unfilled",
        ADOY_patch,
        NA_real_
      ),
      subclass_source = gf_subclass_source,
      adoy_source = dplyr::if_else(
        !is.na(adoy_source_patch),
        adoy_source_patch,
        "unfilled"
      )
    ) %>%
    dplyr::select(
      -dplyr::any_of(c(
        "gf_CLASS", "gf_SUBCLASS", "gf_COUNTY", "gf_subclass_source",
        "ADOY_patch", "adoy_source_patch", "COUNTY_donor"
      ))
    ) %>%
    apply_adoy_class_exempt()

  .pad_full_gap_year_seasons(active_rows, active_season = season_use)
}

.patch_landiq_year <- function(gapfill_year, source_parquet = resolve_landiq_product_source_parquet()) {
  gapfill_year <- as.integer(gapfill_year)[1L]
  mode <- resolve_gapfill_mode(gapfill_year)
  message("Building product slice year=", gapfill_year, " mode=", mode)

  if (identical(mode, "full")) {
    return(.build_full_year_rows(gapfill_year))
  }

  base <- arrow::open_dataset(source_parquet) %>%
    dplyr::filter(year == gapfill_year) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = .normalize_subclass(SUBCLASS),
      subclass_source = subclass_source_observed(),
      adoy_source = .product_adoy_source(ADOY)
    )

  crop_patch <- .load_within_year_crop_patch(gapfill_year)
  adoy_patch <- .load_adoy_patch(gapfill_year)

  base %>%
    .apply_crop_patch(crop_patch) %>%
    .apply_adoy_patch(adoy_patch) %>%
    apply_adoy_class_exempt()
}

.init_provenance_cols <- function(df) {
  df <- df %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      season = as.integer(season),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = .normalize_subclass(SUBCLASS)
    )
  if (!"subclass_source" %in% names(df)) {
    df$subclass_source <- subclass_source_observed()
  }
  if (!"adoy_source" %in% names(df)) {
    df$adoy_source <- .product_adoy_source(df$ADOY)
  }
  df
}

#' Harmonize one year slice to the 2021 legend and normalize provenance.
.finalize_product_year_slice <- function(df, crop_lk) {
  df %>%
    dplyr::mutate(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    ) %>%
    harmonize_landiq_subclass_by_year(crop_lk$merge) %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        is.na(SUBCLASS) | SUBCLASS == "",
        "**",
        SUBCLASS
      ),
      SUBCLASS = dplyr::if_else(
        CLASS == "V" & SUBCLASS == "**",
        vineyard_fallback_subclass(),
        SUBCLASS
      ),
      subclass_source = normalize_subclass_source(CLASS, SUBCLASS, subclass_source)
    ) %>%
    filter_consolidated_parcels()
}

#' Combine year parquet slices into one file without collecting as R data.frames.
.combine_year_parquets <- function(year_files, path_out) {
  tabs <- lapply(year_files, function(f) arrow::read_parquet(f, as_data_frame = FALSE))
  combined <- do.call(arrow::concat_tables, tabs)
  arrow::write_parquet(combined, path_out)
  combined$num_rows
}

build_landiq_product <- function(
    years = landiq_product_years(),
    source_root = path_landiq_root(),
    out_root = landiq_product_root()) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    stop("No years to build")
  }

  out_dir <- normalizePath(out_root, mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path_out <- file.path(out_dir, "crops_all_years.parq")
  path_src_gpkg <- file.path(source_root, "parcels-consolidated.gpkg")

  message("=== LandIQ gap-filled product ===")
  message("Source: ", source_root)
  message("Output: ", out_dir)
  message("Years: ", paste(years, collapse = ", "))
  message("Geometry (unchanged): ", path_src_gpkg)

  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  source_parquet <- resolve_landiq_product_source_parquet()
  # Warm consolidated-id cache once (used per year below).
  consolidated_ids <- load_consolidated_parcel_ids()

  all_years <- arrow::open_dataset(source_parquet) %>%
    dplyr::distinct(year) %>%
    dplyr::collect() %>%
    dplyr::pull(year) %>%
    as.integer() %>%
    sort()
  all_years <- unique(c(all_years, years))
  all_years <- all_years[!is.na(all_years)]

  tmp_dir <- file.path(out_dir, paste0(".merge_tmp_", Sys.getpid()))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE)
  }
  dir.create(tmp_dir, recursive = TRUE)

  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  message(
    "Building product one calendar year at a time (",
    length(all_years), " years; ", length(consolidated_ids), " consolidated parcels)"
  )

  year_files <- character(0)
  n_total <- 0L
  for (y in all_years) {
    message("Building product slice year=", y,
            if (y %in% years) " mode=gapfill" else " mode=carry")
    if (y %in% years) {
      df <- .patch_landiq_year(y, source_parquet = source_parquet)
    } else {
      df <- arrow::open_dataset(source_parquet) %>%
        dplyr::filter(year == y) %>%
        dplyr::collect() %>%
        .init_provenance_cols()
    }
    message("  Harmonizing SUBCLASS to 2021 RS legend")
    df <- .finalize_product_year_slice(df, crop_lk)
    path_y <- file.path(tmp_dir, sprintf("year=%d.parquet", y))
    arrow::write_parquet(df, path_y)
    message("  year ", y, ": ", nrow(df), " rows")
    n_total <- n_total + nrow(df)
    year_files <- c(year_files, path_y)
    rm(df)
    gc(verbose = FALSE)
  }

  message("Combining ", length(year_files), " year slices -> ", path_out)
  n_rows <- .combine_year_parquets(year_files, path_out)
  message("Wrote ", n_rows, " rows -> ", path_out)

  product_label <- basename(out_dir)
  meta_path <- file.path(out_dir, "README.md")
  writeLines(
    c(
      paste0("# ", product_label),
      "",
      "Gap-filled crop identity and ADOY on top of harmonized LandIQ.",
      "Columns `subclass_source` and `adoy_source` record provenance per row.",
      "All SUBCLASS values use the Nov 2021 DWR RS legend (harmonized_SUBCLASS).",
      "Tabular rows are restricted to consolidated parcel_ids.",
      "After merge, run `scripts/R/cover_crop_landiq.R` for COVER (not gap-fill).",
      "",
      paste0("- Built: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("- Source LandIQ: ", source_root),
      paste0("- Gap-fill package: ", landiq_gapfill_root()),
      paste0("- Gap-fill years: ", paste(years, collapse = ", ")),
      paste0("- Geometry: ", path_src_gpkg, " (unchanged; join here, not under gapfilled)"),
      "",
      "Gap-fill pipeline:",
      "1. `gapfill.R crop` (CLASS / SUBCLASS)",
      "2. `gapfill.R adoy` (ADOY)",
      "3. `gapfill.R merge` (join crop+ADOY fills)",
      "Then: `cover_crop_landiq.R` (COVER), `gapfill.R qc`"
    ),
    meta_path
  )
  message("Wrote ", meta_path)
  invisible(list(path_parquet = path_out, path_gpkg = path_src_gpkg, n_rows = as.integer(n_rows)))
}
