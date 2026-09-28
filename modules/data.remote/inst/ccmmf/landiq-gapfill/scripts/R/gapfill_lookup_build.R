# Build CDL x LandIQ subclass lookup tables from overlapping parcel-years.

#' Output paths for emission lookup artifacts.
#'
#' @param suffix Training-year suffix from [landiq_lookup_build_suffix()].
#' @return Named list of parquet/CSV paths under [path_outputs()].
emission_lookup_paths <- function(suffix = landiq_lookup_build_suffix()) {
  out <- path_outputs()
  list(
    mass = file.path(out, sprintf("cdl_landiq_subclass_lookup_%s.parquet", suffix)),
    dominant = file.path(out, sprintf("cdl_landiq_subclass_lookup_dominant_%s.parquet", suffix)),
    prior = file.path(out, sprintf("landiq_subclass_frequency_%s.parquet", suffix)),
    coverage = file.path(out, sprintf("cdl_landiq_subclass_coverage_%s.csv", suffix)),
    coverage_dominant = file.path(out, sprintf("cdl_landiq_subclass_coverage_dominant_%s.csv", suffix)),
    codes_seen = file.path(out, sprintf("cdl_codes_seen_%s.csv", suffix))
  )
}

#' Load USDA NASS CDL code name lookup.
load_cdl_nass_lookup <- function() {
  path_cdl_lookup <- path_cdl_nass_lookup_csv()
  if (!file.exists(path_cdl_lookup)) {
    stop("Missing USDA CDL code name lookup CSV: ", path_cdl_lookup)
  }
  readr::read_csv(path_cdl_lookup, show_col_types = FALSE) %>%
    dplyr::mutate(
      cdl_code = as.integer(cdl_code),
      cdl_name = as.character(cdl_name)
    ) %>%
    dplyr::distinct(cdl_code, .keep_all = TRUE)
}

#' Join harmonized LandIQ season-2 rows with CDL fractions across training years.
#'
#' @return Tibble with parcel_id, year, truth_key, CLASS, SUBCLASS, cdl_code, frac.
load_landiq_cdl_training_join <- function(
    train_years = landiq_emission_training_years(),
    path_landiq = path_landiq_parquet(),
    path_cdl = path_cdl_fractions(),
    crop_lk = load_landiq_crop_lookup(path_crop_lookup_csv()),
    ag_classes = load_ag_class_vector(path_crop_lookup_csv())) {
  liq <- arrow::open_dataset(path_landiq) %>%
    dplyr::filter(season == 2L, year %in% train_years, CLASS %in% ag_classes) %>%
    dplyr::select(parcel_id, year, CLASS, SUBCLASS) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    ) %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        is.na(SUBCLASS) | SUBCLASS == "",
        "**",
        SUBCLASS
      )
    ) %>%
    harmonize_landiq_subclass_by_year(crop_lk$merge) %>%
    landiq_truth_keys()

  joined_parts <- vector("list", length(train_years))
  for (i in seq_along(train_years)) {
    y <- train_years[i]
    path_cdl_y <- file.path(path_cdl, sprintf("cdl_fractions_year=%d.parquet", y))
    if (!file.exists(path_cdl_y)) {
      stop("Missing CDL parquet for training year ", y, ": ", path_cdl_y)
    }
    cdl_y <- suppressWarnings(arrow::read_parquet(path_cdl_y)) %>%
      dplyr::mutate(
        parcel_id = trimws(as.character(parcel_id)),
        year = as.integer(y),
        cdl_code = as.integer(cdl_code)
      ) %>%
      dplyr::filter(!is.na(cdl_code), !is.na(frac), frac > 0) %>%
      dplyr::select(parcel_id, year, cdl_code, frac)

    liq_y <- liq %>% dplyr::filter(year == y)
    j <- liq_y %>%
      dplyr::inner_join(cdl_y, by = c("parcel_id", "year")) %>%
      apply_landiq_subclass_split_by_cdl(crop_lk$split) %>%
      landiq_truth_keys()

    message("Year ", y, ": ", nrow(liq_y), " LandIQ rows, ", nrow(j), " after CDL join")
    joined_parts[[i]] <- j %>%
      dplyr::transmute(parcel_id, year, truth_key, CLASS, SUBCLASS, cdl_code, frac)
  }

  joined <- dplyr::bind_rows(joined_parts)
  if (nrow(joined) == 0L) {
    stop("No joined LandIQ x CDL rows across training years")
  }
  joined
}

#' Build subclass prior P(SUBCLASS | CLASS) from LandIQ history.
build_subclass_prior_table <- function(
    prior_years = landiq_subclass_prior_years(),
    path_landiq = path_landiq_parquet(),
    crop_lk = load_landiq_crop_lookup(path_crop_lookup_csv()),
    ag_classes = load_ag_class_vector(path_crop_lookup_csv())) {
  liq_prior <- arrow::open_dataset(path_landiq) %>%
    dplyr::filter(season == 2L, year %in% prior_years, CLASS %in% ag_classes) %>%
    dplyr::select(parcel_id, year, CLASS, SUBCLASS) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      year = as.integer(year),
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    ) %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        is.na(SUBCLASS) | SUBCLASS == "",
        "**",
        SUBCLASS
      )
    ) %>%
    harmonize_landiq_subclass_by_year(crop_lk$merge)

  liq_prior %>%
    dplyr::filter(SUBCLASS != "**") %>%
    dplyr::count(CLASS, SUBCLASS, name = "n") %>%
    dplyr::group_by(CLASS) %>%
    dplyr::mutate(prior = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(truth_key = paste(CLASS, SUBCLASS, sep = "::"))
}

#' Build CDL x LandIQ lookup tables and QC CSVs.
#'
#' Writes fraction-weighted and dominant-CDL lookups, subclass prior, and coverage summaries.
#' @return Invisibly returns [emission_lookup_paths()].
build_emission_lookup <- function() {
  yr_info <- landiq_lookup_years()
  suffix <- landiq_lookup_build_suffix()
  paths <- emission_lookup_paths(suffix)
  dir.create(path_outputs(), recursive = TRUE, showWarnings = FALSE)

  message(
    "Emission lookup training years: ", paste(yr_info$train_years, collapse = ", "),
    " (excluded: ", paste(yr_info$excluded, collapse = ", "), "); suffix=", suffix
  )

  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  cdl_lookup <- load_cdl_nass_lookup()
  joined <- load_landiq_cdl_training_join(crop_lk = crop_lk)

  mass_long <- joined %>%
    dplyr::group_by(truth_key, CLASS, SUBCLASS, cdl_code) %>%
    dplyr::summarise(mass = sum(frac, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(mass > 0) %>%
    dplyr::left_join(cdl_lookup, by = "cdl_code") %>%
    dplyr::mutate(
      cdl_name = dplyr::if_else(
        is.na(cdl_name),
        paste0("UNKNOWN_", cdl_code),
        cdl_name
      )
    )

  arrow::write_parquet(mass_long, paths$mass)
  message("Wrote ", paths$mass, " (", nrow(mass_long), " rows)")

  onehot_long <- joined %>%
    dplyr::group_by(parcel_id, year, truth_key, CLASS, SUBCLASS) %>%
    dplyr::slice_max(order_by = frac, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::count(truth_key, CLASS, SUBCLASS, cdl_code, name = "n_parcel_years") %>%
    dplyr::filter(n_parcel_years > 0L) %>%
    dplyr::left_join(cdl_lookup, by = "cdl_code") %>%
    dplyr::mutate(
      cdl_name = dplyr::if_else(
        is.na(cdl_name),
        paste0("UNKNOWN_", cdl_code),
        cdl_name
      )
    )

  arrow::write_parquet(onehot_long, paths$dominant)
  message("Wrote ", paths$dominant, " (", nrow(onehot_long), " rows)")

  readr::write_csv(
    onehot_long %>%
      dplyr::group_by(truth_key, CLASS, SUBCLASS) %>%
      dplyr::summarise(
        n_parcel_years_row = sum(n_parcel_years),
        n_codes_gt0 = dplyr::n(),
        max_share = max(n_parcel_years / sum(n_parcel_years)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(n_codes_gt0, truth_key),
    paths$coverage_dominant
  )

  prior_tbl <- build_subclass_prior_table(crop_lk = crop_lk)
  arrow::write_parquet(prior_tbl, paths$prior)
  message("Wrote ", paths$prior, " (", nrow(prior_tbl), " CLASS::SUBCLASS rows)")

  readr::write_csv(
    mass_long %>%
      dplyr::group_by(truth_key, CLASS, SUBCLASS) %>%
      dplyr::summarise(
        row_mass = sum(mass),
        n_codes_gt0 = dplyr::n(),
        max_share = max(mass / sum(mass)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(n_codes_gt0, truth_key),
    paths$coverage
  )

  readr::write_csv(
    mass_long %>%
      dplyr::group_by(cdl_code, cdl_name) %>%
      dplyr::summarise(total_mass = sum(mass), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total_mass)),
    paths$codes_seen
  )

  invisible(paths)
}
