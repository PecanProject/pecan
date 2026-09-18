#!/usr/bin/env Rscript
# library(PEcAn.data.land)
# parcel_ids <- parcel_id_batches[[1]]

get_parcel_ids <- function(crops_path, n_parcels = NULL) {
  parcel_ids <- arrow::open_dataset(crops_path) |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::collect() |>
    dplyr::mutate(parcel_id = as.numeric(.data$parcel_id)) |>
    dplyr::pull(.data$parcel_id)
  if (is.null(n_parcels)) return(parcel_ids)
  sample(parcel_ids, n_parcels)
}

get_phenology <- function(mslsp_path, parcel_ids = NULL) {
  parcel_ids_chr <- if (is.null(parcel_ids)) {
    NULL
  } else {
    as.character(parcel_ids)
  }
  dat_raw <- mslsp_to_canopycover(mslsp_path, parcel_ids_chr)
  phenology_raw <- dat_raw |>
    dplyr::mutate(
      parcel_id = as.numeric(parcel_id),
      landiq_SUBCLASS = as.integer(.data$landiq_SUBCLASS)
    )
  # Resolve overlapping canopy_cover values. If a single date has multiple
  # rows, we take the row with the largest canopy cover.
  phenology <- phenology_raw |>
    dplyr::slice_max(.data$canopy_cover, n = 1, by = c("parcel_id", "date")) |>
    dplyr::relocate("parcel_id", "year", "season", "date")
  phenology
}

get_etref <- function(cimis_etref_path, parcel_ids = NULL) {
  dat <- arrow::open_dataset(cimis_etref_path)
  if (!is.null(parcel_ids)) {
    parcel_ids_num <- as.numeric(parcel_ids)
    dat <- dplyr::filter(dat, .data$parcel_id %in% .env$parcel_ids_num)
  }
  etref <- dat |>
    dplyr::arrange(.data$parcel_id, .data$date) |>
    dplyr::collect()
  etref
}

get_precip <- function(chirps_precip_path, parcel_ids = NULL) {
  dat <- arrow::open_dataset(chirps_precip_path)
  if (!is.null(parcel_ids)) {
    parcel_ids_num <- as.numeric(parcel_ids)
    dat <- dplyr::filter(dat, .data$parcel_id %in% .env$parcel_ids_num)
  }
  precip <- dat |>
    dplyr::arrange(.data$parcel_id, .data$date) |>
    dplyr::collect()
  precip
}

# LandIQ / BIS "no subclass" -> "**"; else digit string.
normalize_landiq_subclass <- function(x) {
  x <- as.character(x)
  bad <- is.na(x) | x %in% c("", "**", "NA")
  x[bad] <- "**"
  x[!bad] <- as.character(as.integer(as.numeric(x[!bad])))
  x
}

# YP borrows next year's season-2 D/C/V from the same crops table.
resolve_yp_via_lookahead <- function(crops) {
  nxt <- crops |>
    dplyr::filter(
      as.integer(.data$season) == 2L,
      .data$CLASS %in% c("D", "C", "V")
    ) |>
    dplyr::transmute(
      parcel_id = .data$parcel_id,
      year = as.integer(.data$year) - 1L,
      look_CLASS = .data$CLASS,
      look_SUBCLASS = .data$SUBCLASS
    ) |>
    dplyr::slice(1, .by = c("parcel_id", "year"))

  crops |>
    dplyr::left_join(nxt, by = c("parcel_id", "year")) |>
    dplyr::mutate(
      .yp_hit = .data$CLASS == "YP" & !is.na(.data$look_CLASS),
      CLASS = dplyr::if_else(.data$.yp_hit, .data$look_CLASS, .data$CLASS),
      SUBCLASS = dplyr::if_else(
        .data$.yp_hit,
        .data$look_SUBCLASS,
        .data$SUBCLASS
      )
    ) |>
    dplyr::select(-"look_CLASS", -"look_SUBCLASS", -".yp_hit")
}

#' @importFrom PEcAn.data.land bism_kc_by_crop crop_whc
get_crop_info <- function(crops_path, parcel_ids = NULL) {
  # parcel_ids <- parcel_id_batches[[1]]
  dat <- arrow::open_dataset(crops_path)
  if (!is.null(parcel_ids)) {
    parcel_ids_num <- as.numeric(parcel_ids)
    dat <- dplyr::filter(dat, .data$parcel_id %in% .env$parcel_ids_num)
  }
  dlocal <- dat |>
    dplyr::collect() |>
    dplyr::mutate(parcel_id = as.numeric(.data$parcel_id))

  #' NOTE: Some LandIQ classes/subclasses map onto multiple BISM crop
  #' types. HACK: select just the first crop per class/subclass group.
  #' Extra LandIQ keys live in packaged bism_kc_by_crop; YP uses look-ahead.
  bism_crop_unique <- bism_kc_by_crop |>
    dplyr::mutate(
      landiq_subclass = normalize_landiq_subclass(.data$landiq_subclass)
    ) |>
    dplyr::arrange(.data$crop_number) |>
    dplyr::distinct(
      .data$landiq_class,
      .data$landiq_subclass,
      .data$crop_name
    ) |>
    dplyr::slice(1, .by = c("landiq_class", "landiq_subclass"))

  crops <- dlocal |>
    dplyr::mutate(
      SUBCLASS = normalize_landiq_subclass(.data$SUBCLASS)
    ) |>
    resolve_yp_via_lookahead() |>
    dplyr::left_join(
      bism_crop_unique,
      by = c(
        "CLASS" = "landiq_class",
        "SUBCLASS" = "landiq_subclass"
      )
    )

  missing_crops <- dplyr::filter(crops, is.na(.data$crop_name))
  if (nrow(missing_crops) > 0) {
    missing_crop_strs <- missing_crops |>
      dplyr::distinct(.data$CLASS, .data$SUBCLASS) |>
      dplyr::mutate(
        string = glue::glue(
          "CLASS: {.data$CLASS} ",
          "SUBCLASS: {.data$SUBCLASS}"
        )
      ) |>
      dplyr::pull(.data$string)
    warning(
      "Skipping ",
      nrow(missing_crops),
      " rows with no matching BIS crop. Relevant pairs are: [",
      paste(missing_crop_strs, collapse = "; "),
      "]"
    )
  }
  crop_whc_sub <- dplyr::select(
    crop_whc,
    "crop_name",
    "whc_min_frac",
    "rooting_depth_m"
  )
  crop_info <- crops |>
    dplyr::filter(!is.na(.data$crop_name)) |>
    dplyr::left_join(crop_whc_sub, by = "crop_name")
  crop_info
}
