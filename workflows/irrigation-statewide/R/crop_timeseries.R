#!/usr/bin/env Rscript

# Sys.setenv(TAR_PROJECT = "small")
# targets::tar_load(c(crops_with_soil, phenology, precip, etref))

make_crop_timeseries <- function(crops_with_soil, phenology, precip, etref) {
  crop_cols <- c(
    "parcel_id",
    "year",
    "crop_name",
    "whc_min_frac",
    "whc_mm",
    "whc_min_frac"
  )

  crop_soil_timeseries <- crops_with_soil |>
    dplyr::select(dplyr::all_of(crop_cols)) |>
    dplyr::inner_join(
      phenology,
      by = c("parcel_id", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::slice_max(
      .data$canopy_cover,
      n = 1,
      by = c("parcel_id", "date")
    )

  check_unique <- crop_soil_timeseries |>
    dplyr::group_by(.data$parcel_id, .data$date) |>
    dplyr::count() |>
    dplyr::filter(.data$n > 1)
  if (nrow(check_unique) > 1) {
    bad_parcels <- unique(check_unique[["parcel_id"]])
    warning(
      "The parcels below have some non-unique values ",
      "even after `slice_max(canopy_cover)`. ",
      "This is likely because of non-unique ",
      "landIQ crop --> crop_type mappings. ",
      "Selecting only the first row in each of these cases.",
      "\n",
      paste(bad_parcels, collapse = ", ")
    )
    crop_soil_timeseries <- crop_soil_timeseries |>
      dplyr::slice_max(
        .data$canopy_cover,
        n = 1,
        by = c("parcel_id", "date"),
        with_ties = FALSE
      )
  }

  start_date <- min(crop_soil_timeseries[["date"]])
  end_date <- max(crop_soil_timeseries[["date"]])

  complete_crop_timeseries <- precip |>
    dplyr::filter(
      .data$date >= .env$start_date,
      .data$date <= .env$end_date
    ) |>
    dplyr::left_join(
      dplyr::select(etref, -"year"),
      by = c("parcel_id", "date")
    ) |>
    dplyr::arrange(.data$parcel_id, .data$date) |>
    tidyr::fill("etref_mm_day") |>
    dplyr::left_join(crop_soil_timeseries, by = c("parcel_id", "date")) |>
    tidyr::replace_na(list(canopy_cover = 0)) |>
    tidyr::fill(
      c("whc_min_frac", "whc_mm", "crop_name"),
      .direction = "downup"
    ) |>
    dplyr::mutate(
      etc_mm_day = eto_to_etc_bism(
        eto = .data$etref_mm_day,
        crop_name = .data$crop_name[[1]],
        date = .data$date
      ),
      .by = "crop_name"
    )

  complete_crop_timeseries
}
