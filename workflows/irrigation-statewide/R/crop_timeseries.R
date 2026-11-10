#!/usr/bin/env Rscript

# Sys.setenv(TAR_PROJECT = "small")
# targets::tar_load(c(crops_with_soil, phenology, precip, etref))

# Join crops to MSLSP by parcel, year, and season. Crop is labeled only between
# greenup (OGI) and min(OGMn, next OGI - 1). Between seasons use dormant Kc, do
# not irrigate, and carry WHC. In-season Kc is canopy or percent-of-season
# (kc_timing); plant/harvest stay OGI/OGMn.
DORMANT_KC <- 0.15

make_crop_timeseries <- function(
  crops_with_soil,
  phenology,
  precip,
  etref,
  kc_timing = c("date", "canopy")
) {
  kc_timing <- match.arg(kc_timing)

  crop_soil_timeseries <- crops_with_soil |>
    dplyr::select(
      "parcel_id", "year", "season", "crop_name", "whc_min_frac", "whc_mm"
    ) |>
    dplyr::filter(!is.na(.data$crop_name)) |>
    dplyr::inner_join(
      phenology |>
        dplyr::summarise(
          ogi = min(.data$date),
          ogmn = max(.data$date),
          .by = c("parcel_id", "year", "season")
        ),
      by = c("parcel_id", "year", "season")
    ) |>
    dplyr::filter(.data$ogmn >= .data$ogi) |>
    dplyr::arrange(.data$parcel_id, .data$ogi) |>
    dplyr::mutate(
      end = pmin(.data$ogmn, dplyr::lead(.data$ogi) - 1, na.rm = TRUE),
      .by = "parcel_id"
    ) |>
    dplyr::filter(.data$end >= .data$ogi) |>
    dplyr::mutate(
      date = purrr::map2(.data$ogi, .data$end, \(a, b) seq(a, b, by = "1 day"))
    ) |>
    tidyr::unnest("date")

  phen_daily <- phenology |>
    dplyr::slice_max(
      .data$canopy_cover,
      n = 1,
      by = c("parcel_id", "date"),
      with_ties = FALSE
    ) |>
    dplyr::select("parcel_id", "date", "canopy_cover")

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
    dplyr::group_by(.data$parcel_id) |>
    tidyr::fill("etref_mm_day") |>
    dplyr::ungroup() |>
    dplyr::left_join(
      dplyr::select(
        crop_soil_timeseries,
        "parcel_id", "date", "crop_name", "whc_min_frac", "whc_mm", "ogi", "ogmn"
      ),
      by = c("parcel_id", "date")
    ) |>
    dplyr::left_join(phen_daily, by = c("parcel_id", "date")) |>
    tidyr::replace_na(list(canopy_cover = 0)) |>
    dplyr::group_by(.data$parcel_id) |>
    tidyr::fill("whc_mm", .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      irrigation_max_mm = dplyr::if_else(!is.na(.data$crop_name), 150, 0),
      whc_min_frac = dplyr::if_else(
        !is.na(.data$crop_name), .data$whc_min_frac, 0
      ),
      etc_mm_day = {
        cn <- .data$crop_name[[1]]
        if (is.na(cn)) {
          DORMANT_KC * .data$etref_mm_day
        } else if (identical(kc_timing, "canopy")) {
          eto_to_etc_bism(
            eto = .data$etref_mm_day,
            crop_name = cn,
            canopy_cover = .data$canopy_cover
          )
        } else {
          # BIS table defaults: eto_to_etc_bism(eto, crop_name, date = dates)
          eto_to_etc_bism(
            eto = .data$etref_mm_day,
            crop_name = cn,
            date = .data$date,
            planting = .data$ogi,
            harvest = .data$ogmn
          )
        }
      },
      .by = c("parcel_id", "crop_name")
    )

  complete_crop_timeseries
}
