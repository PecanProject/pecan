##' @name eto_to_etc
##' @md
##' @title Convert reference evapotranspiration to crop evapotranspiration
##' @description Multiply reference evapotranspiration (ETo) by a unitless 
##'   crop coefficient (Kc) to produce crop evapotranspiration (ETc). 
##'   Output has the same units as input ETo, with length/time scale, 
##'   e.g., mm d-1.
##' @param eto numeric vector of reference evapotranspiration
##' @param kc numeric vector of crop coefficients
##' @return numeric vector of crop evapotranspiration
##' @importFrom rlang .data .env
##' @export
##'
eto_to_etc <- function(eto, kc) {
  if (!is.numeric(eto) || !is.numeric(kc)) {
    PEcAn.logger::logger.severe("`eto` and `kc` must be numeric vectors.")
  }
  if (!(length(kc) %in% c(1, length(eto)))) {
    PEcAn.logger::logger.severe("`kc` must be length 1 or the same length as `eto`.")
  }
  eto * kc
}

##' @name eto_to_etc_bism
##' @md
##' @title Convert ETo to ETc using BIS/BISm crop coefficients
##'
##' @description
##' Convert grass-reference evapotranspiration (ETo) to crop evapotranspiration
##' (ETc) using crop coefficient (Kc) schedules from the BIS/BISm framework
##' (Snyder et al.). Daily ETc is calculated as `ETc = Kc * ETo`.
##'
##' Kc values are determined using one of two mutually exclusive timing modes,
##' selected by input:
##'
##' 1. **Date-based (percent-of-season) mode**: If `date` is
##'    provided, Kc is interpolated linearly between BIS/BISm growth-stage anchors
##'    (B--C--D--E) using percent-of-season, with default planting and
##'    harvest dates taken from `bism_kc_by_crop`.
##'
##' 2. **Canopy-cover mode**: If `canopy_cover` is provided, Kc is
##'    estimated from observed ground cover. For field and row crops, canopy cover
##'    is used to locate position within the B--C growth phase (linear increase from
##'    `Kc_B` at ~10% cover to `Kc_C` at ~75% cover, capped thereafter).
##'    For tree, vine, and subtropical crops, Kc is scaled from the mature mid-season
##'    value using the immaturity relationships of Snyder et al. (Snyder, Fig. 15).
##'
##' Exactly one of `date` or `canopy_cover` must be supplied.
##'
##' @param eto Numeric vector of reference evapotranspiration (ETo).
##' @param crop_name Character scalar identifying a crop in
##'   `bism_kc_by_crop`.
##' @param date Optional `Date` vector (same length as `eto`); triggers
##'   percent-of-season interpolation using BIS/BISm defaults.
##' @param canopy_cover Optional numeric vector giving fractional ground cover
##'   (0-1), length 1 or `length(eto)`; triggers canopy-cover-based Kc
##'   estimation.
##'
##' @return Numeric vector of crop evapotranspiration (ETc), with the same units as
##'   `eto` (e.g., mm d-1).
##'
##' @references
##' Snyder, R., Orang, M., Bali, K., Eching, S., Zaccaria, D. (2014).
##' *BISm Basic Irrigation Scheduling Excel program (metric units)*.
##' University of California Cooperative Extension.
##'
##' Snyder, R.L., 2014. Irrigation scheduling and soil water budgeting (ISWBM). University of California, Davis. https://biomet.ucdavis.edu/basic-irrigation-scheduling-(BIS).html
##'
##' Doorenbos, J., Pruitt, W.O. (1977).
##' *Guidelines for predicting crop water requirements*.
##' FAO Irrigation and Drainage Paper 24.
##'
##' @export
eto_to_etc_bism <- function(
  eto,
  crop_name,
  date = NULL,
  canopy_cover = NULL
) {
  if (!is.numeric(eto)) {
    PEcAn.logger::logger.severe("`eto` must be numeric.")
  }
  if (!is.character(crop_name) || length(crop_name) != 1) {
    PEcAn.logger::logger.severe("`crop_name` must be a single character value.")
  }
  timing_inputs <- c(!is.null(date), !is.null(canopy_cover))
  if (sum(timing_inputs) != 1) {
    PEcAn.logger::logger.severe("Provide exactly one of `date` or `canopy_cover`.")
  }

  kc_row <- PEcAn.data.land::bism_kc_by_crop |>
    dplyr::filter(.data$crop_name == .env$crop_name)
  if (nrow(kc_row) != 1) {
    PEcAn.logger::logger.severe("`crop_name` must match exactly one row in `bism_kc_by_crop`.")
  }

  ## percent of season from date (default BISm behavior)
  ## Using default planting/harvest dates from bism_kc_by_crop
  ## TODO: allow user-specified planting/harvest dates?
  if (!is.null(date)) {
    if (!inherits(date, "Date")) {
      PEcAn.logger::logger.severe("`date` must be a Date vector.")
    }
    if (length(eto) != length(date)) {
      PEcAn.logger::logger.severe("`eto` and `date` must be the same length.")
    }
    planting <- lubridate::make_date(
      lubridate::year(date), 
      kc_row$planting_month, 
      kc_row$planting_day)
    harvest  <- lubridate::make_date(
      lubridate::year(date), 
      kc_row$harvest_month,  
      kc_row$harvest_day)
    idx <- harvest < planting
    harvest[idx] <- harvest[idx] + lubridate::years(1)
    season_len <- as.numeric(harvest - planting)
    percent_season <- 100 * as.numeric(date - planting) / season_len

    kc_fun <- stats::approxfun(
      x = c(kc_row$percent_season_B, kc_row$percent_season_C,
            kc_row$percent_season_D, 100),
      y = c(kc_row$KcB, kc_row$KcC, kc_row$KcD, kc_row$KcE),
      rule = 2,
      method = "linear"
    )
    kc <- kc_fun(percent_season)

  } else {
    ## canopy-cover-based logic
    if (!is.numeric(canopy_cover)) {
      PEcAn.logger::logger.severe("`canopy_cover` must be numeric.")
    }
    if (!(length(canopy_cover) %in% c(1, length(eto)))) {
      PEcAn.logger::logger.severe("`canopy_cover` must be length 1 or the same length as `eto`.")
    }
    if (length(canopy_cover) == 1) canopy_cover <- rep(canopy_cover, length(eto))

    cover_pct <- canopy_cover * 100

    crop_type <- floor(kc_row$crop_number)

    if (crop_type %in% c(3, 4)) {
      ## Equations 5 and 6 from Snyder et al. 2014
      cc <- pmin(cover_pct, 70)
      kc_mid <- kc_row$KcC
      frac <- if (crop_type == 4) {
        sqrt(sin((cc / 70) * pi / 2))
      } else {
        sin((cc / 70) * pi / 2)
      }
      kc <- kc_mid * frac

    } else {
      ## field and row crops: 
      ## piecewise linear B–C, capped at mid‑season
      kc <- dplyr::case_when(
        cover_pct < 10 ~ kc_row$KcB,
        cover_pct < 75 ~ kc_row$KcB +
          (kc_row$KcC - kc_row$KcB) * (cover_pct - 10) / (75 - 10),
        TRUE ~ kc_row$KcC
      )
    }
  }

  eto_to_etc(eto, kc)
}
