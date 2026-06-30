#' Calculate water balance for a time series at a single site
#'
#' This is the core water balance calculation that operates on primitive
#' numeric vectors for easy testing and debugging. Each input is a time series
#' of daily values for a single location (one date per row). The units for all
#' quantities are arbitrary, but they should be consistent (distance / time;
#' e.g., most commonly, mm/day).
#'
#' This function operates in *relative* WHC space, where:
#' - `w = 0` represents wilting point (no plant-available water)
#' - `w = whc` represents field capacity (maximum plant-available water)
#' - `whc = (field_capacity - wilting_point) * rooting_depth` (the plant-available range)
#'
#' Although this function can be used as a crude approximation of rice
#' irrigation (by setting `whc_min_frac = 1.0`), we recommend using
#' [calc_water_balance_rice()] instead, which explicitly tracks rice pond
#' depth, implements seepage, etc.
#'
#' @param et Vector of evapotranspiration values (distance / time)
#' @param precip Vector of precipitation values (distance / time)
#' @param whc Water holding capacity (distance); the plant-available range from
#'   wilting point to field capacity (i.e., `whc = field_capacity - wilting_point`).
#'   Can be a single value or a vector of the same length as `et`.
#' @param whc_min_frac Fraction of WHC for minimum water level (irrigation
#'   trigger); unused if `w_min` is explicitly specified.
#'   Can be a single value or a vector of the same length as `et`.
#' @param W_initial Initial soil water content at start of time series
#'   (distance); defaults to `whc[1]` (field capacity) if NULL
#' @param w_min Minimum water level threshold (distance); irrigation is
#'   triggered when soil water falls below this level; defaults to
#'   `whc_min_frac * whc` if NULL.
#'   Can be a single value or a vector of the same length as `et`.
#' @param irrigation_max If set, maximum amount of irrigation to apply at a
#'   time (distance).
#' @return List with vectors: W_t (soil water), irr (irrigation), runoff
#' @examples
#' # Calculate WHC from field capacity, wilting point, and rooting depth
#' field_capacity <- 0.30  # volumetric (m3/m3)
#' wilting_point <- 0.10   # volumetric (m3/m3)
#' rooting_depth <- 1000   # mm
#' whc <- (field_capacity - wilting_point) * rooting_depth  # mm
#'
#' # Run water balance with 5 days of ET and precip data
#' et <- c(4, 5, 6, 4, 3)  # mm/day
#' precip <- c(0, 0, 10, 0, 0)  # mm/day
#' result <- calc_water_balance(et, precip, whc = whc, whc_min_frac = 0.5)
#' str(result)
#' @export
calc_water_balance <- function(
  et,
  precip,
  whc,
  whc_min_frac,
  W_initial = NULL, #nolint: object_name_linter
  w_min = NULL,
  irrigation_max = NULL
) {

  # nolint start: object_name_linter
  n <- length(et)

  if (!length(precip) == n) {
    PEcAn.logger::logger.severe(
      "Precip and ET have different lengths. ",
      "length(precip) = ", length(precip), "  ",
      "length(et) = ", n
    )
  }

  ensure_vec <- function(x, n, name) {
    if (length(x) == 1) {
      rep(x, n)
    } else if (length(x) == n) {
      x
    } else {
      PEcAn.logger::logger.severe(
        sprintf(
          "%s must have length 1 or %d; actual length = %d",
          name,
          n,
          length(x)
        )
      )
    }
  }

  whc <- ensure_vec(whc, n, "whc")
  if (!is.null(whc_min_frac)) {
    whc_min_frac <- ensure_vec(whc_min_frac, n, "whc_min_frac")
  }

  if (is.null(w_min)) {
    if (is.null(whc_min_frac)) {
      PEcAn.logger::logger.severe(
        "Either whc_min_frac or w_min must be provided"
      )
    }
    w_min <- whc_min_frac * whc
  } else {
    w_min <- ensure_vec(w_min, n, "w_min")
  }

  if (is.null(W_initial)) {
    # Initialize at field capacity (i.e., full WHC)
    W_prev <- whc[1]
  } else {
    W_prev <- W_initial
  }

  W_t <- numeric(n)
  irr <- numeric(n)
  runoff <- numeric(n)

  for (t in seq_len(n)) {
    # Potential state after precip and ET
    W0 <- W_prev + precip[t] - et[t]

    # If W0 falls below w_min (e.g., high ET; low precip), irrigate
    # to field capacity (i.e., full WHC), but no more than irrigation_max.
    if (W0 < w_min[t]) {
      irr[t] <- min(whc[t] - W0, irrigation_max)
      W0 <- W0 + irr[t]
    } else {
      irr[t] <- 0
    }

    # If W0 exceeds field capacity (i.e., whc), the difference is runoff.
    if (W0 > whc[t]) {
      runoff[t] <- W0 - whc[t]
      W_t[t] <- whc[t]
    } else {
      runoff[t] <- 0
      W_t[t] <- max(W0, w_min[t])
    }

    W_prev <- W_t[t]
  }

  # nolint end: object_name_linter

  list(W_t = W_t, irr = irr, runoff = runoff)
}

#' Calculate water balance for a flooded rice paddy
#'
#' Models the water balance of a flooded rice system with a two-layer
#' structure: a ponded water layer above a saturated soil profile. This is
#' physically distinct from the upland soil water balance in
#' [calc_water_balance()]. Water is managed to maintain a target flood depth,
#' with support for mid-season drainage events.
#'
#' The soil profile is assumed to be continuously saturated during flooded
#' periods, so plant-available soil water is not tracked separately. ET is
#' applied directly to the pond layer (open-water ET during flooded periods).
#'
#' Irrigation is triggered when pond_depth falls below flood_min. Farmers
#' refill to flood_target. Runoff (bund overflow) occurs when pond_depth
#' exceeds flood_max.
#'
#' Mid-season drainage is specified as a logical vector (`drain[t] = TRUE`
#' means the field is intentionally drained on day t). During drainage days,
#' the pond is drawn down to pond_depth = 0 and irrigation is suppressed. This
#' represents practices such as weed control or pre-harvest drainage.
#'
#' @param et        Numeric vector. Daily reference ET. During flooded
#'                  periods this is treated as open-water ET; no crop
#'                  coefficient is applied here but you can pre-multiply if
#'                  needed.
#' @param precip    Numeric vector. Daily precipitation.
#' @param flood_target Numeric scalar. Target ponded water depth.
#'                  Irrigation refills to this level.
#' @param flood_min Numeric scalar. Minimum acceptable pond depth before
#'                  irrigation is triggered.
#' @param flood_max Numeric scalar. Maximum pond depth before bund
#'                  overflow / runoff occurs.
#' @param seepage   Numeric scalar. Daily seepage + percolation loss
#'                  Represents losses through the bund and downward percolation
#'                  through the hardpan (if any). Typical range: 1-5 mm/day
#'                  for well-puddled California soils.
#' @param drain     Logical vector (same length as et). TRUE on days when an
#'                  intentional drainage event occurs (e.g., mid-season drain,
#'                  pre-harvest drawdown). Pond is set to 0 on these days and
#'                  irrigation is suppressed.
#' @param pond_init Numeric scalar. Initial pond depth at t = 1.
#'                  Defaults to flood_target.
#'
#' @return A list with numeric vectors of length n:
#'   \item{pond_depth}{Ponded water depth at end of each day}
#'   \item{irr}{Irrigation applied}
#'   \item{runoff}{Bund overflow / surface runoff}
#'
#' @export
calc_water_balance_rice <- function(
  et,
  precip,
  flood_target,
  flood_min,
  flood_max,
  seepage,
  drain = NULL,
  pond_init = flood_target
) {
  n <- length(et)

  if (length(precip) != n) {
    PEcAn.logger::logger.severe("et and precip must be the same length")
  }
  if (is.null(drain)) {
    drain <- rep(FALSE, n)
  }
  if (length(drain) != n) {
    PEcAn.logger::logger.severe("drain must be the same length as et")
  }
  if (flood_min >= flood_target) {
    PEcAn.logger::logger.severe("flood_min must be less than flood_target")
  }
  if (flood_target >= flood_max) {
    PEcAn.logger::logger.severe("flood_target must be less than flood_max")
  }
  if (seepage < 0) {
    PEcAn.logger::logger.severe("seepage must be non-negative")
  }

  pond_depth <- numeric(n)
  irr <- numeric(n)
  runoff <- numeric(n)

  pond_prev <- pond_init

  for (t in seq_len(n)) {
    # --- Intentional drainage day -----------------------------------------
    # The field is deliberately drained (mid-season weed control, pre-harvest,
    # etc.). All water in the pond is released as managed drainage, counted
    # as runoff. Irrigation is suppressed for the day.
    if (drain[t]) {
      runoff[t] <- pond_prev + precip[t] # drain existing pond + any rain
      irr[t] <- 0
      pond_depth[t] <- 0
      pond_prev <- 0
      next
    }

    # --- Normal flooded day -----------------------------------------------

    # 1. Fluxes: precip adds, ET and seepage remove.
    #    Seepage is capped at available pond depth so we don't go below zero
    #    before irrigation is assessed.
    actual_seepage <- min(seepage, max(0, pond_prev))
    pond0 <- pond_prev + precip[t] - et[t] - actual_seepage

    # 2. Irrigation: refill to flood_target if pond drops below flood_min.
    #    Note that pond0 can be negative if ET is very high (e.g., early
    #    season before the pond is established). Irrigation covers the full
    #    deficit back to the target.
    if (pond0 < flood_min) {
      irr[t] <- flood_target - pond0
      pond0 <- flood_target
    } else {
      irr[t] <- 0
    }

    # 3. Runoff (bund overflow): any depth exceeding flood_max spills over.
    if (pond0 > flood_max) {
      runoff[t] <- pond0 - flood_max
      pond_depth[t] <- flood_max
    } else {
      runoff[t] <- 0
      pond_depth[t] <- max(pond0, 0)
    }

    pond_prev <- pond_depth[t]
  }

  list(pond_depth = pond_depth, irr = irr, runoff = runoff)
}

#' Apply water balance calculations to a data frame with multiple sites
#'
#' Groups by location and applies calc_water_balance to each group. Unlike
#' `calc_water_balance`, the units here *do* matter -- they should be `mm_day`.
#'
#' @param df Data frame with columns: `date`, `location_id`, `etc_mm_day`,
#' `precip_mm_day`, `crop_name`, and `whc_min_frac` (optional, defaults to
#' 0.375). If a `whc_mm` column is present, it is used as the water holding
#' capacity.
#' @param idcol Column name for grouping (typically, `location_id`, `parcel_id`
#' or similar).
#' @param whc_mm Water holding capacity (mm); ignored if `whc_mm` is a column
#' in `df`.
#' @param irrigation_max_mm Maximum irrigation to be applied at a time. See
#' `irrigation_max` argument of [calc_water_balance()]. Ignored if
#' `irrigation_max_mm` is a column of `df`.
#' @inheritParams calc_water_balance_rice
#' @return Data frame with added columns: `W_t` / `pond_depth`, `irr`, `runoff`
#' @export
apply_water_balance <- function(
  df,
  idcol,
  whc_mm = 500,
  irrigation_max_mm = 150,
  flood_target = 125,
  flood_min = 62.5,
  flood_max = 175,
  seepage = 2.5
) {
  need_cols <- c("etc_mm_day", "precip_mm_day", "date", "crop_name")
  missing_cols <- need_cols[!(need_cols %in% colnames(df))]
  default_whc_min_frac <- 0.375
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Missing the following required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if ("whc_min_frac" %in% colnames(df)) {
    n_na <- sum(is.na(df$whc_min_frac))
    if (n_na > 0) {
      PEcAn.logger::logger.warn(
        sprintf(
          "whc_min_frac has %d NA values. Replacing with default = %.3f",
          n_na,
          default_whc_min_frac
        )
      )
    }
  } else {
    PEcAn.logger::logger.warn(
      "whc_min_frac column not found in input data. Using default = ",
      default_whc_min_frac
    )
    df[["whc_min_frac"]] <- default_whc_min_frac
  }

  if (!("whc_mm" %in% colnames(df))) {
    df[["whc_mm"]] <- whc_mm
  }

  if (!("irrigation_max_mm" %in% colnames(df))) {
    df[["irrigation_max_mm"]] <- irrigation_max_mm
  }

  try_wb_rice <- function(...) {
    tryCatch(
      calc_water_balance_rice(...),
      error = function(e) {
        warning("Hit the following error: \n\n", e$message)
        list(
          pond_depth = NA_real_,
          irr = NA_real_,
          runoff = NA_real_
        )
      }
    )
  }

  rice <- df |>
    dplyr::filter(.data$crop_name == "Rice") |>
    dplyr::arrange(.data[[idcol]], .data$date) |> # nolint: object_usage_linter
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week = as.integer(format(.data$date, "%U")),
      day_of_year = as.integer(format(.data$date, "%j")),
      results = tibble::as_tibble(try_wb_rice(
        et = .data$etc_mm_day,
        precip = .data$precip_mm_day,
        flood_target = .env$flood_target,
        flood_min = .env$flood_min,
        flood_max = .env$flood_max,
        seepage = .env$seepage
      )),
      .by = dplyr::all_of(idcol)
    ) |>
    tidyr::unpack(.data$results)

  try_wb <- function(...) {
    tryCatch(
      calc_water_balance(...),
      error = function(e) {
        warning("Hit the following error: \n\n", e$message)
        list(
          W_t = NA_real_,
          irr = NA_real_,
          runoff = NA_real_
        )
      }
    )
  }

  others <- df |>
    dplyr::filter(.data$crop_name != "Rice") |>
    dplyr::arrange(.data[[idcol]], .data$date) |> # nolint: object_usage_linter
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week = as.integer(format(.data$date, "%U")),
      day_of_year = as.integer(format(.data$date, "%j")),
      whc_min_frac = tidyr::replace_na(
        .data$whc_min_frac,
        default_whc_min_frac
      ),
      results = tibble::as_tibble(try_wb(
        et = .data$etc_mm_day,
        precip = .data$precip_mm_day,
        whc = .data$whc_mm,
        whc_min_frac = .data$whc_min_frac,
        irrigation_max = .data$irrigation_max_mm
      )),
      .by = dplyr::all_of(idcol)
    ) |>
    tidyr::unpack(.data$results)

  dplyr::bind_rows(rice, others)
}
