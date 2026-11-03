#' Convert MSLSP phenology data to tidy canopy cover
#'
#' @param mslsp_path Path to directory containing MSLSP outputs (in parquet format)
#' @param parcel_ids Vector of parcel IDs for filtering. If `NULL`, use all parcels.
#' @param years Vector of years for filtering. If `NULL`, use all years.
#'
#' @return `data.frame` of `parcel_id`, `year`, `season`, `date`, and
#' `canopy_cover` (fraction). `date` is a sequence from the MSLSP greenness
#' onset (growing season start) to the greenness minimum (growing season end).
#' `canopy_cover` is the fractional canpoy cover (0 to 1), suitable for ingest
#' into [eto_to_etc_bism()] in "canopy cover mode".
#' @export
mslsp_to_canopycover <- function(mslsp_path, parcel_ids = NULL, years = NULL) {
  stopifnot(dir.exists(mslsp_path))
  mslsp_files <- list.files(mslsp_path, "\\.parquet", full.names = TRUE)
  mslsp_dat <- arrow::open_dataset(mslsp_files)
  if (!is.null(parcel_ids)) {
    mslsp_dat <- mslsp_dat |>
      dplyr::filter(.data[["parcel_id"]] %in% unique(parcel_ids))
  }
  if (!is.null(years)) {
    mslsp_dat <- mslsp_dat |>
      dplyr::filter(.data[["year"]] %in% years)
  }
  common_cols <- c(
    "parcel_id",
    "year",
    "season",
    "mslsp_cycle",
    "landiq_PFT",
    "landiq_CLASS",
    "landiq_SUBCLASS"
  )
  mslsp_tbl <- mslsp_dat |>
    dplyr::filter(!is.na(.data[["mslsp_cycle"]])) |>
    dplyr::select(
      dplyr::all_of(common_cols),
      dplyr::all_of(.MSLSP_DATE_MAPPING[["date_name"]])
    ) |>
    dplyr::collect() |>
    tibble::as_tibble()

  result <- mslsp_tbl |>
    dplyr::mutate(
      cc_nested = purrr::pmap(
        dplyr::pick(dplyr::all_of(.MSLSP_DATE_MAPPING[["date_name"]])),
        \(...) expand_mslsp_cycle(list(...)),
        .progress = TRUE
      )
    ) |>
    dplyr::select(dplyr::all_of(common_cols), "cc_nested") |>
    tidyr::unnest("cc_nested")

  result
}

expand_mslsp_cycle <- function(mslsp_row) {
  dates <- as.Date(unlist(mslsp_row[.MSLSP_DATE_MAPPING[["date_name"]]]))
  all_dates <- seq(min(dates), max(dates), by = "1 day")
  tibble::tibble(
    date = all_dates,
    canopy_cover = stats::approx(
      x = dates,
      y = .MSLSP_DATE_MAPPING$canopy_cover,
      xout = all_dates
    )[["y"]]
  )
}

.MSLSP_DATE_MAPPING <- tibble::tribble(
  ~date_name, ~canopy_cover,
  "mslsp_OGI", 0.15,
  "mslsp_50PCGI", 0.5,
  "mslsp_OGMx", 0.9,
  "mslsp_Peak", 1.0,
  "mslsp_OGD", 0.9,
  "mslsp_50PCGD", 0.5,
  "mslsp_OGMn", 0.15
)
