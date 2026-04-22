#' Look up PFT assignments by LandIQ crop code
#'
#' Returns PFT assignments for LandIQ CLASS/SUBCLASS codes.
#'
#' @param crop_type Character vector of LandIQ CLASS codes.
#' @param crop_code Character vector of LandIQ SUBCLASS codes, same length as
#'   `crop_type` or length one. Use NA for class-only lookup.
#' @param output One of "all" (default), "pft_group", "pecan_pft".
#'
#' @return A tibble with one row per input. Columns: `crop_type`, `crop_code`,
#'   `crop_desc`, and `pft_group` and/or `pecan_pft` depending on `output`.
#'
#' @examples
#' look_up_crop_pft("D", "12")
#' look_up_crop_pft(c("D", "G", "R"), c("12", "2", "1"))
#' look_up_crop_pft("F", "6", output = "pecan_pft")
#'
#' @export
look_up_crop_pft <- function(
    crop_type,
    crop_code = NA_character_,
    output = c("all", "pft_group", "pecan_pft")
) {
  output <- match.arg(output)

  if (length(crop_code) == 1 && length(crop_type) > 1) {
    crop_code <- rep(crop_code, length(crop_type))
  }
  if (length(crop_type) != length(crop_code)) {
    PEcAn.logger::logger.severe(
      "`crop_type` and `crop_code` must have the same length (got ",
      length(crop_type), " vs ", length(crop_code), ")."
    )
  }

  query <- tibble::tibble(
    crop_type = as.character(crop_type),
    crop_code = as.character(crop_code)
  )

  dat <- PEcAn.data.land::carb_landiq_crop_pft

  bad_types <- unique(query$crop_type[
    !is.na(query$crop_type) & !query$crop_type %in% dat$crop_type
  ])
  if (length(bad_types) > 0) {
    PEcAn.logger::logger.warn(
      "Unrecognized LandIQ CLASS codes: ",
      paste(bad_types, collapse = ", "), "."
    )
  }

  result <- query |>
    dplyr::left_join(dat, by = c("crop_type", "crop_code"))

  if (output == "pft_group") {
    result <- dplyr::select(
      result, "crop_type", "crop_code", "crop_desc", "pft_group"
    )
  } else if (output == "pecan_pft") {
    result <- dplyr::select(
      result, "crop_type", "crop_code", "crop_desc", "pecan_pft"
    )
  }

  result
}
