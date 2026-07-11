#' Calculate the Nitrogen and Carbon Content of a Fertilizer Application
#'
#' This function calculates the different forms of nitrogen (NO3-N, NH4-N, organic N) and organic carbon (C_org) in a fertilizer application.
#' It can determine fertilizer nitrogen and carbon content using either a lookup table based on
#' the SWAT model's [`fertilizer.frt`](https://github.com/swat-model/swatplus/blob/main/data/Osu_1hru/fertilizer.frt)
#' file, determine the fertilizer's nutrient content based on NN-PP-KK format, or use user-specified
#' fractions of organic nitrogen and carbon.
#'
#' Consistent with assumptions in DayCent, DSSAT, and other models, urea is treated as NH3 because the
#' transformation typically occurs within a day.
#'
#' @param type Character string specifying the type of fertilizer. Valid values include NN-PP-KK format (e.g., "45-5-10") as well
#' as enumerated types including: "urea", "ammonium_nitrate", "compost", "manure", "dairy_fr", "beef_fr".
#' See notes for full list of valid types.
#' @param amount Numeric value specifying the amount of fertilizer applied in kg/ha.
#' @param fraction_organic_n Optional numeric value specifying the fraction of the organic matter that is nitrogen.
#' Used to define organic matter additions if not provided in the dataset.
#' @param fraction_organic_c Optional numeric value specifying the fraction of the organic matter that is carbon.
#' Used to define organic matter additions if not provided in the dataset.
#'
#' @md
#' @note The following is a list of valid fertilizer names:
#' - Mineral fertilizers: ammonium_nitrate, anhydrous_ammonia, urea
#' - Fresh manures: manure, beef_fr, broil_fr, dairy_fr, duck_fr, goat_fr, horse_fr,
#'      layer_fr, sheep_fr, swine_fr, trkey_fr, veal_fr
#' - Compost: org_compost
#'
#' @return A list containing:
#'   - `type`: The type of fertilizer used.
#'   - `NO3_N`: The amount of nitrate nitrogen (NO3-N) in kg/ha.
#'   - `NH4_N`: The amount of ammonium nitrogen (NH4-N) in kg/ha.
#'   - `N_org`: The amount of organic nitrogen in kg/ha.
#'   - `C_org`: The amount of organic carbon in kg/ha.
#'
#' @examples
#' # View all available fertilizer types
#' unique(PEcAn.data.land::fertilizer_composition_data$name)
#'
#' # Calculate components for different fertilizer types
#' look_up_fertilizer_components("urea", 200)
#' look_up_fertilizer_components("45-00-00", 200)
#' look_up_fertilizer_components("org_compost", 1000)
#' look_up_fertilizer_components("dairy_fr", 500)
#' look_up_fertilizer_components("manure", 1000, fraction_organic_n = 0.02, fraction_organic_c = 0.08)
#'
#' @export
look_up_fertilizer_components <- function(
    type,
    amount,
    fraction_organic_n = NULL,
    fraction_organic_c = NULL) {
  # Validate input for organic fertilizers
  if (!is.null(fraction_organic_n) || !is.null(fraction_organic_c)) {
    if (is.null(fraction_organic_n) || is.null(fraction_organic_c)) {
      PEcAn.logger::logger.severe("Both fraction_organic_n and fraction_organic_c must be provided if either is specified.")
      # could also make an assumption, but that seems error prone
    }
  }

  # If user provided organic matter fractions, use those regardless of whether they are in the database
  if (!is.null(fraction_organic_n) && !is.null(fraction_organic_c)) {
    return(list(
      type = type,
      NO3_N = 0,
      NH4_N = 0,
      N_org = round(amount * fraction_organic_n),
      C_org = round(amount * fraction_organic_c)
    ))
  }

  # If not in the database, check if the fertilizer type is in NN-PP-KK format (e.g., 45-5-10)
  if (stringr::str_detect(type, "^\\d{1,2}-\\d{1,2}-\\d{1,2}$")) {
    # Split NN-PP-KK format into components
    if (type %in% PEcAn.data.land::fertilizer_composition_data$name) {
      fraction_no3_n <- PEcAn.data.land::fertilizer_composition_data |>
        dplyr::filter(.data$name == type) |>
        dplyr::pull(.data$fraction_no3_n)
    } else {
      fraction_no3_n <- stringr::str_split(type, "-", simplify = TRUE)[1] |>
        as.numeric() / 100 # convert % to fraction (0-1)
    }
    # Assume all nitrogen is in the form of NO3_N, following SWAT assumptions in swat dataset
    return(list(
      type = type,
      NO3_N = round(amount * fraction_no3_n),
      NH4_N = 0,
      N_org = 0,
      C_org = 0
    ))
  }

  # Handle the case where the fertilizer type is in the database
  if (type %in% PEcAn.data.land::fertilizer_composition_data$name) {
    # Calculate the components directly in the data frame
    fertilizer_info <- PEcAn.data.land::fertilizer_composition_data |>
      dplyr::filter(.data$name == type) |>
      dplyr::mutate(
        NO3_N = round(amount * .data$fraction_no3_n),
        NH4_N = round(amount * .data$fraction_nh3_n),
        N_org = round(amount * .data$fraction_organic_n),
        C_org = round(amount * .data$fraction_c)
      )

    res <- fertilizer_info |>
      dplyr::select("name", "NO3_N", "NH4_N", "N_org", "C_org") |>
      dplyr::rename(type = "name") |>
      as.list()
    return(res)
  } else {
    PEcAn.logger::logger.error(paste("Fertilizer type", type, "not found in the database."))
    return(NULL)
  }
}
