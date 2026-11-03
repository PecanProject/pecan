#' Calculate effective available water capacity (mm) for a soil profile
#' clipped to a given rooting depth.
calc_effective_awc <- function(
  hzdept_r_cm,
  hzdepb_r_cm,
  awc_r,
  rooting_depth_cm
) {
  effective_top <- pmin(hzdept_r_cm, rooting_depth_cm)
  effective_bottom <- pmin(hzdepb_r_cm, rooting_depth_cm)
  thickness_cm <- pmax(0, effective_bottom - effective_top)
  # awc_r is cm water / cm soil;
  # multiply by thickness -> cm water -> mm water
  sum(awc_r * thickness_cm, na.rm = TRUE) * 10
}

add_soil_awc <- function(
  crop_info,
  ssurgo_weights_path,
  ssurgo_gdb_path
) {
  parcel_ids <- unique(crop_info[["parcel_id"]])
  message("Reading SSURGO weights")
  weights <- arrow::open_dataset(ssurgo_weights_path) |>
    dplyr::filter(.data$parcel_id %in% parcel_ids) |>
    dplyr::collect()

  component <- sf::read_sf(
    ssurgo_gdb_path,
    layer = "component",
    as_tibble = TRUE
  ) |>
    dplyr::semi_join(weights, by = "mukey")

  chorizon <- sf::read_sf(
    ssurgo_gdb_path,
    layer = "chorizon",
    as_tibble = TRUE
  ) |>
    dplyr::semi_join(component, by = "cokey")

  combined <- weights |>
    dplyr::left_join(
      dplyr::select(crop_info, "parcel_id", "rooting_depth_m"),
      by = "parcel_id",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      component,
      by = "mukey",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      chorizon,
      by = "cokey",
      relationship = "many-to-many"
    )

  awc <- combined |>
    dplyr::filter(
      !is.na(.data$awc_r),
      !is.na(.data$hzdept_r),
      !is.na(.data$hzdepb_r)
    ) |>
    dplyr::mutate(
      rooting_depth_cm = .data$rooting_depth_m * 100,
      .keep = "unused"
    ) |>
    # Aggregate horizons (by component)
    dplyr::summarize(
      whc_mm_cmp = calc_effective_awc(
        .data$hzdept_r,
        .data$hzdepb_r,
        .data$awc_r,
        .data$rooting_depth_cm
      ),
      .by = c("parcel_id", "mukey", "cokey", "area_m2", "weight", "comppct_r")
    ) |>
    # Aggregate components (by mapping unit)
    dplyr::summarize(
      whc_mm_mu = sum(
        .data$whc_mm_cmp * .data$comppct_r / sum(.data$comppct_r)
      ),
      .by = c("parcel_id", "mukey", "area_m2", "weight")
    ) |>
    # Aggregate mapping units (by parcel)
    dplyr::summarize(
      whc_mm = sum(.data$whc_mm_mu * .data$weight),
      .by = "parcel_id"
    )

  crops_with_soil <- dplyr::left_join(crop_info, awc, by = "parcel_id")
  crops_with_soil
}
