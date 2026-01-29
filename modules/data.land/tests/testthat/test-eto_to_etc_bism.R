test_that("eto_to_etc multiplies and validates", {
  expect_equal(eto_to_etc(c(1, 2), c(0.5, 1)), c(0.5, 2))
  expect_equal(eto_to_etc(c(1, 2), 2), c(2, 4))
  expect_error(eto_to_etc("a", 1), "numeric")
  expect_error(eto_to_etc(1:2, 1:3), "length 1")
})

data("bism_kc_by_crop", package = "PEcAn.data.land")

test_that("eto_to_etc_bism handles date-based anchors", {
  kc_row <- bism_kc_by_crop |>
    dplyr::filter(.data$crop_name == "Beets (table)")
  expect_equal(nrow(kc_row), 1)

  planting <- lubridate::make_date(
    2020,
    kc_row$planting_month,
    kc_row$planting_day
  )
  harvest <- lubridate::make_date(
    2020,
    kc_row$harvest_month,
    kc_row$harvest_day
  )

  season_length <- as.numeric(harvest - planting)

  offsets <- round(season_length * c(
    kc_row$percent_season_B,
    kc_row$percent_season_C,
    kc_row$percent_season_D,
    100
  ) / 100)
  date <- planting + offsets
  eto <- rep(5, length(date))
  expected_kc <- c(kc_row$KcB, kc_row$KcC, kc_row$KcD, kc_row$KcE)

  etc_date <- eto_to_etc_bism(eto, crop_name = kc_row$crop_name, date = date)
  expect_equal(etc_date, eto * expected_kc)
})

test_that("eto_to_etc_bism handles canopy-cover rules", {
  kc_row <- bism_kc_by_crop |>
    dplyr::filter(.data$crop_name == "Beets (table)")
  eto <- rep(4, 3)
  etc_field <- eto_to_etc_bism(
    eto,
    crop_name = kc_row$crop_name,
    canopy_cover = c(0.10, 0.75, 0.90)
  )
  expect_equal(etc_field, eto * c(kc_row$KcB, kc_row$KcC, kc_row$KcC))

  kc_tree <- bism_kc_by_crop |>
    dplyr::filter(.data$crop_name == "Apple")
  expect_equal(nrow(kc_tree), 1)
  eto_tree <- rep(4, 2)
  etc_tree <- eto_to_etc_bism(
    eto_tree,
    crop_name = kc_tree$crop_name,
    canopy_cover = c(0, 0.70)
  )
  expect_equal(etc_tree, eto_tree * c(0, kc_tree$KcC))
})

test_that("eto_to_etc_bism requires exactly one timing input (either date or canopy_cover)", {
  # Missing both
  expect_error(
    eto_to_etc_bism(1, crop_name = "Beets (table)"),
    "Provide exactly one"
  )
  # Providing both
  expect_error(
    eto_to_etc_bism(1, crop_name = "Beets (table)", date = "1975-08-26", canopy_cover = 0.5),
    "Provide exactly one"
  )
})
