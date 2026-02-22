list2nc2list <- function(depth,
                         sand = 0, clay = 0, silt = NULL,
                         ...) {
  if (is.null(silt)) {
    silt <- 1 - (sand + clay)
  }
  nc <- withr::local_tempfile()
  data.frame(
    fraction_of_sand_in_soil = sand,
    fraction_of_silt_in_soil = silt,
    fraction_of_clay_in_soil = clay,
    soil_depth = depth,
    ...
  ) |>
    soil2netcdf(nc)

  pool_ic_netcdf2list(nc)
}

test_that("retains depth dimension even when only one layer", {
  expect_equal(
    list2nc2list(depth = 0.1)$dims$depth,
    array(0.1)
  )
  expect_equal(
    list2nc2list(1:2, c(0.5, 0.8), c(0.4, 0.1))$dims$depth,
    array(c(1, 2))
  )
})

test_that("parameters estimated if missing, but not overwritten", {
  expect_equal(list2nc2list(0.15, 0.3, 0.6)$vals$thcond0, array(0.7636137604))
  expect_equal(
    list2nc2list(0.15, 0.3, 0.6, thcond0 = 1.0)$vals$thcond0,
    array(1.0)
  )
})

test_that("component length recycling enforced before netcdf write", {
  soil.data <- list(
    soil_depth = c(0.1, 0.3, 0.6),              # length 3
    fraction_of_sand_in_soil = c(0.4, 0.5),     # length 2 (mismatch)
    fraction_of_clay_in_soil = c(0.2, 0.2, 0.2) # length 3
  )
  path <- withr::local_tempfile()
  expect_error(soil2netcdf(soil.data, path), "arguments imply differing number of rows")
  expect_false(file.exists(path))
})
