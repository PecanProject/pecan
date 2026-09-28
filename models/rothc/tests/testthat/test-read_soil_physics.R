
# Helper: Send test values to a soil netcdf,
# read back using read_soil_physics().
# Note that it recycles all inputs to common length
nc_roundtrip <- function(depth = c(15, 30, 60), # cm
                         clay = 1 / 3, # frac
                         silt = 1 / 3, # frac
                         bulk = 1350, # kg m-3
                         oc = 2, # kg m-2
                         model_depth = 30) { # cm
  with_mocked_bindings(
    {
      read_soil_physics("path/ignored", model_depth = model_depth)
    },
    netcdf2df = function(...) {
      dat <- data.frame(
        depth = PEcAn.utils::ud_convert(depth, "cm", "m"),
        fraction_of_clay_in_soil = clay,
        fraction_of_silt_in_soil = silt,
        soil_bulk_density = bulk,
        soil_organic_carbon_stock = oc
      )
      attr(dat, "units") <- c(
        depth = "meters",
        fraction_of_clay_in_soil = "1",
        fraction_of_silt_in_soil = "1",
        soil_bulk_density = "kg m-3",
        soil_organic_carbon_stock = "kg m-2"
      )

      dat
    }
  )
}

test_that("one layer", {
  res <- nc_roundtrip(depth = 30)
  expect_equal(res$clay_pct, 33.3333, tolerance = 1e-3)
  expect_equal(res$bulkdens_g_cm3, 1.35)
  # (stock in g cm-3) / (bulk * thickness) * 100%
  expect_equal(res$org_C_pct, (2 / 10) / (1.35 * 30) * 100, tolerance = 1e-6)

  expect_equal(
    nc_roundtrip(depth = 30, model_depth = 30) |>
      dplyr::select(-"depth_cm", -"org_C_pct"),
    nc_roundtrip(depth = 20, model_depth = 20) |>
      dplyr::select(-"depth_cm", -"org_C_pct")
  )
  expect_equal(
    nc_roundtrip(depth = 30, model_depth = 30) |>
      dplyr::select(-"depth_cm", -"iom_tC_ha"),
    nc_roundtrip(depth = 20, model_depth = 20, oc = 2 * 20 / 30) |>
      dplyr::select(-"depth_cm", -"iom_tC_ha"),
    tolerance = 1e-6
  )
})

test_that("two layers", {
  res <- nc_roundtrip(depth = c(15, 30), clay = c(0.1, 0.2), oc = c(2, 4))
  expect_equal(res$clay_pct, 15, tolerance = 1e-3)
  expect_equal(res$bulkdens_g_cm3, 1.35)
  expect_equal(res$org_C_pct, ((2 + 4) / 10) / (1.35 * 30) * 100)
})

test_that("n layers", {
  expect_equal(
    nc_roundtrip(depth = 30),
    nc_roundtrip(depth = 1:30, oc = 2 / 30),
    tolerance = 1e-6
  )
})

test_that("deeper layers ignored", {
  expect_equal(
    nc_roundtrip(depth = 30, clay = 0.5, oc = 2),
    nc_roundtrip(depth = c(30, 60), clay = c(0.5, 0.1), oc = c(2, 20)),
    tolerance = 1e-6
  )
})


test_that("detects unit errors", {
  expect_output(
    nc_roundtrip(depth = c(1500, 3000, 6000)),
    "Assuming the units have been mislabeled"
  )
})
