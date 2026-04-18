test_that("no-argument call returns zero", {
  expect_equal(to_co2e(), 0)
})

test_that("SOC gain is returned as negative CO2e", {
  expect_equal(to_co2e(delta_soc = 1), -(44 / 12))
})

test_that("negative delta_soc returns positive CO2e", {
  expect_equal(to_co2e(delta_soc = -1), 44 / 12)
})

test_that("CH4 conversion uses expected GWP100 values for specified gwp version", {
  expect_equal(to_co2e(ch4 = 1, gwp = "AR4"), 25)
  expect_equal(to_co2e(ch4 = 1, gwp = "AR5"), 28)
  expect_equal(to_co2e(ch4 = 1, gwp = "AR6"), 29.8)
})

test_that("N2O conversion uses expected GWP100 values for specified gwp version", {
  expect_equal(to_co2e(n2o = 1, gwp = "AR4"), 298)
  expect_equal(to_co2e(n2o = 1, gwp = "AR5"), 265)
  expect_equal(to_co2e(n2o = 1, gwp = "AR6"), 273)
})

test_that("to_co2e correctly sums across delta SOC, N2O, and CH4", {
  delta_soc_co2e <- to_co2e(delta_soc = 1)
  n2o_co2e <- to_co2e(n2o = 1)
  ch4_co2e <- to_co2e(ch4 = 1)
  all_co2e <- to_co2e(delta_soc = 1, ch4 = 1, n2o = 1)
  expect_equal(all_co2e, sum(delta_soc_co2e, n2o_co2e, ch4_co2e))
})

test_that("unsupported GWP values error", {
  expect_error(to_co2e(gwp = "BAD"), "AR6.*AR5.*AR4")
})
