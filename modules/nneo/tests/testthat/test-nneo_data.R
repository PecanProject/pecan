context("nneo_data")

test_that("nneo_data works as expected", {
  skip_on_cran()

  aa <- nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
                  year_month = "2016-05")

  expect_is(aa, 'list')

  # there's no longer a status slot apparently
  # expect_type(aa$status, 'integer')

  expect_is(aa$data, 'list')
  expect_is(aa$data$files, 'data.frame')
  expect_is(aa$data$files, 'tbl_df')
  expect_is(aa$data$productCode, 'character')

  # there's no longer a urls slot apparently
  # expect_is(aa$data$urls, 'character')
})


test_that("nneo_data fails well", {
  skip_on_cran()

  expect_error(nneo_data(), "argument \"product_code\" is missing")
  expect_error(nneo_data(product_code = "DP1.00098.001"),
               "argument \"site_code\" is missing")
  expect_error(nneo_data(product_code = "DP1.00098.001", site_code = "HEAL"),
               "argument \"year_month\" is missing")
  expect_error(nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
                         year_month = 44),
               "Bad Request \\(HTTP 400\\)")
})
