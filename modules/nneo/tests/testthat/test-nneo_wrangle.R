context("nneo_wrangle")

test_that("nneo_data works as expected", {
  skip_on_cran()

  aa <- nneo_wrangle(site_code="BART", time_start="2016-06-20",
                     time_end="2016-09-21", data_var="radiation")

  expect_is(aa, 'tbl_df')
  expect_is(aa$difRadMean.000.060, 'numeric')
  expect_is(aa$startDateTime, 'character')

})
