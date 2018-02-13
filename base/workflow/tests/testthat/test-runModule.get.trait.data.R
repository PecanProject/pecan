
context("testing workflow functions")

settings <- PEcAn.settings::Settings(list(
  outdir = "/dev/null",
  database = list(bety = list(), dbfiles = "fake_path"),
  pfts = list(pft = list(name = "fake", outdir = "fake_pft_path")),
  meta.analysis = list(threshold = 1, iter = 1)))

test_that("settings not changed if no MA", {
  settings_noMA <- settings
  settings_noMA$meta.analysis <- NULL
  expect_identical(settings_noMA, runModule.get.trait.data(settings_noMA))
})

test_that("get.trait.data is called exactly once", {
  mock_vals <- mockery::mock(settings$pfts, cycle=TRUE)
  mockery::stub(
    where = runModule.get.trait.data,
    what = "PEcAn.DB::get.trait.data",
    how = mock_vals)

  res <- runModule.get.trait.data(settings)
  expect_equal(res, settings)
  mockery::expect_called(mock_vals, 1)

  settings_3 <- PEcAn.settings::MultiSettings(settings, settings, settings)

  res_multi <- runModule.get.trait.data(settings_3)
  expect_equal(length(res_multi), 3)
  expect_identical(res_multi[[1]], settings)
  mockery::expect_called(mock_vals, 2)
})