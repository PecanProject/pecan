test_that("`.download.raw.met.module` throws an error if register$site is unknown", {
  register <- list(scale = 'example')
  expect_error(
    .download.raw.met.module(dir = NULL, met = NULL, register = register, str_ns = NULL), 
    "Unknown register\\$scale"
  )
})

test_that("`.download.raw.met.module` is able to call the right download function based on met value", {
  mocked_res <- mockery::mock(10)
  mockery::stub(.download.raw.met.module, 'PEcAn.DB::convert_input', mocked_res)
  register <- list(scale = 'site')
  res <- .download.raw.met.module(
          dir = NULL,
          met = 'ERA5',
          register = register,
          machine = NULL,
          start_date = '2010-01-01',
          end_date = '2010-01-01',
          str_ns = NULL,
          con = NULL,
          input_met = NULL,
          site.id = 1,
          lat.in = 1,
          lon.in = 1,
          host = NULL,
          site = NULL,
          username = NULL,
          dbparms = NULL)
  args <- mockery::mock_args(mocked_res)
  mockery::expect_called(mocked_res, 1)
  expect_equal(res, 10)
  expect_equal(args[[1]]$fcn, 'download.ERA5')
})
test_that("registration defaults and prefix identify site-level climate products", {
  convert <- mockery::mock(list(input.id = 1, dbfile.id = 2))
  mockery::stub(.download.raw.met.module, "PEcAn.DB::convert_input", convert)
  register <- list(scale = "site", defaults = list(model = "CESM2", scenario = "ssp370", resolution = "d03"),
    prefix = "CalAdaptWRF.{model}.{scenario}.{resolution}")
  .download.raw.met.module(dir = tempdir(), met = "CalAdaptWRF", register = register,
    start_date = "2025-01-01", end_date = "2025-12-31", str_ns = "001-field", con = NULL,
    input_met = list(model = NULL, resolution = "d01", cache_dir = "/cache"), site.id = 1,
    lat.in = 38, lon.in = -121, host = NULL, site = list(), username = NULL, dbparms = NULL)
  args <- mockery::mock_args(convert)[[1]]
  expect_identical(args$model, "CESM2")
  expect_identical(args$scenario, "ssp370")
  expect_identical(args$resolution, "d01")
  expect_identical(args$cache_dir, "/cache")
  expect_match(args$outfolder, "CalAdaptWRF.CESM2.ssp370.d01_site_001-field", fixed = TRUE)
  expect_true(grepl(args$pattern, "CalAdaptWRF.CESM2.ssp370.d01"))
  expect_false(grepl(args$pattern, "CalAdaptWRF.CESM2.ssp245.d01"))
  expect_false(grepl(args$pattern, "CalAdaptWRF.CESM2.ssp370.d03"))
})
