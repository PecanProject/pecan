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