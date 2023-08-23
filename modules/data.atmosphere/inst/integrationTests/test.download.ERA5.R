library(testthat)

test_download_ERA5 <- function(start_date, end_date, lat.in, lon.in, product_types, reticulate_python) {
  # putting logger to debug mode
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  PEcAn.logger::logger.setLevel("DEBUG")


  # mocking functions
  mockery::stub(PEcAn.DB::convert_input, 'dbfile.input.check', data.frame())
  mockery::stub(PEcAn.DB::convert_input, 'db.query', data.frame(id = 1))

  withr::with_dir(tempdir(), {
    tmpdir <- getwd()
    PEcAn.DB::convert_input(
      input.id = NA,
      outfolder = tmpdir,
      formatname = NULL,
      mimetype = NULL,
      site.id = 1,
      start_date = start_date,
      end_date = end_date,
      pkg = 'PEcAn.data.atmosphere',
      fcn = 'download.ERA5.old',
      con = NULL,
      host = data.frame(name = "localhost"),
      browndog = NULL,
      write = FALSE,
      lat.in = lat.in,
      lon.in = lon.in,
      product_types = product_types,
      reticulate_python = reticulate_python
    )
  })
}

test_download_ERA5(
  start_date = "2010-01-01",
  end_date = "2010-02-01",
  lat.in = 45.5594,
  lon.in = -84.6738,
  product_types = "all",
  reticulate_python = NULL
)