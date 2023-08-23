library(testthat)

test_download_AmerifluxLBL <- function(start_date, end_date, sitename, lat.in, lon.in) {
  # putting logger to debug mode
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  PEcAn.logger::logger.setLevel("DEBUG")

  # mocking functions
  mockery::stub(PEcAn.DB::convert_input, 'dbfile.input.check', data.frame())
  mockery::stub(PEcAn.DB::convert_input, 'db.query', data.frame(id = 1))

  withr::with_dir(tempdir(), {
    tmpdir <- getwd()
    # calling download function
    PEcAn.DB::convert_input(
      input.id = NA,
      outfolder = tmpdir,
      formatname = NULL,
      mimetype = NULL,
      site.id = 1,
      start_date = start_date,
      end_date = end_date,
      pkg = 'PEcAn.data.atmosphere',
      fcn = 'download.AmerifluxLBL',
      con = NULL,
      host = data.frame(name = "localhost"),
      browndog = NULL,
      write = FALSE,
      lat.in = lat.in,
      lon.in = lon.in,
      sitename = sitename
    )
  })
  
  # checking if the file is downloaded
  test_that("Downloaded files are present in the desired location", {
    expect_true(file.exists(paste0(tmpdir, "/AMF_US-Akn_BASE_HH_6-5.csv")))
  })
}

test_download_AmerifluxLBL(
  start_date = "2011-01-01",
  end_date = "2011-12-31",
  sitename = 'US-Akn',
  lat.in = 40,
  lon.in = -88
)