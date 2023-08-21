library(testthat)

# putting logger to debug mode
PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
PEcAn.logger::logger.setLevel("DEBUG")


# mocking functions
mockery::stub(PEcAn.DB::convert_input, 'dbfile.input.check', data.frame())
mockery::stub(PEcAn.DB::convert_input, 'db.query', data.frame(id = 1))

tmpdir <- tempfile(pattern = "era5Data")
dir.create(tmpdir)
on.exit(teardown(unlink(tmpdir, recursive = TRUE)))

PEcAn.DB::convert_input(
  input.id = NA,
  outfolder = tmpdir,
  formatname = NULL,
  mimetype = NULL,
  site.id = 1,
  start_date = "2010-01-01",
  end_date = "2010-02-01",
  pkg = 'PEcAn.data.atmosphere',
  fcn = 'download.ERA5.old',
  con = NULL,
  host = data.frame(name = "localhost"),
  browndog = NULL,
  write = FALSE,
  lat.in = 45.5594,
  lon.in = -84.6738,
  product_types <- "all",
  reticulate_python <- NULL
)

