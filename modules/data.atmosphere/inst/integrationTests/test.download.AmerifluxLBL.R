# putting logger to debug mode
PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
PEcAn.logger::logger.setLevel("DEBUG")


# mocking functions
mockery::stub(PEcAn.DB::convert_input, 'dbfile.input.check', data.frame())
mockery::stub(PEcAn.DB::convert_input, 'db.query', data.frame(id = 1))

# calling download function
PEcAn.DB::convert_input(
  input.id = NA,
  outfolder = "testAmerifluxData",
  formatname = NULL,
  mimetype = NULL,
  site.id = 1,
  start_date = "2011-01-01",
  end_date = "2011-12-31",
  pkg = 'PEcAn.data.atmosphere',
  fcn = 'download.AmerifluxLBL',
  con = NULL,
  host = data.frame(name = "localhost"),
  browndog = NULL,
  write = FALSE,
  lat.in = 40,
  lon.in = -88,
  sitename = 'US-Akn'
)

# checking the downloaded files
