test_that("`convert_input()` able to call the respective download function for a data item with the correct arguments", {
  mocked_res <- mockery::mock(list(c("A", "B")))

  mockery::stub(convert_input, 'dbfile.input.check', data.frame())
  mockery::stub(convert_input, 'db.query', data.frame(id = 1))
  mockery::stub(convert_input, 'PEcAn.remote::remote.execute.R', mocked_res)
  mockery::stub(convert_input, 'purrr::map_dfr', data.frame(missing = c(FALSE), empty = c(FALSE)))

  convert_input(
    input.id = NA,
    outfolder = "test",
    formatname = NULL,
    mimetype = NULL,
    site.id = 1,
    start_date = "2011-01-01",
    end_date = "2011-12-31",
    pkg = 'PEcAn.data.atmosphere',
    fcn = 'download.AmerifluxLBL',
    con = NULL,
    host = data.frame(name = "localhost"),
    write = FALSE,
    lat.in = 40,
    lon.in = -88
  )
  
  args <- mockery::mock_args(mocked_res)
  expect_equal(
    args[[1]]$script, 
    "PEcAn.data.atmosphere::download.AmerifluxLBL(lat.in=40, lon.in=-88, overwrite=FALSE, outfolder='test/', start_date='2011-01-01', end_date='2011-12-31')"
  )
})

test_that("`.get.file.deletion.commands()` able to return correct file deletion commands", {
  res <- .get.file.deletion.commands(c("test"))
  expect_equal(res$move.to.tmp, "dir.create(c('./tmp'), recursive=TRUE, showWarnings=FALSE); file.rename(from=c('test'), to=c('./tmp/test'))")
  expect_equal(res$delete.tmp, "unlink(c('./tmp'), recursive=TRUE)")
  expect_equal(res$replace.from.tmp, "file.rename(from=c('./tmp/test'), to=c('test'));unlink(c('./tmp'), recursive=TRUE)")
})