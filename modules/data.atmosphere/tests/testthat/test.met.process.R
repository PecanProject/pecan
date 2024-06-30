test_that("`met.process` able to call .download.raw.met.module based on met process stage params", {
  input_met <- list(source = 'CRUNCEP', id = '1')

  mockery::stub(met.process, 'PEcAn.DB::db.open', 1)
  mockery::stub(met.process, 'PEcAn.DB::db.close', 1)
  mockery::stub(met.process, 'PEcAn.DB::db.query', list(file_path = '/test/path', file_name = 'test'))
  mockery::stub(met.process, 'read.register', list())
  mockery::stub(met.process, 'PEcAn.DB::query.format.vars', list())
  mockery::stub(met.process, 'PEcAn.DB::dbfile.check', list(id = 1))
  mockery::stub(met.process, 'assign', 1)
  mockery::stub(met.process, 'PEcAn.DB::query.site', list(lat = 0, lon = 0))
  mockery::stub(met.process, 'met.process.stage', list(download.raw = TRUE, met2cf = FALSE, standardize = FALSE, met2model = FALSE))

  mocked_res <- mockery::mock(1)
  mockery::stub(met.process, '.download.raw.met.module', mocked_res)
  res <- met.process(
    site = list(id =  1),
    input_met = input_met,
    start_date = '2001-01-01',
    end_date = '2003-01-01',
    model = 'ED2',
    dbparms = list(),
    dir = 'test')
  mockery::expect_called(mocked_res, 1) 
  expect_equal(res$path$path1, '/test/path/test')
})
