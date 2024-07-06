test_that("`runModule.run.meta.analysis` throws an error for incorrect input", {
  expect_error(runModule.run.meta.analysis('test'), "only works with Settings or MultiSettings")
})

test_that("`run.meta.analysis` able to call run.meta.analysis.pft for each pft in the input list", {
  mocked_res <- mockery::mock(1, cycle = TRUE)
  mockery::stub(run.meta.analysis, 'run.meta.analysis.pft', mocked_res)
  mockery::stub(run.meta.analysis, 'PEcAn.DB::db.open', 1)
  mockery::stub(run.meta.analysis, 'PEcAn.DB::db.close', 1)
  pfts <- list('ebifarm.salix', 'temperate.coniferous')
  run.meta.analysis(pfts = pfts, iterations = 1, dbfiles = NULL, database = NULL)
  mockery::expect_called(mocked_res, 2)
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "ebifarm.salix")
  expect_equal(args[[2]][[1]], "temperate.coniferous")
})

test_that("`run.meta.analysis.pft`", {
  
})