test_that("`stamp_started()` able to correctly update the query for run_id passed", {
  mock_function <- mockery::mock()
  mockery::stub(stamp_started, 'PEcAn.DB::db.query', mock_function)
  stamp_started(1, 1)
  args <- mockery::mock_args(mock_function)
  expect_true(grepl("started_at .* WHERE id =  1", args[[1]]$query))
})

test_that("`stamp_finished()` able to correctly update the query for run_id passed", {
  mock_function <- mockery::mock()
  mockery::stub(stamp_finished, 'PEcAn.DB::db.query', mock_function)
  stamp_finished(1, 1)
  args <- mockery::mock_args(mock_function)
  expect_true(grepl("finished_at .* WHERE id =  1", args[[1]]$query))
})