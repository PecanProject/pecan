test_that("`query.file.path()`", {
  # mock responses for subsequent calls to db.query
  mocked_res <- mockery::mock(data.frame(id = '20210101'), data.frame(file_name = 'test_file', file_path = 'test_path'))
  mockery::stub(query.file.path, 'db.query', mocked_res)
  mockery::stub(query.file.path, 'PEcAn.remote::remote.execute.R', TRUE)
  res <- query.file.path(input.id = 1, host_name = "pecan", con = 1)
  args <- mockery::mock_args(mocked_res)
  expect_true(
    grepl(
      "where hostname = 'pecan'",
      args[[1]]$query
    )
  )
  expect_true(
    grepl(
      "container_id = 1.* machine_id = 20210101",
      args[[2]]$query
    )
  )
  expect_equal(res, 'test_path/test_file')
})