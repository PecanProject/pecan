test_that("`get_table_column_names()` able to return the column names of a table as a list",{
  mocked_res <- mockery::mock(data.frame(head1 = 1, head2 = 2))
  mockery::stub(get_table_column_names, 'PEcAn.DB::db.query', mocked_res)
  res <- get_table_column_names(table = data.frame(table_name = 'test_table'), con = 1)
  args <- mockery::mock_args(mocked_res)
  expect_equal(args[[1]][[1]], "SELECT * from test_table LIMIT 1")
  expect_equal(res, list(test_table = c("head1", "head2")))
})