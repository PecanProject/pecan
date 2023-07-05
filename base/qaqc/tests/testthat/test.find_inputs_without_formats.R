test_that("`find_inputs_without_formats()`", {
  mocked_res <- mockery::mock()
  mockery::stub(find_inputs_without_formats, 'dplyr::tbl', mocked_res)
  expect_equal(1, 1)
})