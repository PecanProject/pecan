test_that("`find_formats_without_inputs()` able to find formats with no input record",{
  format_command_mock <- data.frame(user_id = '2020', created_at = '2001-01-01', updated_at = '2010-01-01')
  input_command_mock <- data.frame(format_id = '2000', user_id = '2021', created_at = '2002-01-02', updated_at = '2012-01-02')
  mocked_res <- mockery::mock(input_command_mock, format_command_mock)
  mockery::stub(find_formats_without_inputs, 'dplyr::tbl', mocked_res)

  res <- find_formats_without_inputs(
    con = NULL, user_id_code = '2020', created_after = '2000-01-01', updated_after = '2009-01-01', created_before = '2002-01-01', updated_before = '2011-01-01'
  )
  expect_equal(
    res, 
    data.frame(id = '2020', created_at = '2001-01-01', updated_at = '2010-01-01', table_name = "formats")
  )
})