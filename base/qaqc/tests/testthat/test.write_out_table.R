test_that("`write_out_table()` able to create and update output file with relevant data",{
  dir <- tempdir()
  on.exit(unlink(dir, recursive = TRUE))
  write_out_table(
    table = data.frame(id = 1, table_name = 'test'), table_name = 'test', relevant_table_columns = c(), outdir = dir
  )
  expect_true(file.exists(paste0(dir, "/query_of_test")))
  file_data <- readLines(paste0(dir, "/query_of_test"))
  expect_equal(grepl("test", file_data), c(FALSE, TRUE))
})