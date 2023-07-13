test_that("`cull_database_entries()` gives errors for faulty inputs",{
  expect_error(
    cull_database_entries(outdir = 'test'),
    "If a table object hasn't been provided, a file_name must be set."
  )
  expect_error(
    cull_database_entries(table = 'test_table', file_name = 'test_file', outdir = 'test'),
    "table and file_name cannot both be provided."
  )
  expect_error(
    cull_database_entries(table = 'test_table', outdir = 'test'),
    "Please provide a table_name"
  )
})

test_that("`cull_database_entries()` able to correctly add logs to the output file", {
  withr::with_dir(tempdir(), {
    mockery::stub(cull_database_entries, 'PEcAn.DB::db.query', 'test_log')
    dir <- getwd()
    cull_database_entries(table = data.frame(id = 1), table_name = 'test', con = 1, outdir = dir)
    expect_true(file.exists(paste0(dir, "/deletion_log_of_test")))
    file_data <- readLines(paste0(dir, "/deletion_log_of_test"))
    expect_equal(grepl("test_log", file_data), c(TRUE, TRUE))
  })
})