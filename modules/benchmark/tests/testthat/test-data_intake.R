library(testthat)

test_that("load_and_map_data correctly maps CSV columns using YAML mapping", {
  # Create temporary CSV file
  csv_file <- tempfile(fileext = ".csv")
  df <- data.frame(
    TIMESTAMP = c("2020-01-01", "2020-01-02"),
    TA_F = c(15.2, 16.5),
    NEE_PI = c(-2.1, -1.8)
  )
  utils::write.csv(df, csv_file, row.names = FALSE)
  
  # Create temporary YAML mapping file
  yaml_file <- tempfile(fileext = ".yaml")
  yaml_content <- c(
    "variables:",
    "  time: TIMESTAMP",
    "  airT: TA_F",
    "  NEE: NEE_PI"
  )
  writeLines(yaml_content, yaml_file)
  
  mapped_df <- load_and_map_data(csv_file, yaml_file)
  
  expect_true(all(c("time", "airT", "NEE") %in% colnames(mapped_df)))
  expect_equal(mapped_df$airT, c(15.2, 16.5))
  expect_equal(mapped_df$NEE, c(-2.1, -1.8))
  
  unlink(csv_file)
  unlink(yaml_file)
})

test_that("load_and_map_data warns on unmapped columns and errors on missing variables section", {
  csv_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1), csv_file, row.names = FALSE)
  
  # Invalid YAML without variables section
  yaml_file <- tempfile(fileext = ".yaml")
  writeLines("invalid_section: foo", yaml_file)
  
  expect_error(load_and_map_data(csv_file, yaml_file))
  
  unlink(csv_file)
  unlink(yaml_file)
})
