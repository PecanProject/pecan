library(testthat)

test_that("load_rds properly reads RDS files", {
  
  # Create a temporary RDS file
  rds_file <- tempfile(fileext = ".rds")
  df_input <- data.frame(
    time = c("2000-01-01", "2000-01-02"),
    GPP  = c(10.5, 11.2),
    NEE  = c(-1.1, -2.2)
  )
  saveRDS(df_input, rds_file)
  
  # 1. Test loading all variables
  res_all <- load_rds(rds_file, format = NULL, site = NULL)
  expect_s3_class(res_all, "data.frame")
  expect_equal(nrow(res_all), 2)
  expect_equal(ncol(res_all), 3)
  expect_true(all(c("time", "GPP", "NEE") %in% names(res_all)))
  
  # 2. Test loading subset of variables
  res_sub <- load_rds(rds_file, format = NULL, site = NULL, vars = c("GPP"))
  expect_s3_class(res_sub, "data.frame")
  expect_equal(ncol(res_sub), 1)
  expect_true("GPP" %in% names(res_sub))
  expect_false("NEE" %in% names(res_sub))
  
  # Cleanup
  unlink(rds_file)
})
