test_that("efi_long_to_array reshapes single-site single-variable EFI long data frame", {
  df <- data.frame(
    datetime = rep(c("2020-01-01 00:00:00", "2020-01-02 00:00:00"), each = 2),
    parameter = rep(c(1, 2), times = 2),
    variable = "TotSoilCarb",
    site_id = "site1",
    prediction = c(10, 20, 11, 21)
  )

  mat <- efi_long_to_array(df)

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(2, 2))
  expect_equal(as.vector(mat[1, ]), c(10, 20))
  expect_equal(as.vector(mat[2, ]), c(11, 21))
  expect_true(inherits(attr(mat, "time"), "POSIXct"))
})

test_that("efi_long_to_array filters by var and site parameters when provided", {
  df <- data.frame(
    datetime = rep(c("2020-01-01 00:00:00", "2020-01-02 00:00:00"), each = 4),
    parameter = rep(c(1, 2), times = 4),
    variable = rep(c("TotSoilCarb", "TotSoilCarb", "AGB", "AGB"), times = 2),
    site_id = rep(c("site1", "site2"), each = 2, times = 2),
    prediction = c(10, 20, 30, 40, 11, 21, 31, 41)
  )

  mat <- efi_long_to_array(df, var = "TotSoilCarb", site = "site1")

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(2, 2))
  expect_equal(as.vector(mat[1, ]), c(10, 20))
})

test_that("efi_long_to_array rejects empty or NULL inputs", {
  expect_error(efi_long_to_array(NULL), "NULL or empty")
  expect_error(efi_long_to_array(data.frame()), "NULL or empty")
})

test_that("efi_long_to_array rejects inputs with multiple variables or sites without filtering", {
  df_multivar <- data.frame(
    datetime = "2020-01-01 00:00:00",
    parameter = 1,
    variable = c("TotSoilCarb", "AGB"),
    site_id = "site1",
    prediction = c(10, 20)
  )
  expect_error(efi_long_to_array(df_multivar), "multiple variables")

  df_multisite <- data.frame(
    datetime = "2020-01-01 00:00:00",
    parameter = 1,
    variable = "TotSoilCarb",
    site_id = c("site1", "site2"),
    prediction = c(10, 20)
  )
  expect_error(efi_long_to_array(df_multisite), "multiple sites")
})
