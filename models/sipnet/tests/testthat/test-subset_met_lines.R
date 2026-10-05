test_that("read_date", {
  expect_equal(read_date("2020\t1\t0\t0.125\t..."), as.Date("2020-01-01"))
  expect_equal(
    read_date("0\t2020\t1\t0\t0.125\t...", offset = 1),
    as.Date("2020-01-01")
  )
  expect_equal(read_date("2020\t1\t23\t0.125\t..."), as.Date("2020-01-01"))
  expect_equal(
    read_date("  2026\t365\t 1\t0.125\t   ..."),
    as.Date("2026-12-31")
  )
  
  expect_error(read_date(""))
  expect_error(read_date("asdlk\tgegfw"))
  expect_error(read_date("0\t2020\t1\t0\t0.125\t...", offset = 0))
  expect_error(read_date("0\t2020\t400\t0\t0.125\t...", offset = 1))
})


test_that("check_start_end", {
  txt = c(
    "2016    1  0.00 0.5 16.2730 13.58  7.810e+00  -6.939e-15   1646.01  1344.39  221.88 3.72499",
    "2016    1 12.00 0.5  5.5212 13.64 -1.926e-15  -6.939e-15    506.04  1169.95  399.81 2.77617",
    "2016    2  0.00 0.5 15.7889 13.78  7.536e+00  -6.939e-15   1493.43  1271.11  316.68 2.54669",
    "2016    2 12.00 0.5  4.7490 13.90 -1.926e-15  -6.939e-15    382.18  1120.24  475.94 1.78448",
    "2016    3  0.00 0.5 16.9078 14.04  6.701e+00  -6.939e-15   1467.40  1136.88  478.77 1.74723",
    "2016    3 12.00 0.5  7.0186 14.13 -1.926e-15  -6.939e-15    386.20  1001.34  619.05 2.01894"
  )
  expect_equal(check_start_end(txt, "2016-01-01", "2016-01-03", n = 6), txt)

  expect_error(check_start_end(txt, "2016-01-01", "2016-01-03", n = 5), "== n")
  expect_error(check_start_end(txt, "2016-01-01", "2016-01-04", n = 6), "== d2")
  expect_error(check_start_end(txt, "2016-01-02", "2016-01-03", n = 6), "== d1")
  expect_error(check_start_end(txt[-4], "2016-01-01", "2016-01-03", n = 6), "== n")

  # Does not detect order/format issues in the rows between start and end!
  quietly_bad <- c(txt, "garbage garbage garbage", txt)
  expect_equal(
    check_start_end(quietly_bad, "2016-01-01", "2016-01-03", n = 13),
    quietly_bad
  )

  v1_txt <- paste("0", txt, "0.0")
  expect_equal(
    check_start_end(v1_txt, "2016-01-01", "2016-01-03", n = 6, offset = 1),
    v1_txt
  )
})

test_that("subset_met_lines", {
  read_clim <- function(path) {
    read.table(
      path,
      colClasses = "double",
      col.names = c("year", "doy", "hour", "step", rep("", 8))
    )
  }

  infile <- "data/hourly_v2.clim"

  # whole file
  out_1_7 <- withr::local_tempfile()
  subset_met_lines(infile, out_1_7, "2024-01-01", "2024-01-08")
  res_1_7 <- read_clim(out_1_7)
  expect_shape(res_1_7, nrow = 168)
  expect_equal(res_1_7$doy, rep(1:7, each = 24))

  # middle of file
  out_3_5 <- withr::local_tempfile()
  subset_met_lines(infile, out_3_5, "2024-01-03", "2024-01-06")
  res_3_5 <- read_clim(out_3_5)
  expect_shape(res_3_5, nrow = 72)
  expect_equal(res_3_5$doy, rep(3:5, each = 24))


  # first day
  out_1_1 <- withr::local_tempfile()
  subset_met_lines(infile, out_1_1, "2024-01-01", "2024-01-02")
  res_1_1 <- read_clim(out_1_1)
  expect_shape(res_1_1, nrow = 24)
  expect_equal(res_1_1$doy, rep(1, each = 24))

  # last day
  out_7_7 <- withr::local_tempfile()
  subset_met_lines(infile, out_7_7, "2024-01-07", "2024-01-08")
  res_7_7 <- read_clim(out_7_7)
  expect_shape(res_7_7, nrow = 24)
  expect_equal(res_7_7$doy, rep(7, each = 24))


  # end day not in file
  out_6_8 <- withr::local_tempfile()
  expect_error(
    subset_met_lines(infile, out_6_8, "2024-01-06", "2024-01-09"),
    "== d2"
  )

  ## variable-width files
  in_vw <- withr::local_tempfile()
  vw_raw <- readLines(infile)
  vw_raw[1:5] <- paste0(vw_raw[1:5], "   ")
  writeLines(vw_raw, in_vw)
  out_vw_10 <- withr::local_tempfile()
  # with n_head long enough to notice: reads as var width
  subset_met_lines(in_vw, out_vw_10, "2024-01-01", "2024-01-03")
  res_vw_10 <- read_clim(out_vw_10)
  expect_shape(res_vw_10, nrow = 48)
  expect_equal(res_vw_10$doy, rep(1:2, each = 24))
  # With n_head too short: Tries to read as fixed width,
  # errors when read finds partial lines
  out_vw_5 <- withr::local_tempfile()
  expect_error(
    subset_met_lines(in_vw, out_vw_5, "2024-01-01", "2024-01-03", n_head = 5),
    "== chars_per_line"
  )
  
  ## variable timesteps
  # Detected: Immediate error
  out_vt_10 <- withr::local_tempfile()
  expect_error(
    subset_met_lines("data/niwot_1999_v2.clim", out_vt_10, "1999-07-01", "1999-08-01", n_head = 10),
    "length(steps_per_day) == 1",
    fixed = TRUE
  )
  # variability not detected, detected steps per day not same as true steps per day:
  # error when dates don't line up
  in_vt_1 <- withr::local_tempfile()
  writeLines(
    c(
      "2016  1   7.5 0.375 16.2730 13.58  7.810e+00  -6.939e-15   1646.01  1344.39  221.88 3.72499",
      "2016  1  16.5 0.625  5.5212 13.64 -1.926e-15  -6.939e-15    506.04  1169.95  399.81 2.77617",
      "2016  2   7.5 0.375 15.7889 13.78  7.536e+00  -6.939e-15   1493.43  1271.11  316.68 2.54669",
      "2016  2  16.5 0.625  4.7490 13.90 -1.926e-15  -6.939e-15    382.18  1120.24  475.94 1.78448",
      "2016  3   7.5 0.375 16.9078 14.04  6.701e+00  -6.939e-15   1467.40  1136.88  478.77 1.74723",
      "2016  3  16.5 0.625  7.0186 14.13 -1.926e-15  -6.939e-15    386.20  1001.34  619.05 2.01894",
      "2016  4   7.5 0.375 16.9078 14.04  6.701e+00  -6.939e-15   1467.40  1136.88  478.77 1.74723",
      "2016  4  16.5 0.625  7.0186 14.13 -1.926e-15  -6.939e-15    386.20  1001.34  619.05 2.01894"
    ),
    in_vt_1
  )
  out_vt_1 <- withr::local_tempfile()
  expect_error(
    subset_met_lines(in_vt_1, out_vt_1, "2016-01-03", "2016-01-05", n_head = 1),
    "== d1"
  )

  # variability not detected, but times add up to detected num lines per day
  # after rounding: success (but lot of ways for this to go wrong!)
  in_vt_2 <- withr::local_tempfile()
  writeLines(
    c(
      # Only difference from in_vt_1 above: step order 0.375/0.625 vs 0.625/0.375
      "2016  1   7.5 0.625 16.2730 13.58  7.810e+00  -6.939e-15   1646.01  1344.39  221.88 3.72499",
      "2016  1  16.5 0.375  5.5212 13.64 -1.926e-15  -6.939e-15    506.04  1169.95  399.81 2.77617",
      "2016  2   7.5 0.625 15.7889 13.78  7.536e+00  -6.939e-15   1493.43  1271.11  316.68 2.54669",
      "2016  2  16.5 0.375  4.7490 13.90 -1.926e-15  -6.939e-15    382.18  1120.24  475.94 1.78448",
      "2016  3   7.5 0.625 16.9078 14.04  6.701e+00  -6.939e-15   1467.40  1136.88  478.77 1.74723",
      "2016  3  16.5 0.375  7.0186 14.13 -1.926e-15  -6.939e-15    386.20  1001.34  619.05 2.01894",
      "2016  4   7.5 0.625 16.9078 14.04  6.701e+00  -6.939e-15   1467.40  1136.88  478.77 1.74723",
      "2016  4  16.5 0.375  7.0186 14.13 -1.926e-15  -6.939e-15    386.20  1001.34  619.05 2.01894"
    ),
    in_vt_2
  )
  out_vt_2 <- withr::local_tempfile()
  subset_met_lines(in_vt_2, out_vt_2, "2016-01-03", "2016-01-05", n_head = 1)
  res_vt_2 <- read_clim(out_vt_2)
  expect_shape(res_vt_2, nrow = 4)
  expect_equal(res_vt_2$doy, rep(3:4, each = 2))
})
