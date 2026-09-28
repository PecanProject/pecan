test_that("subsetting by index", {
  # Faking a multi-year input
  dat <- read.table("data/niwot_1999_v2.clim", header = FALSE)
  dat <- dplyr::bind_rows(
    dat,
    dat |> dplyr::mutate(V1 = 2000),
    dat |> dplyr::mutate(V1 = 2001)
  )

  # Whole file
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      1999, 1, 7.5,
      2001, 365, 17), # NB file ends at 16.5, recall end time is exclusive
    list(start = 1, end = 2190)
  )

  # Fully within file
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      1999, 2, 12,
      1999, 10, 16.5),
    list(start = 4, end = 19)
  )
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      1999, 2, 12,
      2000, 1, 0),
    list(start = 4, end = 730)
  )
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      2000, 1, 0,
      2000, 2, 0),
    list(start = 731, end = 732)
  )

  # Starting before file
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      1998, 1, 0,
      1999, 3, 10),
    list(start = 1, end = 5)
  )

  # Ending after file
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      2001, 364, 12,
      2002, 10, 1.5),
    list(start = 2188, end = 2190)
  )

  # Fully outside range
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      1998, 1, 0,
      1998, 350, 23),
    list(start = 0, end = 0)
  )
  expect_equal(
    sipnet_time_range_idx(
      dat$V1, dat$V2, dat$V3,
      2002, 1, 0,
      2003, 350, 23),
    list(start = 0, end = 0)
  )
})
