test_that("build_SM_CDS_request uses current CDS values and formats dates", {
  cases <- data.frame(
    date = as.Date(c("2026-11-02", "2024-02-29", "2025-01-01")),
    year = c("2026", "2024", "2025"),
    month = c("11", "02", "01"),
    day = c("02", "29", "01")
  )

  for (i in seq_len(nrow(cases))) {
    request <- build_SM_CDS_request(cases$date[i])

    expect_equal(request$variable, "surface_soil_moisture_saturation")
    expect_equal(request$time_aggregation, "daily")
    expect_equal(request$year, cases$year[i])
    expect_equal(request$month, cases$month[i])
    expect_equal(request$day, cases$day[i])
  }
})
