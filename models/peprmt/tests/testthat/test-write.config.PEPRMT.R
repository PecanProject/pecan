
test_that("impose_event", {
  dat <- data.frame(
    Year = 2020,
    DOY_disc = 1:4,
    WTD_cm = 0,
    Salinity_daily_ave_ppt = 30
  )

  # elevation change lasts through end of run
  elev_evt <- list(
    date = "2020-01-02",
    event_type = "elevation",
    cm_elevation_rise = 10
  )
  expect_equal(
    impose_event_on_data(dat, elev_evt)$WTD_cm,
    c(0, 10, 10, 10)
  )

  # salinity change lasts specified num days or to end of run
  sal_evt <- list(
    date = "2020-01-02",
    event_type = "salinity",
    pct_relative_salinity_change = 10,
    days_duration = 1
  )
  expect_equal(
    impose_event_on_data(dat, sal_evt)$Salinity_daily_ave_ppt,
    c(30, 33, 30, 30)
  )
  sal_evt$days_duration <- 5
  expect_equal(
    impose_event_on_data(dat, sal_evt)$Salinity_daily_ave_ppt,
    c(30, 33, 33, 33)
  )

  # Unknown event type returns input
  expect_equal(
    impose_event_on_data(dat, list(event_type = "unknown")),
    dat
  )
  
  # out-of-range dates return input
  # (TODO reconsider if this should be an error instead)
  sal_evt$date = "2021-01-05"
  expect_equal(
    impose_event_on_data(dat, sal_evt),
    dat
  )
  sal_evt$date = "2019-12-31"
  sal_evt$days_duration = 1
  expect_equal(
    impose_event_on_data(dat, sal_evt),
    dat
  )
  sal_evt$days_duration = 2
  expect_equal(
    impose_event_on_data(dat, sal_evt)$Salinity_daily_ave_ppt,
    c(33, 30, 30, 30)
  )
})
