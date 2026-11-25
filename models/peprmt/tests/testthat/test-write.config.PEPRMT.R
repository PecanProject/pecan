
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

test_that("write.config.PEPRMT accepts event files with no supported events", {
  d <- withr::local_tempdir()
  rundir <- file.path(d, "run", "r1")
  dir.create(rundir, recursive = TRUE)
  met <- file.path(d, "met.dat")
  data.frame(
    Year = 2020, DOY_disc = 1:3, DOY = 1:3, TA_C = 10, PAR_umol_m2_day = 5
  ) |>
    utils::write.table(met, row.names = FALSE, quote = FALSE)
  peprmt_in <- file.path(d, "peprmt.csv")
  data.frame(
    Year = 2020, DOY_disc = 1:3, WTD_cm = 0, Salinity_daily_ave_ppt = 30
  ) |>
    utils::write.csv(peprmt_in, row.names = FALSE)
  evt_file <- file.path(d, "events.json")
  writeLines(
    '[{"site_id": "S1", "events": [{"event_type": "planting", "date": "2020-01-02"}]}]',
    evt_file
  )
  settings <- list(
    host = list(rundir = file.path(d, "run"), outdir = file.path(d, "out")),
    model = list(binary = "peprmt"),
    run = list(
      site = list(id = "S1", lat = 1, lon = 2),
      start.date = "2020-01-01",
      end.date = "2020-01-03",
      inputs = list(
        met = list(path = met),
        PEPRMT = list(path = peprmt_in),
        event_json = list(path = evt_file)
      )
    )
  )
  suppressMessages(
    write.config.PEPRMT(list(), list(pft1 = list(a0 = 1)), settings, "r1")
  )
  expect_true(file.exists(file.path(rundir, "run_data.csv")))
})
