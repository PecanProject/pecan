test_that("write.config.SIPNET", {
  pth <- withr::local_tempdir()

  event_lines <- "2025 1 irrig 0 1"
  event_src_path <- file.path(pth, "events-a.in")
  dir.create(file.path(pth, "run", "run1"), recursive = TRUE)
  writeLines(event_lines, con = event_src_path)

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth, "out"),
      rundir = file.path(pth, "run"),
      pfts = list(pft1 = list()),
      model = list(binary = "", revision = ""),
      run = list(
        site = list(name = "site1", lat = 40, lon = -88),
        inputs = list(
          met = list(path = ""),
          events = list(path = event_src_path)
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-02"
      ),
      host = list(
        name = "",
        outdir = file.path(pth, "out"),
        rundir = file.path(pth, "run")
      )
    )
  )

  res <- write.config.SIPNET(
    defaults = list(pft1 = list(constants = list(SLA = 2.0))),
    trait.values = list(pft1 = list(Amax = 5, AmaxFrac = 0.99, leafC = 47)),
    settings = s,
    run.id = "run1"
  )

  # events file correctly copied
  expect_match(
    readLines(file.path(pth, "run", "run1", "events.in")),
    event_lines,
    fixed = TRUE,
    all = TRUE
  )

  # (at least some) parameters updated
  param_result <- readLines(file.path(pth, "run", "run1", "sipnet.param"))
  expect_match(
    param_result,
    "aMax 10", # this is Amax * SLA,
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    param_result,
    "aMaxFrac 0.99", # raw template had 0.76
    fixed = TRUE,
    all = FALSE
  )
  # leaf C specific weight is leafC / SLA, with units converted to g C/m2 leaf
  expect_match(
    param_result,
    "leafCSpWt 235 ", # space at end to catch unit errors (eg fail on 23500)
    fixed = TRUE,
    all = FALSE
  )

})


test_that("update_flag_lines", {
  txt <- c("!comment", "NITROG = 0", "GDD = 1")

  # existing lines updated, new lines added
  expect_equal(
    update_flag_lines(txt, c(GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
  expect_equal(
    update_flag_lines(txt, c(new_flag = 0, NITROG = "1")),
    c("!comment", "NITROG = 1", "GDD = 1", "new_flag = 0")
  )

  # empty flags return input
  expect_equal(txt, update_flag_lines(txt, c()))
  expect_equal(txt, update_flag_lines(txt, NULL))

  # unnamed arguments ignored
  expect_equal(txt, update_flag_lines(txt, c("unnamed")))
  expect_equal(
    update_flag_lines(txt, c(1, GDD = 0)),
    c("!comment", "NITROG = 0", "GDD = 0")
  )
})
