test_that("all PFTs reach pft_params.nml, with or without an env entry", {
  local_mocked_bindings(detect.timestep = function(...) 3600) # needs ncdump

  outdir <- tempfile()
  dir.create(file.path(outdir, "run1"), recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  jobtemplate <- file.path(outdir, "template.job")
  writeLines("#!/bin/bash", jobtemplate)

  settings <- list(
    host = list(name = "localhost", rundir = outdir, outdir = outdir),
    rundir = outdir,
    outdir = outdir,
    model = list(
      binary = "jules",
      jobtemplate = jobtemplate,
      config = system.file("template_nml_4.2", package = "PEcAn.JULES")
    ),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(lat = 45, lon = -90, met.end = "2000-12-31"),
      inputs = list(met = list(path = paste0(outdir, "/")))
    )
  )
  written_heights <- function(trait.values) {
    write.config.JULES(NULL, trait.values, settings, "run1")
    params <- readLines(file.path(outdir, "run1", "pft_params.nml"))
    grep("^canht_ft_io=", params, value = TRUE)
  }
  decid <- data.frame(height = 30)

  # template has 19.01 and 16.38 for deciduous and evergreen
  expect_equal(
    written_heights(list(temperate.deciduous = decid, temperate.coniferous = data.frame(height = 40))),
    "canht_ft_io=30,40,0.79,1.26,1,"
  )
  expect_equal(
    written_heights(list(temperate.deciduous = decid, env = data.frame(row.names = 1))),
    "canht_ft_io=30,16.38,0.79,1.26,1,"
  )
})
