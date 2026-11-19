context("write.insfile")

rundir <- tempfile()
setup(dir.create(file.path(rundir, "run1"), recursive = TRUE, showWarnings = FALSE))
teardown(unlink(rundir, recursive = TRUE))

test_that("traits are matched to PFTs by name, not position", {
  gridcf <- file.path(rundir, "gridcf.txt")
  gridlist <- file.path(rundir, "gridlist.txt")
  writeLines("", gridcf)
  writeLines("", gridlist)
  settings <- list(
    pfts = list(
      pft = list(name = "pft_a"),
      pft = list(name = "pft_b"),
      pft = list(name = "pft_c")
    ),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      inputs = list(
        gridcf = list(path = gridcf),
        gridlist = list(path = gridlist),
        met = list(path = "met.tmp.nc"),
        soil = list(path = "soil.nc"),
        landuse = list(path = "lu.txt"),
        landusechange = list(path = "luc.txt")
      )
    ),
    rundir = rundir
  )
  # pft_b has no sampled traits, so it has no entry in trait.values
  trait.values <- list(pft_a = list(SLA = 11), pft_c = list(SLA = 33))

  write.insfile.LPJGUESS(settings, trait.values, rundir, rundir, "run1")

  params <- readLines(file.path(rundir, "run1", "params.ins"))
  sla <- trimws(sub("^\\s*sla", "", grep("^\\s*sla ", params, value = TRUE)))
  defaults <- PEcAn.utils::load_local(
    system.file("lpjguess_params.Rdata", package = "PEcAn.LPJGUESS")
  )$lpjguess_param_list
  default_sla <- as.character(defaults[["sla"]])
  expect_equal(sla, c("11", default_sla, "33"))
})
