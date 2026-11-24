test_that("segmented SIPNET parameters follow saved run IDs, not Sobol indices twice", {
  case <- make_segmented_sobol_case(withr::local_tempdir())
  run_dir <- file.path(case$settings$rundir, case$run_row$run_id)
  full <- read_sipnet_test_parameters(file.path(run_dir, "sipnet.param"))
  expect_equal(full[c("aMax", "leafCSpWt")], c(aMax = 40, leafCSpWt = 12))

  saved_file <- PEcAn.uncertainty::ensemble.filename(
    case$settings, "ensemble.samples", "Rdata", all.var.yr = TRUE)
  saved <- new.env()
  load(saved_file, envir = saved)
  expect_identical(attr(saved$ens.samples, "run_ids"), saved$ens.run.ids$id)

  # Reorder storage independently of the manifest to require selection by ID.
  order <- 8:1
  ids <- attr(saved$ens.samples, "run_ids")[order]
  saved$ens.samples <- lapply(saved$ens.samples, function(dat) dat[order, , drop = FALSE])
  attr(saved$ens.samples, "run_ids") <- ids
  save(list = ls(saved), file = saved_file, envir = saved)
  write_segment_configs(case$settings, case$run_row,
    crop2pft = function(code) "pft1", replace_and_link = FALSE)
  segmented <- read_sipnet_test_parameters(file.path(
    run_dir, "segments/segment_001/run/1/sipnet.param"))
  expect_equal(segmented, full)

  bad_row <- case$run_row
  bad_row$run_id <- "missing-run"
  expect_error(write_segment_configs(case$settings, bad_row),
               "missing from ensemble samples", fixed = TRUE)
})

test_that("segmented SIPNET still indexes unmarked legacy source samples by param", {
  case <- make_segmented_sobol_case(withr::local_tempdir())
  saved_file <- PEcAn.uncertainty::ensemble.filename(
    case$settings, "ensemble.samples", "Rdata", all.var.yr = TRUE)
  saved <- new.env()
  load(saved_file, envir = saved)
  saved$ens.samples <- case$samples$ensemble.samples
  save(list = ls(saved), file = saved_file, envir = saved)

  write_segment_configs(case$settings, case$run_row,
    crop2pft = function(code) "pft1", replace_and_link = FALSE)
  run_dir <- file.path(case$settings$rundir, case$run_row$run_id)
  full <- read_sipnet_test_parameters(file.path(run_dir, "sipnet.param"))
  segmented <- read_sipnet_test_parameters(file.path(
    run_dir, "segments/segment_001/run/1/sipnet.param"))
  expect_equal(segmented[c("aMax", "leafCSpWt")], c(aMax = 40, leafCSpWt = 12))
  expect_equal(segmented, full)

  bad_row <- case$run_row
  bad_row$param <- 5L
  expect_error(write_segment_configs(case$settings, bad_row),
               "Parameter rows do not match run", fixed = TRUE)
})
