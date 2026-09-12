build_file_tree <- function(name) {
  dir.create(dirname(name), recursive = TRUE)
  writeLines(text = name, con = name)
}

test_that("collect_restarts", {
  rundir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  run_names <- file.path(
    rundir,
    c("ENS-001-1234/segments/segment_001/run/restart.out",
      "ENS-002-1234/segments/segment_001/run/restart.out",
      "ENS-002-1234/segments/segment_003/run/restart.out",
      "ENS-001-5678/segments/segment_001/run/restart.out",
      "ENS-001-5678/segments/segment_010/run/restart.out",
      "ENS-001-91011/restart.out")
  )
  lapply(run_names, build_file_tree)

  res <- collect_restarts(rundir, outdir)
  expect_length(res, 4)

  expect_setequal(
    list.files(outdir),
    c("restart-1234-001.out",
      "restart-1234-002.out",
      "restart-5678-001.out",
      "restart-91011-001.out")
  )

  expect_match(
    readLines(file.path(outdir, "restart-1234-001.out")),
    "segment_001"
  )
  expect_match(
    readLines(file.path(outdir, "restart-1234-002.out")),
    "segment_003"
  )
  expect_match(
    readLines(file.path(outdir, "restart-5678-001.out")),
    # NB this is reliant on all segments getting same zero padding.
    # If we had segment_1 and segment_010, would pick seg 1 instead.
    # Consider having fn operate on numeric segment values instead.
    "segment_010"
  )


})
