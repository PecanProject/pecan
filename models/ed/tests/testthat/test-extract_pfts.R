test_that("pft parsing util works", {
  pfts <-
    list(
      pft = list(
        name = "SetariaWT",
        ed2_pft_number = "1",
        outdir = "/data/output/pecan_runs/transect_runs/ed2_testout/pft/SetariaWT"
      ),
      pft = list(
        name = "ebifarm.c3grass",
        ed2_pft_number = "5",
        outdir = "/data/output/pecan_runs/transect_runs/ed2_testout/pft/ebifarm.c3grass"
      )
    )
  expect_equal(extract_pfts(pfts), c("SetariaWT" = 1, "ebifarm.c3grass" = 5))
  expect_type(extract_pfts(pfts), "integer")
})
