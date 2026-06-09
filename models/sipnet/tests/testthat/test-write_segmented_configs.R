test_that("write_segmented_configs", {
  pth <- withr::local_tempdir()

  event_lines <- "2025 1 irrig 0 1"
  event_src_path <- file.path(pth, "events-a.in")
  met_path <- file.path(pth, "a.clim")
  crp_chg_path <- file.path(pth, "cycles-a.csv")
  run_path <- file.path(pth, "run", "ENS-00001-a")
  dir.create(run_path, recursive = TRUE)

  event_lines |>
    writeLines(con = event_src_path)
  c("date,crop_code", "2025-01-02,D12", "2025-01-05,G6") |>
    writeLines(crp_chg_path)
  c("run_id,site_id", "ENS-00001-a,a") |>
    writeLines(file.path(pth, "runs_manifest.csv"))
  data.frame(
    year=2025,
    day = rep(1:31, each = 4),
    hour = rep(c(0, 6, 12, 18), 31),
    # rest of columns not used by test, just need to be 12 of them
    c4 = NA, c5 = NA, c6 = NA, c7 = NA, c8 = NA, c9 = NA, c10 = NA, c11 = NA,
    c12 = NA 
  ) |>
    write.table(file = met_path, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  ens.samples <- list(
    pft1 = data.frame(Amax = 1, SLA = 2),
    pft2 = data.frame(Amax = 2, SLA = 4),
    pft3 = data.frame(Amax = 3, SLA = 6),
    soil = data.frame(Rd = 0)
  )
  save(ens.samples, file = file.path(pth, "ensemble.samples.testid.Rdata"))

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth),
      rundir = file.path(pth, "run"),
      modeloutdir = file.path(pth, "out"),
      pfts = list(pft0 = list(), pft1 = list(), pft2 = list(), pft3 = list(),
                  soil = list()),
      ensemble = list(ensemble.id = "testid"),
      model = list(binary = "", revision = "2.1.0"),
      run = list(
        site = list(id = "a", name = "site1", lat = 40, lon = -88,
                    site.pft = list(veg="pft1", soil="soil")),
        inputs = list(
          met = list(path = met_path),
          events = list(path = event_src_path),
          crop_changes = list(path = crp_chg_path)
        ),
        start.date = "2025-01-01",
        end.date = "2025-01-10"
      ),
      host = list(
        name = "localhost",
        outdir = file.path(pth),
        rundir = file.path(pth, "run")
      )
    )
  )

  res <- write.config.SIPNET(
    defaults = s$pfts,
    trait.values = ens.samples["pft1"],
    IC = list(soil = 3.14),
    settings = s,
    run.id = "ENS-00001-a"
  )

  seg_res <- write_segmented_configs.SIPNET(
    settings = s,
    crop2pft = \(code) ifelse(startsWith(code, "D"), "pft2", "pft3")
  )

  expect_equal(seg_res,
               file.path(run_path, "job_segmented.sh"))

  # (at least one) parameter updated
  for (seg in 1:3) {
    param_result <- readLines(
      file.path(run_path, "segments", paste0("segment_00", seg),
                "run", "1", "sipnet.param")
    )
    seg_samp <- ens.samples[[paste0("pft", seg)]][c("Amax", "SLA")]
    expected_amax <- seg_samp$Amax * seg_samp$SLA
    expect_match(
      param_result,
      paste0("aMax ", expected_amax),
      fixed = TRUE,
      all = FALSE
    )
  }

  # job.sh includes calls to segment scripts
  jobsh <- readLines(file.path(run_path, "job.sh"))
  expect_match(jobsh, "bash .*segment_001/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "bash .*segment_002/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "bash .*segment_003/run/1/job.sh", all = FALSE)
})
