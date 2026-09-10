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

  # Each segment writes its own restart and later segments read the previous one.
  segment_rundirs <- file.path(
    run_path, "segments", sprintf("segment_%03d", 1:3), "run"
  )
  for (seg in 1:3) {
    config <- readLines(file.path(segment_rundirs[seg], "1", "sipnet.in"))
    restart_in <- grep("^[[:space:]]*RESTART_IN[[:space:]]*=", config, value = TRUE)
    restart_out <- grep("^[[:space:]]*RESTART_OUT[[:space:]]*=", config, value = TRUE)

    expect_identical(
      restart_out,
      paste("RESTART_OUT =", file.path(segment_rundirs[seg], "restart.out"))
    )
    if (seg == 1) {
      expect_length(restart_in, 0)
    } else {
      expect_identical(
        restart_in,
        paste("RESTART_IN =", file.path(segment_rundirs[seg - 1], "restart.out"))
      )
    }
  }

  # job.sh includes calls to segment scripts
  jobsh <- readLines(file.path(run_path, "job.sh"))
  expect_match(jobsh, "bash .*segment_001/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "bash .*segment_002/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "bash .*segment_003/run/1/job.sh", all = FALSE)
})

test_that("segment_dataframe falls back to event_json for the configured site", {
  pth <- withr::local_tempdir()
  events_path <- file.path(pth, "events.json")
  jsonlite::write_json(
    list(
      list(
        site_id = "a",
        events = list(
          list(event_type = "planting", date = "2025-01-02", crop_code = "D12"),
          list(event_type = "planting", date = "2025-01-05", crop_code = "G6")
        )
      ),
      list(
        site_id = "b",
        events = list(
          list(event_type = "planting", date = "2025-01-03", crop_code = "P1")
        )
      )
    ),
    path = events_path,
    auto_unbox = TRUE
  )
  run_settings <- PEcAn.settings::as.Settings(list(
    run = list(
      site = list(id = "a", site.pft = list(veg = "pft1")),
      inputs = list(event_json = list(path = events_path)),
      start.date = "2025-01-01",
      end.date = "2025-01-10"
    )
  ))

  result <- PEcAn.SIPNET:::segment_dataframe(run_settings)

  expect_equal(nrow(result), 3)
  expect_identical(result$site_id, rep("a", 3))
  expect_identical(result$segment_id, c("001", "002", "003"))
  expect_identical(
    result$start_date,
    as.Date(c("2025-01-01", "2025-01-02", "2025-01-05"))
  )
  expect_identical(
    result$end_date,
    as.Date(c("2025-01-01", "2025-01-04", "2025-01-10"))
  )
  expect_identical(result$crop_code, c(NA_character_, "D12", "G6"))
  expect_identical(result$pft[1], "pft1")
})

test_that("segment_dataframe returns empty when run start is after all crop cycles", {
  pth <- withr::local_tempdir()
  crp_chg_path <- file.path(pth, "cycles-a.csv")
  c("date,crop_code", "2025-01-02,D12", "2025-01-05,G6") |>
    writeLines(crp_chg_path)

  run_settings <- PEcAn.settings::as.Settings(list(
    run = list(
      site = list(id = "a", site.pft = list(veg = "pft1")),
      inputs = list(crop_changes = list(path = crp_chg_path)),
      start.date = "2025-01-10",
      end.date = "2025-01-20"
    )
  ))

  result <- PEcAn.SIPNET:::segment_dataframe(run_settings)

  expect_equal(nrow(result), 0)
  expect_named(
    result, 
    c("start_date", "crop_code", "end_date")
  )
})

test_that("write_segment_configs returns unaltered job.sh when no segments remain", {
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
    year = 2025,
    day = rep(1:31, each = 4),
    hour = rep(c(0, 6, 12, 18), 31),
    c4 = NA, c5 = NA, c6 = NA, c7 = NA, c8 = NA, c9 = NA, c10 = NA, c11 = NA,
    c12 = NA
  ) |>
    write.table(
      file = met_path, quote = FALSE,
      row.names = FALSE, col.names = FALSE
    )
  ens.samples <- list(
    pft1 = data.frame(Amax = 1, SLA = 2),
    soil = data.frame(Rd = 0)
  )
  save(ens.samples, file = file.path(pth, "ensemble.samples.testid.Rdata"))

  s <- PEcAn.settings::as.Settings(
    list(
      outdir = file.path(pth),
      rundir = file.path(pth, "run"),
      modeloutdir = file.path(pth, "out"),
      pfts = list(pft1 = list(), soil = list()),
      ensemble = list(ensemble.id = "testid"),
      model = list(binary = "", revision = "2.1.0"),
      run = list(
        site = list(id = "a", name = "site1", lat = 40, lon = -88,
                    site.pft = list(veg = "pft1", soil = "soil")),
        inputs = list(
          met = list(path = met_path),
          events = list(path = event_src_path),
          crop_changes = list(path = crp_chg_path)
        ),
        start.date = "2025-01-10",
        end.date = "2025-01-20"
      ),
      host = list(
        name = "localhost",
        outdir = file.path(pth),
        rundir = file.path(pth, "run")
      )
    )
  )

  write.config.SIPNET(
    defaults = s$pfts,
    trait.values = ens.samples["pft1"],
    IC = list(soil = 3.14),
    settings = s,
    run.id = "ENS-00001-a"
  )

  seg_res <- write_segmented_configs.SIPNET(settings = s)

  expect_equal(seg_res, file.path(run_path, "job.sh"))
  expect_false(file.exists(file.path(run_path, "job_segmented.sh")))
  expect_false(dir.exists(file.path(run_path, "segments")))
})
