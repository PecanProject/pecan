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
  c("date,crop_code", "2025-01-02,D12", "2025-01-04,D13", "2025-01-05,G6") |>
    writeLines(crp_chg_path)
  c("run_id,site_id", "ENS-00001-a,a") |>
    writeLines(file.path(pth, "runs_manifest.csv"))
  data.frame(
    year = 2025,
    day = rep(1:31, each = 4),
    hour = rep(c(0, 6, 12, 18), 31),
    step = 0.25,
    # rest of columns not used by test, just need to be 12 of them
    c5 = NA, c6 = NA, c7 = NA, c8 = NA, c9 = NA, c10 = NA, c11 = NA,
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
  expect_match(jobsh, "segment_001/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "segment_002/run/1/job.sh", all = FALSE)
  expect_match(jobsh, "segment_003/run/1/job.sh", all = FALSE)
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

test_that("segment_dataframe retains the crop planted before the run starts", {
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

  expect_equal(nrow(result), 1)
  expect_equal(result$crop_code, "G6")
  expect_equal(result$start_date, as.Date("2025-01-10"))
  expect_equal(result$end_date, as.Date("2025-01-20"))
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
  c("date,crop_code", "2025-02-02,D12", "2025-02-05,G6") |>
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

test_that("drop_same_pft_segments merges consecutive segments with the same PFT", {
  segments <- data.frame(
    start_date = as.Date(c(
      "2025-01-01", "2025-01-02", "2025-01-04", "2025-01-05"
    )),
    end_date = as.Date(c(
      "2025-01-01", "2025-01-03", "2025-01-04", "2025-01-10"
    )),
    crop_code = c(NA_character_, "D12", "D13", "G6"),
    pft = c("pft1", "pft2", "pft2", "pft3")
  )

  result <- PEcAn.SIPNET:::drop_same_pft_segments(segments)

  expect_equal(nrow(result), 3)

  expect_equal(
    result$start_date,
    as.Date(c("2025-01-01", "2025-01-02", "2025-01-05"))
  )

  expect_equal(
    result$end_date,
    as.Date(c("2025-01-01", "2025-01-04", "2025-01-10"))
  )

  expect_identical(
    result$crop_code,
    c("NA", "D12 D13", "G6")
  )
  expect_identical(
    result$pft,
    c("pft1", "pft2", "pft3")
  )

  expect_identical(
    result$segment_id,
    c("001", "002", "003")
  )

  # Merging leaves no gaps and does not change the overall date range
  expect_equal(min(result$start_date), min(segments$start_date))
  expect_equal(max(result$end_date), max(segments$end_date))
  expect_equal(result$start_date[-1], result$end_date[-nrow(result)] + 1)

  # First and last segments can be part of a merge: all one PFT -> one row
  segments$pft <- "pft1"
  all_one <- PEcAn.SIPNET:::drop_same_pft_segments(segments)
  expect_equal(nrow(all_one), 1)
  expect_equal(all_one$start_date, min(segments$start_date))
  expect_equal(all_one$end_date, max(segments$end_date))
})

test_that("segment_dataframe clips overlapping crops and sorts planting dates", {
  pth <- withr::local_tempdir()
  path <- file.path(pth, "crops.csv")
  writeLines(c("date,crop_code", "2025-01-10,B", "2025-01-01,A", "2025-02-01,C"), path)
  settings <- list(run = list(
    site = list(id = "a"), inputs = list(crop_changes = list(path = path)),
    start.date = "2025-01-05", end.date = "2025-01-10"
  ))
  result <- segment_dataframe(settings)
  expect_equal(result$crop_code, c("A", "B"))
  expect_equal(result$start_date, as.Date(c("2025-01-05", "2025-01-10")))
  expect_equal(result$end_date, as.Date(c("2025-01-09", "2025-01-10")))
})

test_that("segmented SA applies crop and soil perturbations to the correct segments", {
  pth <- withr::local_tempdir()
  crops <- file.path(pth, "crops.csv")
  events <- file.path(pth, "events.in")
  met <- file.path(pth, "met.clim")
  writeLines(c("date,crop_code,pft", "2025-01-03,B,pftB", "2025-01-06,A,pftA"), crops)
  writeLines("2025 1 irrig 0 1", events)
  climate <- matrix(0, nrow = 40, ncol = 12)
  climate[, 1] <- 2025
  climate[, 2] <- rep(1:10, each = 4)
  climate[, 3] <- rep(c(0, 6, 12, 18), 10)
  climate[, 4] <- 0.25
  utils::write.table(climate, met, quote = FALSE, row.names = FALSE, col.names = FALSE)
  sa.samples <- list(
    pftA = matrix(c(1, 2, 2, 2), 2, dimnames = list(c("50", "95"), c("Amax", "SLA"))),
    pftB = matrix(c(3, 6, 2, 2), 2, dimnames = list(c("50", "95"), c("Amax", "SLA"))),
    soil = matrix(c(1, 4), 2, dimnames = list(c("50", "95"), "soil_respiration_Q10"))
  )
  settings <- PEcAn.settings::as.Settings(list(
    outdir = pth, rundir = file.path(pth, "run"), modeloutdir = file.path(pth, "out"),
    pfts = list(
      pftA = list(constants = list(AmaxFrac = 0.2)),
      pftB = list(constants = list(AmaxFrac = 0.8)),
      soil = list(constants = list(frozenSoilEff = 0.35))
    ),
    sensitivity.analysis = list(ensemble.id = "testid"),
    model = list(binary = "", revision = "2.1.0"),
    run = list(
      site = list(id = "a", name = "site1", lat = 40, lon = -88,
                  site.pft = list(veg = "pftA", soil = "soil")),
      inputs = list(met = list(path = list("unused", met)), events = list(path = events),
                    crop_changes = list(path = crops)),
      start.date = "2025-01-01", end.date = "2025-01-10"
    ),
    host = list(name = "localhost", outdir = pth, rundir = file.path(pth, "run"))
  ))
  samples_file <- PEcAn.uncertainty::sensitivity.filename(
    settings, "sensitivity.samples", "Rdata", all.var.yr = TRUE
  )
  save(sa.samples, file = samples_file)
  manifest <- tibble::tibble(
    run_id = c("101", "101", "102", "103", "104"), site_id = "a", ens_num = c(1, 1, 2, 3, 4),
    pft_name = c("pftA", "pftB", "pftB", "soil", "pftA"),
    trait = c("Amax", "Amax", "Amax", "soil_respiration_Q10", "Amax"),
    quantile = c("50", "50", "95", "95", "95"), type = "Sensitivity"
  )
  utils::write.csv(manifest, file.path(pth, "runs_manifest.csv"), row.names = FALSE)
  for (run_id in unique(manifest$run_id)) {
    run_path <- file.path(settings$rundir, run_id)
    dir.create(run_path, recursive = TRUE)
    writeLines("original", file.path(run_path, "job.sh"))
  }
  design <- tibble::tibble(
    param = 1:4, met = 2L, sa_pft = c(NA, "pftB", "soil", "pftA"),
    sa_trait = c(NA, "Amax", "soil_respiration_Q10", "Amax"),
    sa_quantile = c("50", "95", "95", "95")
  )
  jobs <- write_segmented_configs.SIPNET(settings, design)
  expect_length(jobs, 4)
  expect_identical(settings$run$site$site.pft$veg, "pftA")
  for (i in 1:4) {
    for (segment in 1:3) {
      params <- utils::read.table(file.path(
        settings$rundir, as.character(100 + i), "segments",
        sprintf("segment_%03d", segment), "run", "1", "sipnet.param"
      ))
      expected_amax <- if (segment == 2) {
        if (i == 2) 12 else 6
      } else {
        if (i == 4) 4 else 2
      }
      expect_equal(params$V2[params$V1 == "aMax"], expected_amax)
      expect_equal(params$V2[params$V1 == "soilRespQ10"], if (i == 3) 4 else 1)
      expect_equal(params$V2[params$V1 == "aMaxFrac"], if (segment == 2) 0.8 else 0.2)
      expect_equal(params$V2[params$V1 == "frozenSoilEff"], 0.35)
    }
  }
  expect_error(write_segmented_configs.SIPNET(settings, design[1, ]), "ens_num")
  expect_error(write_segmented_configs.SIPNET(settings, design[c(1, 3, 2, 4), ]), "labels")

  settings$outdir <- file.path(pth, "vegetation")
  settings$rundir <- file.path(settings$outdir, "run")
  settings$modeloutdir <- file.path(settings$outdir, "out")
  settings$host$rundir <- settings$rundir
  settings$host$outdir <- settings$modeloutdir
  settings$pfts$soil <- NULL
  sa.samples$soil <- NULL
  manifest <- manifest[manifest$pft_name != "soil", ]
  dir.create(settings$outdir)
  save(sa.samples, file = PEcAn.uncertainty::sensitivity.filename(
    settings, "sensitivity.samples", "Rdata", all.var.yr = TRUE
  ))
  utils::write.csv(manifest, file.path(settings$outdir, "runs_manifest.csv"), row.names = FALSE)
  for (run_id in unique(manifest$run_id)) {
    run_path <- file.path(settings$rundir, run_id)
    dir.create(run_path, recursive = TRUE)
    writeLines("original", file.path(run_path, "job.sh"))
  }
  expect_length(write_segmented_configs.SIPNET(settings, design), 3)
  params <- utils::read.table(file.path(
    settings$rundir, "102", "segments", "segment_002", "run", "1", "sipnet.param"
  ))
  expect_equal(params$V2[params$V1 == "aMax"], 12)
  expect_equal(params$V2[params$V1 == "aMaxFrac"], 0.8)
})
