make_sobol_settings <- function(outdir) {
  samples_file <- file.path(outdir, "samples.Rdata")
  trait.samples <- list(
    temperate = list(
      SLA = seq_len(40)
    )
  )
  save(trait.samples, file = samples_file)

  PEcAn.settings::Settings(
    outdir = outdir,
    pfts = list(list(name = "temperate", posterior.files = "post.distns.Rdata")),
    run = list(inputs = list(
      met = list(path = c("met1", "met2", "met3")),
      poolinitcond = list(path = c("ic1", "ic2")),
      events = list(path = c("evt1", "evt2", "evt3"))
    )),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling"),
        poolinitcond = list(method = "looping"),
        events = list(method = "sampling")
      )
    )
  )
}

make_parent_sobol_settings <- function(outdir) {
  samples_file <- file.path(outdir, "samples.Rdata")
  trait.samples <- list(
    temperate = list(
      SLA = seq_len(40)
    )
  )
  save(trait.samples, file = samples_file)

  PEcAn.settings::Settings(
    outdir = outdir,
    pfts = list(list(name = "temperate", posterior.files = "post.distns.Rdata")),
    run = list(inputs = list(
      met = list(path = c("met1", "met2", "met3")),
      poolinitcond = list(path = c("ic1", "ic2")),
      events = list(path = c("evt1", "evt2", "evt3"))
    )),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling"),
        poolinitcond = list(method = "looping"),
        events = list(method = "sampling", parent = "met")
      )
    )
  )
}

make_mixed_bank_sobol_settings <- function(outdir) {
  samples_file <- file.path(outdir, "samples.Rdata")
  trait.samples <- list(
    hardwood = list(
      SLA = seq_len(20)
    ),
    conifer = list(
      SLA = seq_len(10)
    )
  )
  save(trait.samples, file = samples_file)

  PEcAn.settings::Settings(
    outdir = outdir,
    pfts = list(
      list(name = "hardwood", posterior.files = "post1.distns.Rdata"),
      list(name = "conifer", posterior.files = "post2.distns.Rdata")
    ),
    run = list(inputs = list()),
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform")
      )
    )
  )
}

test_that("Sobol design treats parentless inputs as independent factors", {
  withr::with_tempdir({
    settings <- make_sobol_settings(getwd())

    result <- generate_joint_ensemble_design(
      settings = settings,
      ensemble_size = 4,
      sobol = TRUE
    )

    # 4 independent factors -- param, met, poolinitcond, events (no parent)
    # total runs = N * (k + 2) = 4 * (4 + 2) = 24
    expect_equal(nrow(result$X), 24)
    expect_equal(result$N, 4)
    expect_identical(
      result$params,
      c("param", "met", "poolinitcond", "events")
    )
    expect_identical(result$backend, "sensobol")
    expect_identical(result$matrices, c("A", "B", "AB"))
    expect_identical(result$first, "saltelli")
    expect_identical(result$total, "jansen")

    # all factor columns present
    expect_true(all(
      c("param", "met", "poolinitcond", "events") %in% names(result$X)
    ))

    # events should have independent indices -- not identical to met
    # (quasi-random design makes exact equality extremely unlikely)
    expect_false(identical(result$X$events, result$X$met))

    # parameter indices stay within bank range
    expect_true(all(result$X$param >= 1))
    expect_true(all(result$X$param <= 24))

    # input indices stay within available paths
    expect_true(all(result$X$met >= 1 & result$X$met <= 3))
    expect_true(all(result$X$poolinitcond >= 1 & result$X$poolinitcond <= 2))
    expect_true(all(result$X$events >= 1 & result$X$events <= 3))

    # factor metadata covers all independent factors
    expect_true(all(
      c("factor", "source_type", "source_tag") %in%
        names(result$factor_metadata)
    ))
    expect_identical(
      result$factor_metadata$source_type,
      c("param", "met", "poolinitcond", "events")
    )
  })
})

test_that("Sobol design respects parent-child relationships", {
  withr::with_tempdir({
    settings <- make_parent_sobol_settings(getwd())

    result <- generate_joint_ensemble_design(
      settings = settings,
      ensemble_size = 4,
      sobol = TRUE
    )

    # 3 independent factors -- param, met, poolinitcond
    # events has parent = "met" so it is NOT an independent factor
    # total runs = N * (k + 2) = 4 * (3 + 2) = 20
    expect_equal(nrow(result$X), 20)
    expect_equal(result$N, 4)
    expect_identical(
      result$params,
      c("param", "met", "poolinitcond")
    )

    # events column should still be in the design matrix (as a child)
    expect_true("events" %in% names(result$X))

    # events should NOT be in sobol_factors / factor_metadata
    expect_false("events" %in% result$params)
    expect_false("events" %in% result$factor_metadata$factor)

    # met and poolinitcond bounds
    expect_true(all(result$X$met >= 1 & result$X$met <= 3))
    expect_true(all(result$X$poolinitcond >= 1 & result$X$poolinitcond <= 2))
    # events bounds (inherited from met, but mapped to events paths)
    expect_true(all(result$X$events >= 1 & result$X$events <= 3))
  })
})

test_that("Non-Sobol design generation remains row-for-row", {
  withr::with_tempdir({
    settings <- make_sobol_settings(getwd())

    result <- generate_joint_ensemble_design(
      settings = settings,
      ensemble_size = 5,
      sobol = FALSE
    )

    expect_named(result, "X")
    expect_equal(nrow(result$X), 5)
    expect_false("backend" %in% names(result))
  })
})

test_that("Sobol regenerates parameter bank when any PFT bank is too short", {
  withr::with_tempdir({
    settings <- make_mixed_bank_sobol_settings(getwd())
    captured <- new.env(parent = emptyenv())

    mockery::stub(
      generate_joint_ensemble_design,
      "PEcAn.uncertainty::get.parameter.samples",
      function(settings, ensemble.size, posterior.files, ens.sample.method) {
        captured$ensemble.size <- ensemble.size
        captured$posterior.files <- posterior.files
        captured$ens.sample.method <- ens.sample.method
        NULL
      }
    )

    result <- generate_joint_ensemble_design(
      settings = settings,
      ensemble_size = 5,
      sobol = TRUE
    )

    # only param factor here (no inputs in samplingspace)
    # total = N * (k + 2) = 5 * (1 + 2) = 15
    expect_equal(captured$ensemble.size, 15)
    expect_identical(
      captured$posterior.files,
      c("post1.distns.Rdata", "post2.distns.Rdata")
    )
    expect_identical(captured$ens.sample.method, "uniform")
    expect_equal(nrow(result$X), 15)
    expect_true(all(result$X$param >= 1))
    expect_true(all(result$X$param <= 15))
  })
})

test_that("compute_sobol_indices matches direct sensobol results", {
  withr::with_tempdir({
    sobol_obj <- list(
      N = 4L,
      params = c("param", "met"),
      backend = "sensobol",
      matrices = c("A", "B", "AB"),
      first = "saltelli",
      total = "jansen",
      factor_metadata = data.frame(
        factor = c("param", "met"),
        source_type = c("param", "met"),
        source_tag = c(NA_character_, "met"),
        stringsAsFactors = FALSE
      )
    )

    y <- seq_len(16)
    ensemble.output <- as.list(y)
    save(ensemble.output, file = file.path(
      getwd(),
      "ensemble.output.NOENSEMBLEID.GPP.NA.NA.Rdata"
    ))

    result <- compute_sobol_indices(
      outdir = getwd(),
      sobol_obj = sobol_obj,
      var = "GPP"
    )

    expected <- tibble::as_tibble(
      sensobol::sobol_indices(
        matrices = c("A", "B", "AB"),
        Y = y,
        N = 4L,
        params = c("param", "met"),
        first = "saltelli",
        total = "jansen",
        order = "first",
        boot = FALSE
      )$results
    )

    expect_equal(
      result[, c("parameters", "sensitivity", "original")],
      expected[, c("parameters", "sensitivity", "original")]
    )
    expect_true(file.exists(file.path(
      getwd(),
      "sobol.design.NOENSEMBLEID.GPP.NA.NA.Rdata"
    )))
    expect_true(file.exists(file.path(
      getwd(),
      "sobol.indices.NOENSEMBLEID.GPP.NA.NA.Rdata"
    )))
  })
})
