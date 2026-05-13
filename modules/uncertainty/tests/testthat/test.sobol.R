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
      SLA = seq_len(9)
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

    # 4 independent factors - param, met, poolinitcond, events (no parent)
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

    # events should have independent indices, not identical to met
    # (quasi-random design makes exact equality extremely unlikely)
    expect_false(identical(result$X$events, result$X$met))

    # parameter indices stay within bank range (2*N = 8)
    expect_true(all(result$X$param >= 1))
    expect_true(all(result$X$param <= 8))

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
    # param bank = 2*N = 10, total runs = N*(k+2) = 15
    expect_equal(captured$ensemble.size, 10)
    expect_identical(
      captured$posterior.files,
      c("post1.distns.Rdata", "post2.distns.Rdata")
    )
    expect_identical(captured$ens.sample.method, "uniform")
    expect_equal(nrow(result$X), 15)
    expect_true(all(result$X$param >= 1))
    expect_true(all(result$X$param <= 10))
  })
})

test_that("compute_sobol_indices recovers analytic Ishigami indices", {
  # validates the full pipeline, design construction, ensemble.output round
  # trip, and sensobol estimator, against a function with known indices
  # sensobol::ishigami_Fun internally uses A=2, B=1 and rescales [0,1] to
  # [-pi, pi]
  skip_if_not_installed("Rfast")

  withr::with_tempdir({
    a_const <- 2
    b_const <- 1
    v1   <- 0.5 * (1 + b_const * pi^4 / 5)^2
    v2   <- a_const^2 / 8
    v13  <- 0.5 * (1 + 2 * b_const * pi^4 / 5 + b_const^2 * pi^8 / 9) - v1
    vtot <- v1 + v2 + v13
    expected_si <- c(x1 = v1 / vtot,         x2 = v2 / vtot, x3 = 0)
    expected_ti <- c(x1 = (v1 + v13) / vtot, x2 = v2 / vtot, x3 = v13 / vtot)

    n_base <- 2L^13
    params <- c("x1", "x2", "x3")
    sobol_obj <- list(
      N = n_base,
      params = params,
      backend = "sensobol",
      matrices = c("A", "B", "AB"),
      first = "saltelli",
      total = "jansen",
      factor_metadata = data.frame(
        factor = params,
        source_type = params,
        source_tag = c(NA_character_, "x2", "x3"),
        stringsAsFactors = FALSE
      )
    )

    mat <- sensobol::sobol_matrices(
      matrices = sobol_obj$matrices,
      N = sobol_obj$N,
      params = sobol_obj$params,
      order = "first",
      type = "QRN"
    )
    y <- sensobol::ishigami_Fun(mat)
    ensemble.output <- as.list(y)
    save(ensemble.output, file = file.path(
      getwd(),
      "ensemble.output.NOENSEMBLEID.Ishigami.NA.NA.Rdata"
    ))

    result <- compute_sobol_indices(
      outdir = getwd(),
      sobol_obj = sobol_obj,
      var = "Ishigami"
    )

    pick <- function(sens, par) {
      result$original[result$sensitivity == sens & result$parameters == par]
    }

    # tol 0.01 is loose; empirical max err at N=2^13 is ~6e-4
    for (par in params) {
      expect_lt(abs(pick("Si", par) - unname(expected_si[par])), 0.01)
      expect_lt(abs(pick("Ti", par) - unname(expected_ti[par])), 0.01)
    }
  })
})

test_that(".map_sobol_to_indices preserves uniformity for QRN inputs", {
  # the discretizer maps QRN draws on [0, 1) to integer indices on
  # {1, ..., size}; verify it does not over or under sample any bin so the
  # cross matrix structure that Saltelli's estimator relies on is preserved
  qrn <- sensobol::sobol_matrices(N = 2048L, params = "x", type = "QRN")
  x <- as.numeric(qrn[, "x"])

  for (size in c(5L, 10L, 50L)) {
    idx <- PEcAn.uncertainty:::.map_sobol_to_indices(x, size)
    counts <- as.integer(table(idx))
    expected_per_bin <- length(idx) / size
    rel_dev <- max(abs(counts - expected_per_bin)) / expected_per_bin

    expect_length(counts, size)
    expect_true(all(idx >= 1L))
    expect_true(all(idx <= size))
    # QRN is space-filling so deviation stays well under 5% even at size=50
    expect_lt(rel_dev, 0.05)
  }

  # defensive pmin clamps the x = 1.0 edge case which floor would push to size+1
  edge <- PEcAn.uncertainty:::.map_sobol_to_indices(
    c(0, 0.5, 0.9999, 1.0), 5L
  )
  expect_identical(edge, c(1L, 3L, 5L, 5L))
})

test_that("Sobol design warns only when user method overrides QRN sampling", {
  # independent Sobol factors always get QRN columns, so any non default
  # method declared on the factor in <ensemble><samplingspace> is silently
  # dropped. We surface that drop with a warn. The default "sampling" stays
  # quiet because it matches input.ens.gen's own default and most existing
  # configs leave it as-is
  withr::with_tempdir({
    base_settings <- make_sobol_settings(getwd())

    # positive case: looping on met is non-default, expect the warn
    settings <- base_settings
    settings$ensemble$samplingspace$met$method <- "looping"
    captured <- capture.output(
      result <- generate_joint_ensemble_design(
        settings = settings, ensemble_size = 4, sobol = TRUE
      ),
      type = "message"
    )
    joined <- gsub("\\s+", " ", paste(captured, collapse = " "))
    expect_true(any(grepl("WARN", captured)))
    expect_true(grepl("\\bmet\\b", joined))
    expect_true(grepl("looping", joined))
    expect_true(grepl("independent Sobol factor", joined))
    # design itself must still be valid; the warning is informational only
    expect_equal(nrow(result$X), 24)

    # negative case: every factor uses the default "sampling", stay quiet
    settings <- base_settings
    settings$ensemble$samplingspace$poolinitcond$method <- "sampling"
    captured <- capture.output(
      generate_joint_ensemble_design(
        settings = settings, ensemble_size = 4, sobol = TRUE
      ),
      type = "message"
    )
    joined <- gsub("\\s+", " ", paste(captured, collapse = " "))
    expect_false(grepl("independent Sobol factor", joined))
  })
})

test_that("compute_sobol_indices filters multisite outdir and passes boot through", {
  # in multisite run each site writes its own
  # ensemble.output.<eid>.<var>.<startyr>.<endyr>.Rdata into outdir
  # ensemble_id picks the right file for one site without staging
  # or symlinks. boot pass through gives sensobol bootstrap CI columns.
  skip_if_not_installed("Rfast")
  withr::with_tempdir({
    sobol_obj <- list(
      N = 4L, params = c("param", "met"), backend = "sensobol",
      matrices = c("A", "B", "AB"), first = "saltelli", total = "jansen",
      factor_metadata = data.frame(
        factor = c("param", "met"), source_type = c("param", "met"),
        source_tag = c(NA_character_, "met"), stringsAsFactors = FALSE
      )
    )

    # two sites in the same outdir. Use distinct random Y vectors so the
    # resulting Si/Ti differ between sites; Sobol indices are scale invariant
    # so y_b = c * y_a would not produce different indices.
    set.seed(1); ensemble.output <- as.list(rnorm(16))
    save(ensemble.output, file = "ensemble.output.SITEA.NPP.2016.2024.Rdata")
    set.seed(2); ensemble.output <- as.list(rnorm(16))
    save(ensemble.output, file = "ensemble.output.SITEB.NPP.2016.2024.Rdata")

    # multi match guard, no ensemble_id and two files match var=NPP
    expect_error(
      compute_sobol_indices(getwd(), sobol_obj, var = "NPP"),
      "Multiple ensemble outputs"
    )

    # ensemble_id picks the right file for each site
    res_a <- compute_sobol_indices(getwd(), sobol_obj, var = "NPP",
                                   ensemble_id = "SITEA")
    res_b <- compute_sobol_indices(getwd(), sobol_obj, var = "NPP",
                                   ensemble_id = "SITEB")
    expect_equal(nrow(res_a), 4)
    expect_false(identical(res_a$original, res_b$original))

    # bad ensemble_id surfaces the eid in the error
    expect_error(
      compute_sobol_indices(getwd(), sobol_obj, var = "NPP",
                            ensemble_id = "NOSUCHID"),
      "NOSUCHID"
    )

    # boot pass through gives bootstrap CI columns from sensobol
    res_boot <- compute_sobol_indices(getwd(), sobol_obj, var = "NPP",
                                      ensemble_id = "SITEA",
                                      boot = TRUE, R = 50L)
    expect_true(all(c("low.ci", "high.ci") %in% colnames(res_boot)))
    expect_false(any(c("low.ci", "high.ci") %in% colnames(res_a)))

    # function writes sobol.design.* and sobol.indices.* siblings next to
    # the ensemble.output file so callers can reload the design and indices
    expect_true(file.exists("sobol.design.SITEA.NPP.2016.2024.Rdata"))
    expect_true(file.exists("sobol.indices.SITEA.NPP.2016.2024.Rdata"))
  })
})
