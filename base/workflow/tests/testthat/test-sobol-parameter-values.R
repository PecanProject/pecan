test_that("ensemble designs select both written and saved parameter values", {
  model <- "ISSUE4221"
  writer_name <- paste0("write.config.", model)
  writer <- function(defaults, trait.values, settings, run.id) {
    writeLines(as.character(trait.values$pft1$SLA),
               file.path(settings$rundir, run.id, "config.txt"))
  }
  old_writer <- get0(writer_name, envir = .GlobalEnv, inherits = FALSE)
  assign(writer_name, writer, envir = .GlobalEnv)
  withr::defer({
    if (is.null(old_writer)) rm(list = writer_name, envir = .GlobalEnv)
    else assign(writer_name, old_writer, envir = .GlobalEnv)
  })
  bundle <- list(
    trait.samples = list(pft1 = list(SLA = 101:104)),
    ensemble.samples = list(pft1 = data.frame(SLA = c(10, 20, 30, 40))),
    sa.samples = list(), runs.samples = list(), env.samples = list()
  )
  original_bundle <- serialize(bundle, NULL)
  cases <- list(
    list(sobol = TRUE, indices = NULL),
    list(sobol = TRUE, indices = c(4L, 2L, 4L, 1L, 3L, 1L, 2L, 3L)),
    list(sobol = FALSE, indices = NULL),
    list(sobol = FALSE, indices = c(4L, 2L, 4L, 1L)),
    list(sobol = FALSE, indices = NULL, omit_param = TRUE)
  )
  for (case in cases) {
    directory <- withr::local_tempdir()
    size <- if (case$sobol) 2 else 4
    settings <- PEcAn.settings::as.Settings(list(
      outdir = directory, rundir = file.path(directory, "run"),
      modeloutdir = file.path(directory, "out"),
      host = list(name = "localhost"), model = list(type = model),
      run = list(site = list(id = 1), inputs = list(met = list(path = c("m1", "m2")))),
      ensemble = list(size = size, samplingspace = list(
        parameters = list(method = "uniform"), met = list(method = "looping"))),
      pfts = list(list(name = "pft1"))
    ))
    design <- PEcAn.uncertainty::generate_joint_ensemble_design(
      settings, ensemble_size = size, samples = bundle, sobol = case$sobol)
    if (!is.null(case$indices)) {
      design$design_matrix$param <- case$indices
      design$X$param <- case$indices
    }
    if (isTRUE(case$omit_param)) {
      design$design_matrix$param <- NULL
      design$X$param <- NULL
    }
    original_design <- serialize(design, NULL)
    settings <- runModule.run.write.configs(settings, input_design = design)
    run_ids <- readLines(file.path(settings$rundir, "runs.txt"))
    written <- unname(vapply(run_ids, function(id) {
      scan(file.path(settings$rundir, id, "config.txt"), what = numeric(), quiet = TRUE)
    }, numeric(1)))
    indices <- design$design_matrix$param
    if (is.null(indices)) indices <- seq_len(size)
    expected <- bundle$ensemble.samples$pft1$SLA[indices]
    expect_equal(written, expected)
    saved <- new.env()
    load(PEcAn.uncertainty::ensemble.filename(
      settings, "ensemble.samples", "Rdata", all.var.yr = TRUE), envir = saved)
    expect_equal(saved$ens.samples$pft1$SLA, expected)
    source <- new.env()
    load(file.path(directory, "samples.Rdata"), envir = source)
    expect_setequal(ls(saved), c(
      "ens.run.ids", "ens.ensemble.id", "ens.samples", "pft.names", "trait.names"))
    expect_identical(source$ensemble.samples, bundle$ensemble.samples)
    expect_identical(serialize(design, NULL), original_design)
    expect_identical(serialize(bundle, NULL), original_bundle)
  }
})
