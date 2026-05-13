test_that("runModule.run.write.configs uses input_design row count", {
  settings <- PEcAn.settings::Settings(
    ensemble = list(
      size = 3,
      samplingspace = list(parameters = list(method = "uniform"))
    ),
    database = list(bety = list(write = FALSE)),
    pfts = list(list(posterior.files = "post.distns.Rdata"))
  )
  input_design <- data.frame(param = seq_len(5))
  captured <- new.env(parent = emptyenv())

  mockery::stub(
    runModule.run.write.configs,
    "PEcAn.workflow::run.write.configs",
    function(settings, ensemble.size, input_design, write, posterior.files, overwrite) {
      captured$ensemble.size <- ensemble.size
      captured$input_design <- input_design
      list(
        ensemble = list(ensemble.id = 123),
        pfts = settings$pfts
      )
    }
  )

  result <- runModule.run.write.configs(
    settings = settings,
    input_design = input_design
  )

  expect_equal(captured$ensemble.size, 5)
  expect_identical(captured$input_design, input_design)
  expect_equal(result$ensemble$ensemble.id, 123)
})

test_that("run.write.configs validates param against the shortest trait sample bank", {
  withr::with_tempdir({
    trait.samples <- list(
      pftA = list(SLA = seq_len(4)),
      pftB = list(SLA = seq_len(3))
    )
    sa.samples <- list()
    runs.samples <- list()
    env.samples <- list()
    ensemble.samples <- list()
    save(
      trait.samples,
      sa.samples,
      runs.samples,
      env.samples,
      ensemble.samples,
      file = file.path(getwd(), "samples.Rdata")
    )

    settings <- list(
      outdir = getwd(),
      database = list(),
      model = list(type = "FAKE"),
      ensemble = list(size = 2),
      pfts = list(
        list(name = "pftA", posteriorid = NULL),
        list(name = "pftB", posteriorid = NULL)
      )
    )
    run_write_configs <- PEcAn.workflow::run.write.configs
    mockery::stub(
      run_write_configs,
      "PEcAn.logger::logger.error",
      function(...) stop(paste(...), call. = FALSE)
    )

    expect_error(
      run_write_configs(
        settings = settings,
        ensemble.size = 2,
        input_design = data.frame(param = c(1L, 4L)),
        write = FALSE,
        overwrite = FALSE
      ),
      "input_design\\$param includes indices beyond the available parameter sample bank"
    )
  })
})
