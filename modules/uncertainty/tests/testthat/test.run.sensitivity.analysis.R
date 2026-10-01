test_that("Celsius trait samples are passed to sensitivity analysis unchanged", {
  skip_if_not_installed("testthat")

  settings <- list(
    outdir = tempfile("pecan_sa_"),
    sensitivity.analysis = list(
      ensemble.id = 1,
      variable = "GPP",
      start.year = 2000,
      end.year = 2000
    ),
    pfts = list(
      list(
        name = "pft1",
        outdir = tempfile("pecan_pft_")
      )
    )
  )

  dir.create(settings$outdir, recursive = TRUE)
  dir.create(settings$pfts[[1]]$outdir, recursive = TRUE)

  trait.samples <- list(
    pft1 = list(
      plant_min_temp = c(0, 10, 20),
      Vcmax = c(40, 50, 60)
    )
  )

  sa.samples <- list(
    pft1 = data.frame(
      plant_min_temp = c(0, 20),
      Vcmax = c(40, 60),
      row.names = c("15.9", "84.1")
    )
  )

  trait.names <- list(
    pft1 = c("plant_min_temp", "Vcmax")
  )

  sa.run.ids <- list(
    pft1 = c(1, 2)
  )

  samples <- list(
    trait.samples = trait.samples,
    sa.samples = sa.samples,
    trait.names = trait.names,
    sa.run.ids = sa.run.ids
  )

  samples.file <- file.path(settings$outdir, "samples.Rdata")
  save(samples, file = samples.file)

  variable.fn <- PEcAn.utils::convert.expr("GPP")$variable.drv

  sens.file <- sensitivity.filename(
    settings,
    "sensitivity.output",
    "Rdata",
    all.var.yr = FALSE,
    ensemble.id = 1,
    variable = variable.fn,
    start.year = 2000,
    end.year = 2000
  )

  sensitivity.output <- list(
    pft1 = data.frame(
      plant_min_temp = c(1, 2),
      Vcmax = c(3, 4)
    )
  )

  save(sensitivity.output, file = sens.file)

  captured_traits <- NULL

  fake_sensitivity_analysis <- function(
    trait.samples,
    sa.samples,
    sa.output,
    outdir
  ) {
    captured_traits <<- trait.samples

    list(
      variance.decomposition.output = NULL,
      sensitivity.output = NULL
    )
  }

  testthat::local_mocked_bindings(
    sensitivity.analysis = fake_sensitivity_analysis,
    .package = "PEcAn.uncertainty"
  )

  run.sensitivity.analysis(
    settings = settings,
    plot = FALSE,
    ensemble.id = 1,
    variable = "GPP",
    start.year = 2000,
    end.year = 2000,
    pfts = "pft1"
  )

  expect_equal(
    captured_traits$plant_min_temp,
    c(0, 10, 20)
  )

  expect_equal(
    captured_traits$Vcmax,
    c(40, 50, 60)
  )
})
