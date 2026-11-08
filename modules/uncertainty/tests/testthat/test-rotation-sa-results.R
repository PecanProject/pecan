test_that("SA results follow saved runs rather than initial vegetation", {
  path <- withr::local_tempdir()
  settings <- list(
    outdir = path,
    modeloutdir = path,
    run = list(site = list(site.pft = list(veg = "pftA"))),
    pfts = list(list(name = "pftA"), list(name = "pftB")),
    sensitivity.analysis = list(
      ensemble.id = "test", start.year = 2020, end.year = 2020,
      variable = "NPP"
    )
  )
  trait.samples <- list(
    pftA = list(Vcmax = c(1, 2, 3)),
    pftB = list(Vcmax = c(2, 3, 4), SLA = c(4, 5, 6))
  )
  sa.samples <- list(
    pftA = matrix(c(1, 2, 3), 3, dimnames = list(c("5", "50", "95"), "Vcmax")),
    pftB = matrix(c(2, 3, 4, 4, 5, 6), 3,
                 dimnames = list(c("5", "50", "95"), c("Vcmax", "SLA")))
  )
  sa.run.ids <- list(pftB = matrix(
    c("low", "median", "high"), 3,
    dimnames = list(c("5", "50", "95"), "Vcmax")
  ))
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  save(trait.samples, sa.samples, sa.run.ids, pft.names, trait.names,
       file = file.path(path, "samples.Rdata"))
  file.copy(file.path(path, "samples.Rdata"),
            file.path(path, "sensitivity.samples.Rdata"))
  filename <- function(settings, prefix, ...) file.path(path, paste0(prefix, ".Rdata"))

  get_results <- get.results
  mockery::stub(get_results, "sensitivity.filename", filename)
  mockery::stub(get_results, "read.sa.output", function(pft.name, traits, ...) {
    expect_identical(pft.name, "pftB")
    expect_identical(traits, "Vcmax")
    data.frame(Vcmax = c(2, 3, 4), row.names = c("5", "50", "95"))
  })
  get_results(settings)
  output <- new.env(parent = emptyenv())
  load(file.path(path, "sensitivity.output.Rdata"), envir = output)
  expect_named(output$sensitivity.output, "pftB")

  analyze <- run.sensitivity.analysis
  mockery::stub(analyze, "sensitivity.filename", filename)
  mockery::stub(analyze, "sensitivity.analysis", function(trait.samples, sa.samples,
                                                         sa.output, ...) {
    expect_named(trait.samples, "Vcmax")
    expect_identical(colnames(sa.samples), "Vcmax")
    expect_identical(colnames(sa.output), "Vcmax")
    list(variance.decomposition.output = 1)
  })
  analyze(settings, plot = FALSE)
  load(file.path(path, "sensitivity.results.Rdata"), envir = output)
  expect_named(output$sensitivity.results, "pftB")
})

test_that("legacy SA bundles without run IDs retain their quantile tables", {
  path <- withr::local_tempdir()
  settings <- list(
    outdir = path, modeloutdir = path,
    sensitivity.analysis = list(
      ensemble.id = "legacy", start.year = 2020, end.year = 2020,
      variable = "NPP"
    )
  )
  sa.samples <- list(pftA = data.frame(
    Vcmax = c(1, 2, 3), row.names = c("5", "50", "95")
  ))
  trait.samples <- list(pftA = list(Vcmax = c(1, 2, 3)))
  save(sa.samples, trait.samples, file = file.path(path, "sensitivity.samples.Rdata"))
  collect <- get.results
  mockery::stub(collect, "sensitivity.filename", function(settings, prefix, ...) {
    file.path(path, paste0(prefix, ".Rdata"))
  })
  mockery::stub(collect, "read.sa.output", function(pft.name, traits, quantiles,
                                                  sa.run.ids, ...) {
    expect_identical(pft.name, "pftA")
    expect_identical(traits, "Vcmax")
    expect_identical(quantiles, c("5", "50", "95"))
    expect_null(sa.run.ids)
    sa.samples$pftA
  })
  collect(settings)
  output <- new.env(parent = emptyenv())
  load(file.path(path, "sensitivity.output.Rdata"), envir = output)
  expect_named(output$sensitivity.output, "pftA")
})
