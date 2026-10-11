# Tests for run.sensitivity.analysis

# SA inputs for one pft, with the output of each run from a known model,
# written where run.sensitivity.analysis looks for them
write_sa_fixture <- function(dir, trait.samples, model) {
  q <- stats::pnorm(-3:3)
  sa.samples <- list(pft1 = as.data.frame(lapply(trait.samples, stats::quantile, q),
                                          row.names = as.character(round(q * 100, 3))))
  trait.samples <- list(pft1 = trait.samples)
  pft.names <- "pft1"
  trait.names <- list(pft1 = names(trait.samples$pft1))
  save(trait.samples, sa.samples, pft.names, trait.names,
       file = file.path(dir, "samples.Rdata"))

  # each run holds one trait at a quantile and the rest at their medians
  med <- lapply(trait.samples$pft1, stats::median)
  sensitivity.output <- list(pft1 = as.data.frame(
    lapply(trait.names$pft1, function(trait) {
      runs <- as.data.frame(med)[rep(1, length(q)), , drop = FALSE]
      runs[[trait]] <- sa.samples$pft1[[trait]]
      model(runs)
    }),
    col.names = trait.names$pft1,
    row.names = rownames(sa.samples$pft1)
  ))

  settings <- list(
    outdir = dir,
    pfts = list(pft = list(name = "pft1", outdir = dir)),
    sensitivity.analysis = list(ensemble.id = "1", variable = "NPP",
                                start.year = 2001, end.year = 2001)
  )
  save(sensitivity.output, file = sensitivity.filename(
    settings, "sensitivity.output", "Rdata", all.var.yr = FALSE,
    ensemble.id = "1", variable = "NPP", start.year = 2001, end.year = 2001
  ))
  settings
}

sa_results <- function(settings) {
  run.sensitivity.analysis(settings, plot = FALSE)
  e <- new.env()
  load(sensitivity.filename(
    settings, "sensitivity.results", "Rdata", all.var.yr = FALSE, pft = NULL,
    ensemble.id = "1", variable = "NPP", start.year = 2001, end.year = 2001
  ), envir = e)
  e$sensitivity.results
}


test_that("a trait in Celsius is analysed in the units it was run in", {
  dir <- withr::local_tempdir()
  withr::local_seed(1)
  samples <- data.frame(psnTOpt = stats::rnorm(5000, 25, 3), SLA = stats::rnorm(5000, 20, 4))
  settings <- write_sa_fixture(dir, samples, function(x) 10 - 0.05 * (x$psnTOpt - 28)^2 + 0.2 * x$SLA)
  vd <- sa_results(settings)$pft1$variance.decomposition.output

  expect_equal(vd$sensitivities[["psnTOpt"]], -0.1 * (stats::median(samples$psnTOpt) - 28), tolerance = 0.02)
  expect_equal(vd$variances[["psnTOpt"]], stats::var(-0.05 * (samples$psnTOpt - 28)^2), tolerance = 0.01)
  # CV and elasticity are still taken in K
  t_K <- samples$psnTOpt + 273.15
  y <- 10 - 0.05 * (samples$psnTOpt - 28)^2 + 0.2 * stats::median(samples$SLA)
  expect_equal(vd$coef.vars[["psnTOpt"]], get.coef.var(t_K))
  expect_equal(vd$elasticities[["psnTOpt"]],
               -0.1 * (stats::median(samples$psnTOpt) - 28) * stats::median(t_K) / stats::median(y),
               tolerance = 0.02)
  expect_equal(vd$sensitivities[["SLA"]], 0.2, tolerance = 0.01)
})
