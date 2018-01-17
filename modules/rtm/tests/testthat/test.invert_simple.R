if (interactive()) {
  devtools::load_all("~/Projects/pecan/pecan/modules/rtm")
}
if (Sys.getenv('CI') == 'true') {
    message('Skipping inversion tests on CI system')
} else {
    library(PEcAnRTM)
    library(testthat)
    context("Inversion of simple quadratic model")

    true <- c(2, 3, 4)

    nx <- 100
    x <- seq(0, 10, length.out = nx)

    model <- function(params) {
        params[1] * x^2 + params[2] * x + params[3]
    }

    set.seed(666)
    y <- as.matrix(model(true) + generate.noise(n = nx, fw = 10, sigma = 1))

    nchains <- 3
    invert.options <- list()
    invert.options$model <- model
    invert.options$prior.function <- function(p) sum(dnorm(p, 0, 30, TRUE)) 
    invert.options$inits.function <- function() rnorm(3, 0, 30)
    invert.options$inits <- invert.options$inits.function()
    invert.options$nchains <- nchains
    invert.options$run_first <- function(inputs) {
        fname <- paste0("testfile_", inputs$runID)
        file.create(fname, showWarnings = FALSE)
    }
    fname_expect <- paste0("testfile_", 1:3)
    save.samples <- "test_samples.rds"

    output_tests <- function(output) {
        test_that("Inversion output produces results and samples", {
                      expect_is(output$samples, "mcmc.list")
                      expect_true(length(output$results) > 0)
                      expect_false(is.null(output$results))
                      expect_false(any(is.na(output$results)))
                      expect_true(coda::niter(output$samples) > 100)
})

        test_that("Inversion output produces distinct chains", {
                      expect_false(identical(output$samples[[1]],
                                             output$samples[[2]]))
})

        test_that("Saving samples is successful", {
                      expect_true(file.exists(save.samples))
})

        test_that("run_first function creates three testfiles", {
                      expect_true(all(file.exists(fname_expect)))
})
    }

    invert.options$threshold <- 1.2
    samp_parallel <- invert.auto(observed = y,
                                 invert.options = invert.options, 
                                 save.samples = save.samples)
    output_tests(samp_parallel)

    invert.options$calculate.burnin <- FALSE
    invert.options$threshold <- NULL
    samp_series <- invert.auto(observed = y,
                               invert.options = invert.options, 
                               save.samples = save.samples, 
                               parallel = FALSE)
    output_tests(samp_series)

    file.remove(fname_expect, save.samples)
}
