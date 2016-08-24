# Test slow inversion
library(PEcAnRTM)
library(testthat)
context("PROSPECT R inversion")
data(sensor.rsr)
params <- c('N' = 1.4, 
            'Cab' = 40,
            'Car' = 8,
            'Cw' = 0.01,
            'Cm' = 0.01)
obs.raw <- prospect(params, 5)[,1] + generate.noise()
sensor <- "identity"
obs <- spectral.response(obs.raw, sensor)

invert.options <- default.settings.prospect
invert.options$model <- function(params) spectral.response(prospect(params,5)[,1], sensor)
invert.options$ngibbs.min <- 1000
invert.options$ngibbs.step <- 500
invert.options$do.lsq <- FALSE
invert.options$nchains <- 3

save.samples <- "samps.RData"
output_tests <- function(output){
    test_that("Parallel inversion output is list of length 2", {
                  expect_is(output, "list")
                  expect_equal(length(output), 2)
                             })

    test_that("Parallel inversion output produces distinct chains", {
                  expect_false(identical(output$samples[[1]],
                                         output$samples[[2]]))
                             })

    test_that("Saving samples is successful", {
                  expect_true(file.exists(save.samples))
                             })
    file.remove(save.samples)
}

test.parallel <- invert.auto(obs, invert.options, return.samples = TRUE,
                             save.samples = save.samples)
output_tests(test.parallel)

mus <- unlist(test.parallel$results[1:5])
diag_table <- rbind(params, mus, mus - params)
rownames(diag_table) <- c("True", "Inversion", "Inv. - True")
colnames(diag_table) <- names(params)
print(diag_table)

# Run in series, with settings that facilitate convergence
#obs <- prospect(params, 5)
#invert.options$nchains <- 2
#invert.options$do.lsq <- TRUE
#test.serial <- invert.auto(obs, invert.options, return.samples = TRUE,
                           #save.samples = save.samples, parallel = FALSE,
                           #quiet = FALSE)

