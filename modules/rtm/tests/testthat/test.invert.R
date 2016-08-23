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
settings <- default.settings.prospect
settings$model <- function(params) spectral.response(prospect(params,5)[,1], sensor)
settings$ngibbs.min <- 1000
settings$ngibbs.step <- 500
settings$do.lsq.first <- FALSE
settings$nchains <- 3
#test <- invert.auto(obs, settings, return.samples=TRUE, save.samples=NULL, quiet=FALSE)
#test_that("Inversion output is list of length 2", {
              #expect_is(test, "list")
              #expect_equal(length(test), 2)
#})

test.parallel <- invert.auto(obs, settings, quiet=TRUE)
test_that("Parallel inversion output is list of length 2", {
              expect_is(test.parallel, "list")
              expect_equal(length(test.parallel), 2)
})

test_that("Parallel inversion output produces distinct chains", {
              expect_false(identical(test.parallel$samples[[1]],
                                     test.parallel$samples[[2]]))
})

mus <- unlist(test.parallel$results[1:5])
diag_table <- rbind(params, mus, mus - params)
rownames(diag_table) <- c("True", "Inversion", "Inv. - True")
colnames(diag_table) <- names(params)
print(diag_table)
