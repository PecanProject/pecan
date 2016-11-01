# Test slow inversion
library(PEcAnRTM)
context("PROSPECT R inversion")
params <- c('N' = 1.4, 
            'Cab' = 40,
            'Car' = 8,
            'Cw' = 0.01,
            'Cm' = 0.01)
sensor <- "identity"
data(sensor.rsr)
generate_obs <- function(i){
  obs.raw <- prospect(params, 5)[,1] + generate.noise()
  obs <- spectral.response(obs.raw, sensor)
  return(obs)
}

n_obs <- 3
obs <- do.call(cbind, lapply(1:n_obs, generate_obs))

invert.options <- default.settings.prospect
invert.options$model <- function(params) spectral.response(prospect(params,5)[,1], sensor)
invert.options$ngibbs.min <- 5000
invert.options$ngibbs.step <- 2000
invert.options$ngibbs.max <- 100000
invert.options$do.lsq <- TRUE
invert.options$nchains <- 3
invert.options$run_first <- function(inputs) {
  fname <- paste0("testfile_", inputs$runID)
  file.create(fname, showWarnings = FALSE)
}
fname_expect <- paste0("testfile_", 1:3)

save.samples <- "samps.rds"
output_tests <- function(output) {
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

  test_that("run_first function creates three testfiles", {
              expect_true(all(file.exists(fname_expect)))
            })
}

diag_table <- function(output, params){
  mus <- unlist(output$results[1:length(params)])
  diag_table <- rbind(params, mus, mus - params)
  rownames(diag_table) <- c("True", "Inversion", "Inv. - True")
  colnames(diag_table) <- names(params)
  print(diag_table)
}

diag_plot <- function(output, ...) {
    samps <- PEcAn.assim.batch::makeMCMCList(output$samples)
    plot(samps, ...)
}

test.parallel <- invert.auto(obs, invert.options,
                             return.samples = TRUE,
                             save.samples = save.samples,
                             quiet = FALSE)
output_tests(test.parallel)
diag_table(test.parallel, params)


# Run in series, with settings that facilitate convergence
obs <- prospect(params, 5)[,1]
invert.options$nchains <- 3
invert.options$do.lsq <- TRUE
test.serial <- invert.auto(obs, invert.options,
                           return.samples = TRUE,
                           save.samples = save.samples,
                           parallel = FALSE,
                           quiet = FALSE)

output_tests(test.serial)
diag_table(test.serial, params)

pdf("diag_plots.pdf")
diag_plot(test.parallel)
diag_plot(test.serial)
dev.off()
