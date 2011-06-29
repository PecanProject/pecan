#test_that('sensitivity analysis inputs make sense') {
#  expect_that(rownames(sa.agb[[1]] == 

load('~/pecan/tests/test.sensitivity.plot.inputs.Rdata')


test_that('sensitivity analysis function works', {
  sa.sample <- sensitivity.results$sensitivity.plot.inputs$sa.samples
  sa.splinefuns <- sensitivity.results$sensitivity.plot.inputs$sa.splinefuns
  traits <- names(sa.sample)
  
  expect_true(all(lapply(list(sa.splinefuns, sa.sample), is.list)))
  expect_equal(length(traits), 15)
  sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[1]],
                                              sa.samples = sa.samples[[1]],
                                              sa.output = sa.agb[[1]],
                                              outdir = outdir)
  expect_true(all(unlist(lapply(list(sensitivity.results,
                                     sensitivity.results$sensitivity.plot.inputs,
                                     sensitivity.results$variance.decomposition.plot.inputs),
                                is.list))))
  expect_equivalent(names(sensitivity.results),
                    c("sensitivity.plot.inputs","variance.decomposition.plot.inputs"))
})

## Testing variance decomposition plots
source('R/plots.R')
plot.variance.decomposition(sensitivity.results$variance.decomposition.plot.inputs, outdir)
