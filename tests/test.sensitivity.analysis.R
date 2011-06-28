#test_that('sensitivity analysis inputs make sense') {
#  expect_that(rownames(sa.agb[[1]] == 

load('~/pecan/tests/test.sensitivity.plot.inputs.Rdata')


test_that('sensitivity analysis function works') {
  sa.sample <- sensitivity.results$sensitivity.plot.inputs$sa.samples
  sa.splinefuns <- sensitivity.results$sensitivity.plot.inputs$sa.splinefuns
  traits <- names(sa.sample)

  expect_true(all(lapply(list(sa.splinefuns, sa.sample), is.list)))
  expect_equal(length(traits), 15)
  sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[1]],
                                              sa.samples = sa.samples[[1]],
                                              sa.output = sa.agb[[1]],
                                              outdir = outdir)
  expect_true(all(lapply(list(sensitivity.results, sensitivity.results[[1]], sensitivity.results[[2]]),
                         is.list)))
  expect_equivalent(names(sensitivity.results),
                    c("sensitivity.plot.inputs","variance.decomposition.plot.inputs"))
}
               
test_that('sensitivity analysis inputs are as expected') {
  expect_equal(as.numeric(rownames(sa.agb[[1]])), round(pnorm(-3:3)*100,3))
} #this should always be true if using test data 

plot.sensitivity(sa.sample = sa.sample[,1],
                 sa.splinefun = sa.splinefuns[[1]],
                 trait = colnames(sa.sample)[1],
                 y.range = pretty(range(sa.sample)))

plot.sensitivities(test.sensitivity.plot.inputs, outdir)

sensitivity.results <- sensitivity.analysis(trait.samples = trait.samples[[pft$name]],
                                                sa.samples = sa.samples[[pft$name]],
                                                sa.output = sa.agb[[pft$name]],
                                                outdir = outdir)
