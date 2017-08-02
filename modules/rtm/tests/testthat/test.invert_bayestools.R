#devtools::load_all('.')
library(PEcAnRTM)
library(testthat)
context('Inversion using BayesianTools')

true_params <- defparam('prospect_5')
model <- function(x) prospect(x, 5)[,1]
observed <- model(true_params) + generate.noise()
prior <- prospect_bt_prior(5)
custom_settings <- list()
samples <- invert_bt(observed = observed, model = model, prior = prior,
                     custom_settings = list())

samples_burned <- PEcAn.assim.batch::autoburnin(BayesianTools::getSample(samples, coda = TRUE), method = 'gelman.plot')
mean_estimates <- do.call(cbind, summary(samples_burned)[c('statistics', 'quantiles')])

test_that('Mean estimates are within 10% of true values', 
          expect_equal(true_params, mean_estimates[seq_along(true_params),'Mean'], tol = 0.1))
