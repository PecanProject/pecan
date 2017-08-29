# devtools::load_all('.')
library(PEcAnRTM)
library(testthat)
context('Inversion using BayesianTools')

if (Sys.getenv('CI') == 'true') {
    message('Skipping inversion tests on CI system')
} else {
    set.seed(12345678)
    true_params <- defparam('prospect_5')
    model <- function(x) prospect(x, 5)[,1]
    observed <- model(true_params) + generate.noise()
    prior <- prospect_bt_prior(5)
    threshold <- 1.3
    custom_settings <- list(init = list(iterations = 2000),
                            loop = list(iterations = 1000),
                            other = list(threshold = threshold))
    samples <- invert_bt(observed = observed, model = model, prior = prior,
                         custom_settings = custom_settings)

    samples_burned <- PEcAn.assim.batch::autoburnin(BayesianTools::getSample(samples, coda = TRUE), method = 'gelman.plot', threshold = threshold)
    mean_estimates <- do.call(cbind, summary(samples_burned)[c('statistics', 'quantiles')])

    test_that('Mean estimates are within 10% of true values',
              expect_equal(true_params, mean_estimates[names(true_params),'Mean'], tol = 0.1))
}
