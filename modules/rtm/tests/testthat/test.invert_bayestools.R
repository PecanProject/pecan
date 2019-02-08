# devtools::load_all('.')
library(PEcAnRTM)
library(testthat)
context('Inversion using BayesianTools')

skip_on_travis()

skip_if_not(
  interactive(),
  "Long-running tests. Only run in interactive mode."
)

set.seed(12345678)
true_prospect <- defparam('prospect_5')
true_params <- c(true_prospect, residual = 0.01)
model <- function(x) prospect(x, 5)[,1]
true_model <- model(true_prospect)
noise <- rnorm(length(true_model), 0, true_params['residual'])
observed <- true_model + noise
if (interactive()) {
  plot(400:2500, observed, type = 'l')
  lines(400:2500, true_model, col = 'red')
  legend("topright", c('observation', 'pseudo-data'), col = c('black', 'red'), lty = 'solid')
}
# Alternate test, using observed spectra
# data("testspec")
# observed <- testspec_ACRU[,2]
prior <- prospect_bt_prior(5)
threshold <- 1.1
custom_settings <- list(init = list(iterations = 2000),
                        loop = list(iterations = 1000),
                        other = list(threshold = threshold,
                                     verbose_loglike = FALSE))
samples <- invert_bt(observed = observed, model = model, prior = prior,
                     custom_settings = custom_settings)

samples_mcmc <- BayesianTools::getSample(samples, coda = TRUE)
samples_burned <- PEcAn.assim.batch::autoburnin(samples_mcmc, method = 'gelman.plot', threshold = threshold)

mean_estimates <- do.call(cbind, summary(samples_burned, quantiles = c(0.01, 0.5, 0.99))[c('statistics', 'quantiles')])

test_that(
  'True values are within 95% confidence interval',
  {
    expect_true(all(true_params > mean_estimates[,'1%']))
    expect_true(all(true_params < mean_estimates[,'99%']))
  }
)

test_that(
  'Mean estimates are within 10% of true values',
  expect_equal(true_params, mean_estimates[names(true_params), 'Mean'], tol = 0.1)
)

# Compare observation with predicted interval
samp_mat <- as.matrix(samples_burned)
nsamp <- 2500
prosp_mat <- matrix(0.0, nsamp, 2101)
message('Generating PROSPECT confidence interval')
pb <- txtProgressBar(style = 3)
for (i in seq_len(nsamp)) {
  setTxtProgressBar(pb, i/nsamp)
  samp_param <- samp_mat[sample.int(nrow(samp_mat), 1),]
  prosp_mat[i,] <- rnorm(2101, model(samp_param[-6]), samp_param[6])
}
close(pb)
mid <- colMeans(prosp_mat)
lo <- apply(prosp_mat, 2, quantile, 0.025)
hi <- apply(prosp_mat, 2, quantile, 0.975)
pi_y <- c(lo, rev(hi))
pi_x <- c(seq_along(lo), rev(seq_along(hi)))
outside <- which(observed < lo | observed > hi)

test_that(
  '95% predictive interval overlaps around 95% of data',
  expect_lt(100 * length(outside) / length(true_model), 7.5)

)
if (interactive()) {
  par(mfrow = c(1,1))
  plot(observed, type = 'l')
  lines(mid, col = 'red')
  polygon(pi_x, pi_y, col = rgb(1, 0, 0, 0.2), border = 'red', lty = 'dashed')
  legend(
    'topright',
    c('observed', 'mean prediction', 'predictive interval'),
    lty = c('solid', 'solid', 'dashed'),
    col = c('black', 'red', 'red')
  )
}
