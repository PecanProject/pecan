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
    true_rsd <- mean(sum((observed - model(true_params)) ^ 2))
    true_params['residual'] <- true_rsd
    prior <- prospect_bt_prior(5)
    threshold <- 1.3
    custom_settings <- list(init = list(iterations = 2000),
                            loop = list(iterations = 1000),
                            other = list(threshold = threshold,
                                         verbose_loglike = FALSE))
    samples <- invert_bt(observed = observed, model = model, prior = prior,
                         custom_settings = custom_settings)

    samples_burned <- PEcAn.assim.batch::autoburnin(BayesianTools::getSample(samples, coda = TRUE), method = 'gelman.plot', threshold = threshold)

    mean_estimates <- do.call(cbind, summary(samples_burned)[c('statistics', 'quantiles')])

    test_that('Mean estimates are within 10% of true values',
              expect_equal(true_params, mean_estimates[names(true_params),'Mean'], tol = 0.1))

    # Compare observation with predicted interval
    if (interactive()) {
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
        mid <- colMeans(prosp_mat)
        lo <- apply(prosp_mat, 2, quantile, 0.025)
        hi <- apply(prosp_mat, 2, quantile, 0.975)
        outside <- which(observed < lo | observed > hi)
        plot(observed, type = 'l')
        lines(mid, col = 'red')
        lines(lo, col = 'red', lty = 'dashed')
        lines(hi, col = 'red', lty = 'dashed')
        orng <- rgb(1, 0.5, 0, 0.2)
        abline(v = outside, col = orng)
        legend(
            'topright',
            c('observed', 'predictive interval', 'outside PI'),
            lty = c('solid', 'dashed', 'solid'),
            col = c('black', 'red', 'orange'),
            lwd = c(1, 1, 2)
        )
        print(paste0(
            length(outside), '/', 2101,
            ' = ',
            format(length(outside) / 2101 * 100, digits = 2),
            '% of values outside 95% CI'
        ))
    }
}
