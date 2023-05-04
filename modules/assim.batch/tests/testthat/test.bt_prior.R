library(PEcAn.assim.batch)
library(testthat)

context("BayesianTools prior functions")

set.seed(26353451)

prior_list <- list(list('normal', 'norm', 0.5, 2),
                   list('lognormal', 'lnorm', 1, 1),
                   list('gamma', 'gamma', 0.5, 0.5))
prior_df <- do.call(rbind.data.frame, prior_list)
colnames(prior_df) <- c('param_name', 'distn', 'parama', 'paramb')

prior <- pda.create.btprior(prior_df)

x <- c(2, 3, 4)
correct_dens <- with(prior_df, dnorm(x[1], parama[1], paramb[1], log = TRUE) + 
                        dlnorm(x[2], parama[2], paramb[2], log = TRUE) + 
                        dgamma(x[3], parama[3], paramb[3], log = TRUE))
prior_dens <- prior$density(x)

test_that('Prior returns correct density', expect_equal(correct_dens, prior_dens))

correct_mean <- with(prior_df, c(parama[1],
                                 exp(parama[2] + paramb[2]^2 / 2),
                                 parama[3] / paramb[3]))
correct_var <- with(prior_df, c(paramb[1]^2,
                                (exp(paramb[2]^2) - 1) * exp(2 * parama[2] + paramb[2]^2),
                                parama[3] / paramb[3]^2))
names(correct_mean) <- names(correct_var) <- prior_df[['param_name']]

nsamp <- 10000
prior_samples <- vapply(seq_len(nsamp), function(x) prior$sampler(), numeric(3))
prior_sampmean <- rowMeans(prior_samples)
prior_sampvar <- apply(prior_samples, 1, var)

test_that('Prior sampler returns reasonable values', {
              expect_equal(correct_mean, prior_sampmean, tolerance = 0.1)
              expect_equal(correct_var, prior_sampvar, tolerance = 0.25)
                        })
