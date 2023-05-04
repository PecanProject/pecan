library(PEcAn.assim.batch)
library(testthat)

context("Autoburnin functions")

# Generate some simple data for testing convergence check
n1 <- 7200
n2 <- 800
mu_common <- 0
chain1 <- coda::mcmc(cbind("a" = c(rnorm(n1, 5), rnorm(n2, mu_common)),
                           "b" = c(rnorm(n1, 5), rnorm(n2, mu_common))))
chain2 <- coda::mcmc(cbind("a" = c(rnorm(n1, -5), rnorm(n2, mu_common)),
                           "b" = c(rnorm(n1, -5), rnorm(n2, mu_common))))
test_mcmc <- coda::mcmc.list(chain1, chain2)

burnin <- getBurnin(test_mcmc, threshold = 1.1)
burned <- autoburnin(test_mcmc)

test_that("Burnin value is a number and within the dimensions of `test_mcmc`", {
            expect_is(burnin, "numeric")
            expect_is(test_mcmc[burnin,], "list")
            expect_is(unlist(test_mcmc[burnin,]), "numeric")
})

test_that("Number of chains hasn't changed", {
            expect_equal(length(test_mcmc), length(burned))
})

test_that("Burned-in chains have same dimensions", {
            expect_equal(dim(burned[[1]]), dim(burned[[2]]))
})

test_that("Burned-in chains are shorter than original", {
            expect_true(coda::niter(test_mcmc) > coda::niter(burned))
})

test_that("Burnin value is where chains actually converge", {
            expect_true(burnin > n1)
})
