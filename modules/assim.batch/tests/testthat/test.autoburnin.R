library(PEcAn.assim.batch)
context("Autoburnin functions")

# Simple test with `line` data from `coda`
data(line)
burnin <- getBurnin(line, threshold = 1.1, method = "gelman.plot")
line_burned <- autoburnin(line, method = "gelman.plot")

test_that("Burnin value is a number and within the dimensions of `line`", {
              expect_is(burnin, "numeric")
              expect_is(line[burnin,], "list")
              expect_is(unlist(line[burnin,]), "numeric")
              expect_equal(sapply(line[burnin,], length), c('line1'=3,'line2'=3))
})

test_that("Number of chains hasn't changed", {
              expect_equal(length(line), length(line_burned))
})

test_that("Burned-in chains have same dimensions", {
              expect_equal(dim(line_burned[[1]]), dim(line_burned[[2]]))
})

test_that("Burned-in chains are shorter than original", {
              expect_true(nrow(line[[1]]) > nrow(line_burned[[1]]))
})
