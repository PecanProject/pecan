library(PEcAnRTM)
library(testthat)
context("Two stream model")

p4.pars <- defparam("prospect_4")
p5.pars <- defparam("prospect_5")
p5b.pars <- defparam("prospect_5B")
ts.pars <- c(solar.zenith = 0, LAI = 5, soil.moisture = 0.3)

nwl <- 2101

p4 <- pro2s(c(p4.pars, ts.pars), 4)
test_that("PRO4-2S works and gives physically possible output", {
              expect_is(p4, "matrix")
              expect_equal(dim(p4), c(nwl, 6))
              expect_true(all(p4) < 1)
})
p5 <- pro2s(c(p5.pars, ts.pars), 5)
test_that("PRO5-2S works and gives physically possible output", {
              expect_is(p5, "matrix")
              expect_equal(dim(p5), c(nwl, 6))
              expect_true(all(p5) < 1)
})
p5b <- pro2s(c(p5b.pars, ts.pars), "5B")
test_that("PRO5B-2S works and gives physically possible output", {
              expect_is(p5b, "matrix")
              expect_equal(dim(p5b), c(nwl, 6))
              expect_true(all(p5b) < 1)
})



