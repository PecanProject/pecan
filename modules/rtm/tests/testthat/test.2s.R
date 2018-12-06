library(PEcAnRTM)
context("Two stream model")

p4.pars <- defparam("prospect_4")
p5.pars <- defparam("prospect_5")
p5b.pars <- defparam("prospect_5b")
ts.pars <- c(solar.zenith = 0, LAI = 5, soil.moisture = 0.3)

nwl <- 2101

p2s.test <- function(modname, input){
    test_that(paste(modname, "works and gives physically possible output"), {
                  expect_is(input, "matrix")
                  expect_equal(dim(input), c(nwl, 6))
                  expect_true(all(input < 1))
                  expect_true(all(input > 0))
    })
}


p4 <- pro2s(c(p4.pars, ts.pars), 4)
p2s.test("PROSPECT4-2S", p4)
p5 <- pro2s(c(p5.pars, ts.pars), 5)
p2s.test("PROSPECT5-2S", p4)
p5b <- pro2s(c(p5b.pars, ts.pars), "5B")
p2s.test("PROSPECT5B-2S", p4)



