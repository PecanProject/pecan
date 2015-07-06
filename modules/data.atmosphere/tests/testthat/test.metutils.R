context("testing met utility functions")

test_that("sw2par, par2ppfd, sw2ppfd are consistent ",{
    expect_equal(sw2par(1000), 486)
    expect_equal(par2ppfd(486), 2068.08510638298)
    expect_equal(sw2ppfd(1000), par2ppfd(sw2par(1000)))
    expect_equal(sw2ppfd(0:1000), par2ppfd(sw2par(0:1000)))
})

test_that("qair2rh is consistent",{
#    qair2rh(qair = 1, temp = 10, press = 1013.25)
})
