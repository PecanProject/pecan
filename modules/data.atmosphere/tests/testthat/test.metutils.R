context("testing met utility functions")

test_that("sw2par, par2ppfd, sw2ppfd are consistent ",{
    expect_equal(sw2par(1000), 486)
    expect_equal(par2ppfd(486), 2068.08510638298)
    expect_equal(sw2ppfd(1000), par2ppfd(sw2par(1000)))
    expect_equal(sw2ppfd(0:1000), par2ppfd(sw2par(0:1000)))
})

test_that("qair2rh is consistent",{
  expect_equal(qair2rh(qair = 1, temp = 10, press = 1013.25), 1)
})

test_that("get.rh RH from dewpoint",{
  # air temp fixed at 15C
  getrhtest <- function(Td_test){
    testrh <-  get.rh(T  = 273.15 + 15, 
                      Td = 273.15 + Td_test)
    return(round(testrh))# tolerance = +/- 0.5%
  }
  # air T = dewpoint
  expect_equal(getrhtest(15), 100)
  # air T < dewpoint
  expect_equal(getrhtest(20), 100)
  # air T > dewpoint
  # compared to values at NOAA calculator
  # https://www.wpc.ncep.noaa.gov/html/dewrh.shtml
  expect_equal(getrhtest(12.5), 85)
  expect_equal(getrhtest(4.6), 50)
  expect_equal(getrhtest(0), 36)
})
  
