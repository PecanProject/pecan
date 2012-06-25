test_that("time utils work",{
  eng.months  <- c("jan", "feb", "mar", "apr", "may", "jun",
                  "jul", "aug", "sep", "oct", "nov", "dec")
  port.months <- c("jan", "fev", "mar", "abr", "mai", "jun",
                   "jul", "ago", "set", "out", "nov", "dez")
  expect_equal(mon2mmm(1), 'jan')
  expect_equal(mmm2mon('jan'), 1)
  expect_equal(mon2mmm(1:12), eng.months)
  expect_equal(mon2mmm(1:12, "Portuguese"), port.months)
  expect_equal(mmm2mon(port.months, "Portuguese"), 1:12)
  expect_equal(mmm2mon(eng.months), 1:12)
})
  
