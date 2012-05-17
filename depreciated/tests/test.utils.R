traits <- trait.dictionary()$id

test_that('get.units works for all traits', {
  expect_true(all(traits[traits %in% get.units(traits)$name] == traits))
})

test_that('convert.samples and arrhenius scaling works', {
  expect_equivalent(convert.samples.ED(c("Vcmax" = 1)),
                    0.7052557)
  expect_equivalent(signif(convert.samples.ED(c("root_respiration_rate" = 1)),5),
                    c("root_respiration_factor" = 0.35263))
  expect_equivalent(signif(arrhenius.scaling(1,2,3),5),
                    c(1.0403))
  expect_equivalent(signif(arrhenius.scaling(1,3,2),5),
                    c(0.96129))  
})


test_that('utility functions work as expected',{

  ## pr.dens()
  expect_that(nrow(pr.dens('norm', 0, 1, n=10, alpha=0.5)),
              equals(1))
  expect_that(nrow(pr.dens('norm', 0, 10, n=10, alpha=0.5)),
              equals(1)) # function should set n=1 when alpha = 0.5 
  expect_that(nrow(pr.dens('norm', 0, 10, n=10, alpha=0.4)),
              equals(10))
  expect_that(sum(pr.dens('norm', 0, 10, n=10, alpha=0.4)$x),
              equals(0))

  ## pr.samp()
  expect_that(length(pr.samp('norm', 0, 1, 2)),
              equals(2))
  expect_that(pr.samp('norm', 0, 1, 1) < 100,
              is_true())
  

  ## get.units()
  expect_that(get.units(1),
              throws_error())
  expect_that(get.units('a'),
              throws_error())
  expect_that(get.units('SLA')$units,
              is_equivalent_to("m2 kg-1"))
  expect_that(get.units('Vcmax')$units,
              matches('umol'))
  expect_that(order(get.units(traits)$name),
              equals(order(traits)))

  ## vecpaste()
  expect_that(vecpaste(c('a','b')),
              equals("'a','b'"))
  expect_that(vecpaste(1),
              equals("'1'"))

  ## left.pad.zeros()
  expect_that(left.pad.zeros(1,2),
              equals("01"))
  expect_that(left.pad.zeros(100,2),
              equals("100"))
})

  
  
test_that("summarize.result works appropriately", {
## generate testdata  
  testresult <- data.frame(citation_id = 1,
                           site_id = 1:10,
                           name = rep(c('control', 'fert'),5),
                           control = rep(c(0,1), 5), 
                           greenhouse = c(rep(0,5), rep(1,5)),
                           date = 1,
                           time = NA,
                           cultivar_id = 1,
                           specie_id = 1,
                           n = 1,
                           mean = sqrt(1:10),
                           stat = 'none',
                           statname = 'none'
                           )
  testresult2 <- transform(testresult, site_id= 1) 
  expect_that(summarize.result(testresult)$mean, equals(testresult$mean))
  expect_that(nrow(summarize.result(testresult2)), equals(4))
})

test_that("as.sequence works";{
  expect_identical(as.sequence(c("a", "b")),
                   1:2)
  expect_identical(as.sequence(c("a", NA)),
                   1:2)
  expect_equal(as.sequence(c("a", NA), na.rm = FALSE),
               c(1,NA))
  expect_equal(as.sequence(c(NA,NA)), c(1,1))
})
