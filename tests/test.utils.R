traits <- c("mort2", "growth_resp_factor", "leaf_turnover_rate", "leaf_width", 
            "nonlocal_dispersal", "fineroot2leaf", "root_turnover_rate", 
            "seedling_mortality", "stomatal_slope", "quantum_efficiency",
            "r_fract", "root_respiration_rate", "Vm_low_temp", "SLA", "Vcmax")


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
