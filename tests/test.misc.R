test_that('Misc. utility functions work'){
  expect_equivalent(get.units(1), data.frame())
  expect_equivalent(get.units('a'), data.frame())
  expect_that(get.units('Vcmax')$units, matches('umol'))
  expect_equivalent(get.units('fineroot2leaf')$units, 'ratio')

  expect_equal(vecpaste(c('a','b')), "'a','b'")
  expect_equal(vecpaste(1), "'1'")

  expect_equal(left.pad.zeros(1,2), "01")
  expect_equal(left.pad.zeros(100,2), "100")
}
