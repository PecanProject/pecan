
# These tests validate the refactoring to use PEcAn.utils::ud_convert()
# for consistent unit conversions across SIPNET, DALEC, GDAY, and FATES models.

test_that("UDUNITS2 recognizes new unit strings", {
  library(PEcAn.utils)
  
  # Pool conversions (mass only)
  expect_equal(ud_convert(100, "g/m2", "kg/m2"), 0.1)
  expect_equal(ud_convert(1000, "g/m2", "kg/m2"), 1.0)
  
  # Flux conversions (mass and time)
  expect_equal(ud_convert(86400, "g/m2/d", "kg/m2/s"), 0.001, tolerance = 1e-10)
  
  # Large-scale conversions (GDAY)
  expect_equal(ud_convert(10, "Mg/ha", "kg/m2"), 1.0)
  
  # Energy conversions (FATES)
  expect_equal(ud_convert(1000, "J/mol", "kJ/mol"), 1.0)
})

test_that("SIPNET flux conversions produce realistic values", {
  
  # GPP: ~5 g C/m2/d --> kg/m2/s
  expect_equal(ud_convert(5, "g/m2/d", "kg/m2/s"), 5.79e-8)
  
  # Pool: 1000 g C/m2 → 1 kg/m2
  wood_result <- ud_convert(1000, "g/m2", "kg/m2")
  expect_equal(wood_result, 1.0)
})

test_that("DALEC flux conversions are mathematically sound", {
  library(PEcAn.utils)
  
  # Autotrophic respiration: 5 g C/m2/d → kg/m2/s
  ar_result <- ud_convert(5, "g/m2/d", "kg/m2/s")
  expect_true(ar_result > 0)
  expect_true(ar_result < 5)
  
  # Leaf carbon: 200 g C/m2 → 0.2 kg/m2
  leaf_result <- ud_convert(200, "g/m2", "kg/m2")
  expect_equal(leaf_result, 0.2)
})

test_that("GDAY conversions from Mg/ha to kg/m2 work correctly", {
  library(PEcAn.utils)
  
  # 5 Mg/ha = 0.5 kg/m2
  stem_result <- ud_convert(5, "Mg/ha", "kg/m2")
  expect_equal(stem_result, 0.5)
  
  # 50 Mg/ha = 5 kg/m2
  soil_result <- ud_convert(50, "Mg/ha", "kg/m2")
  expect_equal(soil_result, 5.0)
})

test_that("No 'C' prefix in unit strings (UDUNITS2 requirement)", {
  library(PEcAn.utils)
  
  # These should NOT work (they include 'C' for carbon)
  expect_error(ud_convert(100, "gC/m2", "kgC/m2"))
  expect_error(ud_convert(86400, "gC/m2/d", "kgC/m2/s"))
  
  # But these (without 'C') should work
  expect_equal(ud_convert(100, "g/m2", "kg/m2"), 0.1)
  expect_equal(ud_convert(86400, "g/m2/d", "kg/m2/s"), 0.001, tolerance = 1e-10)
})
