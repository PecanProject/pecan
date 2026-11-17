test_that("doi_to_product_name maps a known DOI to its product name", {
  expect_equal(doi_to_product_name("10.5067/HLS/HLSS30.002"), "HLS_Img")
  expect_equal(doi_to_product_name("10.5067/VIIRS/VJ215A2H.002"), "VIIRS_LAI")
})

test_that("doi_to_product_name returns an empty string for a NULL DOI", {
  expect_equal(doi_to_product_name(NULL), "")
})

test_that("doi_to_product_name returns 0 for an unknown DOI instead of failing", {
  expect_equal(doi_to_product_name("10.0000/unknown"), 0)
})
