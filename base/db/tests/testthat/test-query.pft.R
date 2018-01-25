 
context("Checking PFT lookup")

con <- db.open(
  params = list(user = "bety", password = "bety", host = "localhost"))
teardown(
  db.close(con)
)

test_that("query.pft_species finds species for a PFT", {
  one_sp <- query.pft_species(pft = "salix-miyabeana", modeltype = NULL, con)
  expect_is(one_sp, "data.frame")
  expect_equal(nrow(one_sp), 1)
  expect_equal(one_sp$id, 2871)
  expect_equal(one_sp$scientificname, "Salix miyabeana")

  multi_sp <- query.pft_species(pft = "salix", modeltype = NULL, con)
  expect_is(multi_sp, "data.frame")
  expect_gt(nrow(multi_sp), 10) # 15 spp today, but allow some change
  expect_equal(length(multi_sp$id), length(unique(multi_sp$id)))
  expect_equal(unique(multi_sp$genus), "Salix")
})

test_that("specifying modeltype removes duplicates from ambiguous query", {
  soil_null <- query.pft_species(pft = "soil", modeltype = NULL, con)
  soil_ed <- query.pft_species(pft = "soil", modeltype = "ED2", con)

  expect_lt(nrow(soil_ed), nrow(soil_null))
  expect_true(all(soil_ed$id %in% soil_null$id))
})

test_that("nonexistant PFTs and modeltypes return empty dataframes", {
  expect_length(query.pft_species("soil", "NOTAMODEL", con)$id, 0)
  expect_length(query.pft_species("NOTAPFT", NULL, con)$id, 0)
})


test_that("query.pft_cultivars finds cultivars for a PFT", {
  one_cv <- query.pft_cultivars(pft = "Pavi_9005438", modeltype = NULL, con)
  expect_is(one_cv, "data.frame")
  expect_equal(nrow(one_cv), 1)
  expect_equal(one_cv$id, 1)
  expect_equal(one_cv$specie_id, 938)
  expect_equal(one_cv$species_name, "Panicum virgatum")

  multi_cv <- query.pft_cultivars(pft = "Pavi_all", modeltype = NULL, con)
  expect_is(multi_cv, "data.frame")
  expect_gt(nrow(multi_cv), 90) # 92 spp today, but allow some change
  expect_equal(length(multi_cv$id), length(unique(multi_cv$id)))
  expect_true(one_cv$id %in% multi_cv$id)
})

test_that("query.pft_species and query.pft_cultivars do not find each other's PFTs", {
  expect_equal(nrow(query.pft_species("Pavi_9005438", NULL, con)), 0)
  expect_equal(nrow(query.pft_cultivars("soil", NULL, con)), 0)
})
