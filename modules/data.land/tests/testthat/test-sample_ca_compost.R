test_that("sample_ca_compost_material draws only from the per family whitelist", {
  set.seed(1)
  wl <- PEcAn.data.land::ca_compost_material_whitelist
  ann_allowed <- wl$material_class[wl$pft_family == "annual"]
  per_allowed <- wl$material_class[wl$pft_family == "perennial"]
  expect_true(all(PEcAn.data.land::sample_ca_compost_material("annual", 500) %in% ann_allowed))
  expect_true(all(PEcAn.data.land::sample_ca_compost_material("perennial", 500) %in% per_allowed))
})

test_that("sample_ca_compost_app_rate handles a mixed pft_family vector", {
  set.seed(1)
  fams <- rep(c("annual", "perennial"), 100)
  x <- PEcAn.data.land::sample_ca_compost_app_rate(fams)
  env <- PEcAn.data.land::ca_compost_app_rate_envelope
  ann <- env[env$pft_family == "annual", ]
  per <- env[env$pft_family == "perennial", ]
  expect_true(all(x[fams == "annual"] >= ann$min_t_ac & x[fams == "annual"] <= ann$max_t_ac))
  expect_true(all(x[fams == "perennial"] >= per$min_t_ac & x[fams == "perennial"] <= per$max_t_ac))
})

test_that("samplers fail loudly on an unknown pft_family", {
  expect_error(PEcAn.data.land::sample_ca_compost_app_rate("hay", 5))
  expect_error(PEcAn.data.land::sample_ca_compost_date_offset("rice", 5))
  expect_error(PEcAn.data.land::sample_ca_compost_material("alfalfa", 5))
})
