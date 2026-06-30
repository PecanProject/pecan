# look_up_ca_n_rate tests

test_that("exact match returns correct N rates in g/m2 (default)", {
  result <- look_up_ca_n_rate("Corn")
  expect_equal(nrow(result), 1)
  expect_equal(result$crop, "Corn")
  expect_equal(result$pft_group, "row")
  expect_equal(result$min_n, 16.813)
  expect_equal(result$max_n, 30.823)
  expect_true("source" %in% names(result))
})

test_that("exact match is case-insensitive", {
  result <- look_up_ca_n_rate("corn")
  expect_equal(nrow(result), 1)
  expect_equal(result$crop, "Corn")
})

test_that("unit = lbs_acre returns imperial units", {
  result <- look_up_ca_n_rate("Corn", unit = "lbs_acre")
  expect_equal(result$min_n, 150)
  expect_equal(result$max_n, 275)
})

test_that("pft_group filter works", {
  result <- look_up_ca_n_rate("Pistachios", pft_group = "woody")
  expect_equal(nrow(result), 1)
  expect_equal(result$crop, "Pistachios")

  # wrong pft_group returns empty result
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result2 <- look_up_ca_n_rate("Pistachios", pft_group = "row")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result2), 0)
})

test_that("partial match suggests crops and returns empty result", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result <- look_up_ca_n_rate("Tomato")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result), 0)
  expect_true("source" %in% names(result))
})

test_that("no match returns empty data frame with correct columns", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result <- look_up_ca_n_rate("Alfalfa")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("pft_group", "crop", "min_n", "max_n", "source"))
  expect_s3_class(result, "tbl_df")
})

test_that("source column contains citations, not urls", {
  result <- look_up_ca_n_rate("Wheat")
  expect_false(grepl("^http", result$source))
  expect_true(grepl("Rosenstock", result$source))
})

test_that("broccoli has multi-source citation", {
  result <- look_up_ca_n_rate("Broccoli")
  expect_true(grepl("Rosenstock", result$source))
  expect_true(grepl("CDFA-FREP", result$source))
})

# look_up_ca_compost_amendment tests

test_that("exact compost match returns correct properties", {
  result <- look_up_ca_compost_amendment("Poultry litter")
  expect_equal(nrow(result), 1)
  expect_equal(result$material, "Poultry litter")
  expect_equal(result$n_class, "LOWER")
  expect_true("source" %in% names(result))
})

test_that("duplicate materials return multiple rows", {
  result <- look_up_ca_compost_amendment("Cow manure")
  expect_equal(nrow(result), 2)
  expect_true(all(result$material == "Cow manure"))
  expect_equal(sort(result$source),
               sort(c("Rynk, NC State Extension", "Eghball, UNL Extension")))
})

test_that("aggregate = mean collapses duplicate materials", {
  result <- look_up_ca_compost_amendment("Cow manure", aggregate = "mean")
  expect_equal(nrow(result), 1)
  expect_equal(result$material, "Cow manure")
  # cn_avg should be mean of 22.5 and 20.0
  expect_equal(result$cn_avg, 21.25)
  # source should contain both
  expect_true(grepl("Rynk", result$source))
  expect_true(grepl("Eghball", result$source))
})

test_that("n_class filter works", {
  result <- look_up_ca_compost_amendment("Blood meal", n_class = "HIGHER")
  expect_equal(nrow(result), 1)
  expect_equal(result$n_class, "HIGHER")
})

test_that("no compost match returns empty tibble with correct columns", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result <- look_up_ca_compost_amendment("Unicorn dust")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result), 0)
  expect_true("source" %in% names(result))
  expect_s3_class(result, "tbl_df")
})

test_that("compost partial match suggests materials", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result <- look_up_ca_compost_amendment("manure")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result), 0)
})

# dataset structure tests

test_that("ca_n_application_rate dataset has expected structure", {
  dat <- PEcAn.data.land::ca_n_application_rate
  expect_equal(nrow(dat), 33)
  expect_true(all(c("pft_group", "crop", "min_n_lbs_acre", "max_n_lbs_acre",
                     "source", "min_n_g_m2", "max_n_g_m2") %in% names(dat)))
})

test_that("ca_compost_amendment dataset has expected structure", {
  dat <- PEcAn.data.land::ca_compost_amendment
  expect_equal(nrow(dat), 32)
  expect_true("source" %in% names(dat))
  expect_true(all(dat$n_class %in% c("LOWER", "HIGHER")))
})
