# look_up_crop_pft tests

test_that("single code returns one row with both PFT columns", {
  result <- look_up_crop_pft("D", "12")
  expect_equal(nrow(result), 1)
  expect_equal(result$crop_desc, "Almonds")
  expect_equal(result$pft_group, "woody")
  expect_equal(result$pecan_pft, "temperate.deciduous")
})

test_that("vectorized lookup preserves input order", {
  result <- look_up_crop_pft(c("D", "G", "R", "P"), c("12", "2", "1", "1"))
  expect_equal(nrow(result), 4)
  expect_equal(result$crop_type, c("D", "G", "R", "P"))
  expect_equal(result$pft_group, c("woody", "row", "rice", "row"))
  expect_equal(result$pecan_pft, c("temperate.deciduous", "grass", "grass", "grass"))
})

test_that("output = pft_group drops pecan_pft column", {
  result <- look_up_crop_pft("D", "12", output = "pft_group")
  expect_equal(names(result), c("crop_type", "crop_code", "crop_desc", "pft_group"))
})

test_that("output = pecan_pft drops pft_group column", {
  result <- look_up_crop_pft("F", "6", output = "pecan_pft")
  expect_equal(names(result), c("crop_type", "crop_code", "crop_desc", "pecan_pft"))
  expect_equal(result$pecan_pft, "grass")
})

test_that("subclass override wins over class default", {
  bush_berry <- look_up_crop_pft("T", "19")
  expect_equal(bush_berry$pft_group, "woody")
  strawberry <- look_up_crop_pft("T", "20")
  expect_equal(strawberry$pft_group, "row")
})

test_that("hay subclass override works", {
  result <- look_up_crop_pft(c("G", "G"), c("1", "6"))
  expect_equal(result$pft_group, c("row", "hay"))
})

test_that("class-only lookup returns the class-level row", {
  result <- look_up_crop_pft("X")
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$crop_code))
  expect_equal(result$pft_group, "non-crop")
  expect_equal(result$pecan_pft, "soil")
})

test_that("unrecognized code returns NA and warns", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  result <- look_up_crop_pft("QQ", "99")
  PEcAn.logger::logger.setLevel(level)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$pft_group))
  expect_true(is.na(result$pecan_pft))
})

test_that("mismatched input lengths error out", {
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")
  expect_error(look_up_crop_pft(c("D", "G"), c("12", "2", "1")))
  PEcAn.logger::logger.setLevel(level)
})

test_that("carb_landiq_crop_pft dataset has expected structure", {
  dat <- PEcAn.data.land::carb_landiq_crop_pft
  expect_equal(nrow(dat), 203)
  expect_equal(
    names(dat),
    c("crop_type", "crop_code", "crop_desc", "pft_group", "pecan_pft")
  )
  expect_true(all(dat$pft_group %in%
    c("row", "woody", "rice", "hay", "idle", "semi-ag", "urban", "non-crop", NA)))
  expect_true(all(dat$pecan_pft %in%
    c("grass", "temperate.deciduous", "soil", NA)))
})
