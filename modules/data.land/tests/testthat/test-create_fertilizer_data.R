test_that("SWAT conversion preserves compost and separates urea nitrogen", {
  script <- test_path("..", "..", "data-raw", "create_fertilizer_data.R")
  skip_if_not(file.exists(script), "SWAT regeneration script requires the source checkout")
  fixture <- test_path("testdata", "fertilizer.frt")
  read_table <- readr::read_table
  requested_source <- NULL
  local_mocked_bindings(
    read_table = function(file, ...) {
      requested_source <<- file
      read_table(fixture, ...)
    },
    .package = "readr"
  )
  local_mocked_bindings(use_data = function(...) NULL, .package = "usethis")
  env <- new.env()
  sys.source(script, env)
  expect_match(requested_source, "/refdata/Osu_1hru/fertilizer.frt$", fixed = FALSE)
  swat <- env$convert_swat_fert_table_to_pkg_df()
  urea <- swat[swat$name %in% c("urea", "46_00_00"), ]
  expect_equal(urea$fraction_urea_n, c(0.46, 0.46))
  expect_equal(urea$fraction_nh3_n, c(0, 0))
  expect_equal(urea$fraction_no3_n, c(0, 0))
  expect_equal(urea$fraction_organic_n, c(0, 0))
  expect_true(all(swat$fraction_urea_n[!swat$name %in% urea$name] == 0))
  compost <- swat[swat$name == "org_compost", ]
  expect_equal(compost$description, "OrganicCompost")
  expect_equal(compost$fraction_no3_n, 0.008)
  expect_equal(compost$fraction_organic_n, 0.012)
  expect_equal(swat$fraction_mineral_n,
               swat$fraction_nh3_n + swat$fraction_no3_n + swat$fraction_urea_n)
  expect_equal(anyDuplicated(env$fertilizer_composition_data$name), 0L)
})

test_that("packaged fertilizer data retains its inventory and nitrogen budgets", {
  dat <- fertilizer_composition_data
  expect_equal(nrow(dat), 64L)
  expect_equal(anyDuplicated(dat$name), 0L)
  expect_true(all(c("org_compost", "urea", "46_00_00", "uan_32", "can_17") %in% dat$name))
  expect_equal(dat$fraction_mineral_n,
               dat$fraction_nh3_n + dat$fraction_no3_n + dat$fraction_urea_n)
  selected <- dat[match(c("urea", "46_00_00", "uan_32", "can_17"), dat$name), ]
  expect_equal(selected$fraction_mineral_n + selected$fraction_organic_n,
               c(0.46, 0.46, 0.32, 0.17))
  expect_equal(selected$fraction_organic_n, rep(0, 4))
  expect_equal(selected$fraction_c, rep(0, 4))
})
