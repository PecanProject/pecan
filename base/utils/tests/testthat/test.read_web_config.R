context("Read web config")

test_that("comment markers inside quoted values are preserved", {
  withr::with_tempfile("config", {
    writeLines(c(
      '$api_url = "https://example.org/api/"; // service endpoint',
      '$path = "folder;//part"; // a literal separator',
      "$single = 'https://example.org/data'; // single quoted",
      '$count = 30; // numeric setting'
    ), config)

    raw <- read_web_config(config, parse = FALSE, expand = FALSE)
    expect_identical(raw$api_url, '"https://example.org/api/"')
    expect_identical(raw$path, '"folder;//part"')
    expect_identical(raw$single, "'https://example.org/data'")

    parsed <- read_web_config(config, expand = FALSE)
    expect_identical(parsed$api_url, "https://example.org/api/")
    expect_identical(parsed$path, "folder;//part")
    expect_identical(parsed$count, 30)
  })
})

php_config_example <- file.path("data", "config.example.php")

test_that("Read example config file", {
  cfg_example <- read_web_config(php_config_example)
  expect_equal(cfg_example[["output_folder"]], "/home/carya/output/")
  expect_equal(cfg_example[["dbfiles_folder"]], "/home/carya/output//dbfiles")
})

test_that("parse converts types", {
  cfg_example <- read_web_config(php_config_example, parse = FALSE)
  expect_type(cfg_example[["pagesize"]], "character")
  expect_equal(cfg_example[["pagesize"]], "30")

  cfg_example <- read_web_config(php_config_example, parse = TRUE)
  expect_type(cfg_example[["pagesize"]], "double")
  expect_equal(cfg_example[["pagesize"]], 30)
})

test_that("expand replaces output_folder", {
  cfg_example <- read_web_config(php_config_example, expand = FALSE)
  expect_equal(cfg_example[["dbfiles_folder"]], "$output_folder . /dbfiles")

  cfg_example <- read_web_config(php_config_example, expand = TRUE)
  expect_equal(cfg_example[["dbfiles_folder"]], "/home/carya/output//dbfiles")
})
