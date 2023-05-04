context("Read web config")

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
