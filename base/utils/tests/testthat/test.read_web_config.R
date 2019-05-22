context("Read web config")

# `here` package needed to correctly set path relative to package
skip_if_not_installed("here")
pecan_root <- normalizePath(here::here("..", ".."))

test_that("Read example config file", {
  php_config_example <- file.path(pecan_root, "web", "config.example.php")
  cfg_example <- read_web_config(php_config_example)
  expect_equal(cfg_example[["output_folder"]], "/home/carya/output/")
  expect_equal(cfg_example[["dbfiles_folder"]], "/home/carya/output//dbfiles")
})

test_that("Read docker config file", {
  php_config_docker <- file.path(pecan_root, "docker", "web", "config.docker.php")
  cfg_docker <- read_web_config(php_config_docker)
  expect_equal(cfg_docker[["output_folder"]], "/data/workflows")
  expect_equal(cfg_docker[["dbfiles_folder"]], "/data/dbfiles")
})
