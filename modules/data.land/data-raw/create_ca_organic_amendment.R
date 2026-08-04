#!/usr/bin/env Rscript

# Build ca_organic_amendment_properties and ca_organic_amendment_app_rate
# from the harmonized CSVs in this folder

ca_organic_amendment_properties <- readr::read_csv(
  file.path("data-raw", "ca_organic_amendment_properties.csv"),
  show_col_types = FALSE
)

ca_organic_amendment_app_rate <- readr::read_csv(
  file.path("data-raw", "ca_organic_amendment_app_rate.csv"),
  show_col_types = FALSE
)

usethis::use_data(ca_organic_amendment_properties, overwrite = TRUE)
usethis::use_data(ca_organic_amendment_app_rate, overwrite = TRUE)
