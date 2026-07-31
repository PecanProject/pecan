#!/usr/bin/env Rscript

# Build ca_n_application_rate from the harmonized CSV in this folder

ca_n_application_rate <- readr::read_csv(
  file.path("data-raw", "ca_n_application_rate.csv"),
  show_col_types = FALSE
)

usethis::use_data(ca_n_application_rate, overwrite = TRUE)
