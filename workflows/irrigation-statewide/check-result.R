#!/usr/bin/env Rscript

fname <- "../../event-outputs/irrigation_all"
dat <- arrow::open_dataset(fname)

dat |>
  head(20) |>
  dplyr::collect()

dat |>
  dplyr::filter(parcel_id == 3657) |>
  dplyr::collect()

pids <- dat |>
  dplyr::distinct(.data$parcel_id) |>
  dplyr::pull()
