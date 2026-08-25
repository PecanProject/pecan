#!/usr/bin/env Rscript

library(arrow)
library(duckdb)

root_dir <- here::here("workflows/irrigation-statewide")
cfg <- config::get(
  file = file.path(root_dir, "config_paths.yml"),
  config = "default"
)

preprocess_dir <- cfg[["ssurgo_preprocess_dir"]]
weights_out <- cfg[["ssurgo_weights_path"]]

conn <- dbConnect(duckdb(), dbdir = ":memory:")

dbExecute(conn, "SET memory_limit = '32GB'")

dbExecute(conn, sprintf("
  COPY (
    SELECT
    *
    FROM read_parquet('%s/*.parquet')
    ORDER BY parcel_id ASC
  )
  TO '%s'
  (
    FORMAT PARQUET,
    OVERWRITE_OR_IGNORE,
    COMPRESSION 'ZSTD'
  );
  ", preprocess_dir, weights_out))

dbDisconnect(conn, shutdown = TRUE)

# Test to confirm we can open
message(
  "Testing to confirm we can open the data ",
  "and it produces valid weights."
)
dat <- open_dataset(weights_out)

dsub <- dat |>
  dplyr::filter(parcel_id %in% c(1, 100, 1000, 10000, 100000)) |>
  dplyr::collect()
print(dsub)

dat |>
  dplyr::summarize(
    wt = sum(weight),
    delta = abs(wt - 1),
    .by = "parcel_id"
  ) |>
  dplyr::arrange(dplyr::desc(delta)) |>
  dplyr::collect()
