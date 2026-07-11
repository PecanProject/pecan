#!/usr/bin/env Rscript

library(arrow)
library(duckdb)

conn <- dbConnect(duckdb(), dbdir = ":memory:")

dbExecute(conn, "SET memory_limit = '32GB'")

dbExecute(conn, "
  COPY (
    SELECT
    *
    FROM read_parquet('_results/*.parquet')
    ORDER BY parcel_id ASC
  )
  TO 'ssurgo-weights.parquet'
  (
    FORMAT PARQUET,
    OVERWRITE_OR_IGNORE,
    COMPRESSION 'ZSTD'
  );
  "
)

dbDisconnect(conn, shutdown = TRUE)

# Test to confirm we can open
message(
  "Testing to confirm we can open the data ",
  "and it produces valid weights."
)
dat <- open_dataset("ssurgo-weights.parquet")

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
