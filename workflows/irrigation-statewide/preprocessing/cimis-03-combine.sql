-- DuckDB SQL script
-- Run with:
--    duckdb < 03-combine.sql
--
-- Or, interactively:
-- 1. Launch duckdb -- `duckdb`
-- 2. From duckdb -- `.read 03-combine.sql`

SET memory_limit = '32GB';

COPY (
  SELECT
    parcel_id,
    date,
    etref_mm_day,
    year(date) AS year
  FROM read_parquet('_results_v2/daily-raw/*.parquet')
  ORDER BY parcel_id ASC
)
TO '_results_v2/cimis-extracted'
(
  FORMAT PARQUET,
  PARTITION_BY (year),
  OVERWRITE_OR_IGNORE,
  COMPRESSION 'ZSTD'
);

