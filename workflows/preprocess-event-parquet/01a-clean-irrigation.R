#!/usr/bin/env Rscript

irr_path <- "/projectnb/dietzelab/ccmmf/usr/ashiklom/event-outputs/irrigation_all"

outdir <- "_output"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

dbdir <- tempfile("duckdb", fileext = ".duckdb")
conn <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
on.exit({
  DBI::dbDisconnect(conn, shutdown = TRUE)
  unlink(dbdir)
}, add = TRUE)

# Cast ensemble ID to an enum to accelerate and reduce the memory pressure of
# the sort.
DBI::dbExecute(conn, glue::glue("
  CREATE OR REPLACE TYPE ens_id_enum AS ENUM (
    SELECT DISTINCT ens_id FROM read_parquet('{irr_path}')
  )
  "
))

# Now, sort and write the (partitioned) parquet output
DBI::dbExecute(conn, glue::glue("
  COPY (
    SELECT
      CAST (parcel_id AS INTEGER) AS site_id,
      CAST (ens_id AS ens_id_enum) AS event_member_id,
      date,
      CAST (amount_mm AS DECIMAL(6, 2)) AS amount_mm,
      method
    FROM read_parquet('{irr_path}')
    ORDER BY event_member_id, site_id, date
  ) TO
  '{outdir}/irrigation.parquet' 
  (FORMAT PARQUET, COMPRESSION ZSTD, OVERWRITE, PARTITION_BY (event_member_id))
  "
))
