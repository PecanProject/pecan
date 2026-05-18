#!/usr/bin/env Rscript

ncc_path <- "/projectnb/dietzelab/ccmmf/usr/akash/event_files/ncc"

outdir <- "_output"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

dbdir <- file.path(Sys.getenv("TMPDIR", "/tmp"), "temp.duckdb")
conn <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))

# cast ensemble id to an enum to accelerate and reduce the memory pressure
# of the sort.
DBI::dbExecute(conn, glue::glue("
  CREATE OR REPLACE TYPE ncc_ens_id_enum AS ENUM (
    SELECT DISTINCT ens_id FROM read_parquet('{ncc_path}/*.parquet')
  )
  "
))

# sort and write the partitioned parquet output. rename parcel_id to site_id
# and ens_id to event_member_id to match the schema the json converter
# consumes.
DBI::dbExecute(conn, glue::glue("
  COPY (
    SELECT
      CAST (parcel_id AS INTEGER) AS site_id,
      CAST (ens_id AS ncc_ens_id_enum) AS event_member_id,
      date,
      material,
      CAST (org_c_kg_m2 AS DECIMAL(10, 8)) AS org_c_kg_m2,
      CAST (org_n_kg_m2 AS DECIMAL(10, 8)) AS org_n_kg_m2,
      CAST (nh4_n_kg_m2 AS DECIMAL(10, 8)) AS nh4_n_kg_m2,
      CAST (no3_n_kg_m2 AS DECIMAL(10, 8)) AS no3_n_kg_m2,
      ncc_subtype,
      crop_code,
      PFT
    FROM read_parquet('{ncc_path}/*.parquet')
    ORDER BY event_member_id, site_id, date
  ) TO
  '{outdir}/ncc.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD, OVERWRITE, PARTITION_BY (event_member_id))
  "
))
