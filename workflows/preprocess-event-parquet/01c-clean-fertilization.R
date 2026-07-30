#!/usr/bin/env Rscript

# the cleaner ingests outputs from the two workflows. synthetic N comes from fertilization-
# statewide as nh4 + no3 rows; compost comes from ncc-statewide as org_c +
# org_n + (optionally) nh4 from the PAN fraction. they share the ens_NNN
# naming so an ensemble member can carry both kinds of rows under one
# event_type = fertilization in the json output
#
# input paths default to where the two workflows ship their parquet
# shards but can be overridden with env vars FERT_RAW_DIR and NCC_RAW_DIR
# so a different output_dir in either workflow config does not require
# editing this file
fert_path <- Sys.getenv("FERT_RAW_DIR",
                        unset = "/projectnb/dietzelab/ccmmf/usr/akash/event_files/fertilization")
ncc_path  <- Sys.getenv("NCC_RAW_DIR",
                        unset = "/projectnb/dietzelab/ccmmf/usr/akash/event_files/ncc")

outdir <- "_output"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

dbdir <- file.path(Sys.getenv("TMPDIR", "/tmp"), "temp.duckdb")
conn <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
on.exit({
  DBI::dbDisconnect(conn, shutdown = TRUE)
  unlink(dbdir)
}, add = TRUE)

# detect what is actually on disk so cleaner is runnable in three
# cases: fert only, ncc only, both
has_fert <- length(Sys.glob(file.path(fert_path, "*.parquet"))) > 0
has_ncc  <- length(Sys.glob(file.path(ncc_path,  "*.parquet"))) > 0

if (!has_fert && !has_ncc) {
  stop("No fertilization or compost parquet shards found at ",
       fert_path, " or ", ncc_path)
}

fert_select <- glue::glue("
  SELECT
    CAST (parcel_id AS INTEGER) AS site_id,
    CAST (ens_id AS event_ens_id_enum) AS event_member_id,
    date,
    CAST (nh4_n_kg_m2 AS DECIMAL(10, 8)) AS nh4_n_kg_m2,
    CAST (no3_n_kg_m2 AS DECIMAL(10, 8)) AS no3_n_kg_m2,
    CAST (org_c_kg_m2 AS DECIMAL(10, 8)) AS org_c_kg_m2,
    CAST (org_n_kg_m2 AS DECIMAL(10, 8)) AS org_n_kg_m2,
    crop_code
  FROM read_parquet('{fert_path}/*.parquet')
")

ncc_select <- glue::glue("
  SELECT
    CAST (parcel_id AS INTEGER) AS site_id,
    CAST (ens_id AS event_ens_id_enum) AS event_member_id,
    date,
    CAST (nh4_n_kg_m2 AS DECIMAL(10, 8)) AS nh4_n_kg_m2,
    CAST (no3_n_kg_m2 AS DECIMAL(10, 8)) AS no3_n_kg_m2,
    CAST (org_c_kg_m2 AS DECIMAL(10, 8)) AS org_c_kg_m2,
    CAST (org_n_kg_m2 AS DECIMAL(10, 8)) AS org_n_kg_m2,
    crop_code
  FROM read_parquet('{ncc_path}/*.parquet')
")

ens_source <- if (has_fert && has_ncc) {
  glue::glue("
    SELECT DISTINCT ens_id FROM read_parquet('{fert_path}/*.parquet')
    UNION
    SELECT DISTINCT ens_id FROM read_parquet('{ncc_path}/*.parquet')
  ")
} else if (has_fert) {
  glue::glue("SELECT DISTINCT ens_id FROM read_parquet('{fert_path}/*.parquet')")
} else {
  glue::glue("SELECT DISTINCT ens_id FROM read_parquet('{ncc_path}/*.parquet')")
}

union_query <- if (has_fert && has_ncc) {
  paste(fert_select, "UNION ALL", ncc_select)
} else if (has_fert) {
  fert_select
} else {
  ncc_select
}

# cast ensemble id to an enum to accelerate and reduce the memory pressure
# of the sort
DBI::dbExecute(conn, glue::glue("
  CREATE OR REPLACE TYPE event_ens_id_enum AS ENUM ({ens_source})
"))

# sort and write the partitioned parquet output. rename parcel_id to site_id
# and ens_id to event_member_id to match the schema the json converter
# consumes. match compression to what the downstream arrow reader supports
# (some arrow builds ship without zstd); same guard the 03 scripts use
parquet_codec <- if (arrow::codec_is_available("zstd")) "ZSTD" else "SNAPPY"
DBI::dbExecute(conn, glue::glue("
  COPY (
    {union_query}
    ORDER BY event_member_id, site_id, date
  ) TO
  '{outdir}/fertilization.parquet'
  (FORMAT PARQUET, COMPRESSION {parquet_codec}, OVERWRITE, PARTITION_BY (event_member_id))
"))
