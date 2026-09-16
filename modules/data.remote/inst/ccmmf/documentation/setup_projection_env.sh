#!/usr/bin/env bash

# Paths used by the CCMMF projection workflow.
# Override any variable before sourcing if needed.

if [ -z "${CCMMF_ROOT:-}" ]; then
  echo "ERROR: CCMMF_ROOT is not set. Source documentation/setup_env.sh first."
  return 1
fi

# Where projection outputs/intermediate files are written
export PROJECTION_WORK_ROOT="${PROJECTION_WORK_ROOT:-$CCMMF_ROOT/projections}"

# Where S3 projection inputs were copied/synced locally
export PROJECTION_DATA_ROOT="${PROJECTION_DATA_ROOT:-$CCMMF_ROOT}"

# Existing CCMMF inputs reused by projection workflow
export PROJ_CROP_LOOKUP="${PROJ_CROP_LOOKUP:-$CCMMF_PFT_LOOKUP}"
export PROJ_MATCHED_PHENO_DIR="${PROJ_MATCHED_PHENO_DIR:-$CCMMF_PHEN_DIR}"
export PROJ_SSURGO_WEIGHTS="${PROJ_SSURGO_WEIGHTS:-$SSURGO_DIR/ssurgo-weights.parquet}"

# ---------- shared projection inputs ----------

# Source: s3://carb/management/crops/v4.1.2/crops_all_years.parq
export PROJ_CROPS_PATH="${PROJ_CROPS_PATH:-$PROJECTION_DATA_ROOT/management/crops/v4.1.2/crops_all_years.parq}"

# Source: s3://carb/management/planting/v2.0/
export PROJ_PLANTING_DIR="${PROJ_PLANTING_DIR:-$PROJECTION_DATA_ROOT/management/planting/v2.0}"

# Source: s3://carb/management/harvest/v2.0/
export PROJ_HARVEST_DIR="${PROJ_HARVEST_DIR:-$PROJECTION_DATA_ROOT/management/harvest/v2.0}"

# Source: s3://carb/management/phenology/v2.0/
export PROJ_PHENOLOGY_DIR="${PROJ_PHENOLOGY_DIR:-$PROJECTION_DATA_ROOT/management/phenology/v2.0}"

# Source: s3://carb/management/tillage/v2.0/
export PROJ_TILLAGE_DIR="${PROJ_TILLAGE_DIR:-$PROJECTION_DATA_ROOT/management/tillage/v2.0}"

# Source: s3://carb/management/fertilization/v2.0/
export PROJ_FERTILIZATION_DIR="${PROJ_FERTILIZATION_DIR:-$PROJECTION_DATA_ROOT/management/fertilization/v2.0}"

# Source: s3://carb/management/ncc/v2.0/
export PROJ_NCC_DIR="${PROJ_NCC_DIR:-$PROJECTION_DATA_ROOT/management/ncc/v2.0}"

# Source: s3://carb/management/session3/ssurgo/gSSURGO_CA.gdb/
export PROJ_SSURGO_GDB="${PROJ_SSURGO_GDB:-$PROJECTION_DATA_ROOT/management/session3/ssurgo/gSSURGO_CA.gdb}"

# Source: s3://carb/met/wrf_met_CA_45km_2024_2051.tgz
export PROJ_WRF_ARCHIVE="${PROJ_WRF_ARCHIVE:-$PROJECTION_DATA_ROOT/met/wrf_met_CA_45km_2024_2051.tgz}"

export PROJ_WRF_DIR="${PROJ_WRF_DIR:-$PROJECTION_DATA_ROOT/met/wrf_met_CA_45km_2024_2051}"

echo "[projection setup] WORK=$PROJECTION_WORK_ROOT"
echo "[projection setup] DATA=$PROJECTION_DATA_ROOT"
