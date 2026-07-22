#!/usr/bin/env bash
# CCMMF monitoring - portable environment template
#
# Copy to $CCMMF_ROOT/ccmmf_env.sh, edit paths, then:
#   source /path/to/ccmmf_env.sh
#
# Do NOT leave BU /projectnb defaults if you are not on SCC.

# --- Workspace (EDIT THESE) ---
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"

# PEcAn checkout: modules/data.remote/inst/ccmmf
export CCMMF_CODE="${CCMMF_CODE:-$HOME/pecan/modules/data.remote/inst/ccmmf}"

# Outputs + shared lookups (may equal a "management" tree under CCMMF_ROOT)
export CCMMF_MANAGEMENT="${CCMMF_MANAGEMENT:-$CCMMF_ROOT/management}"

# --- Years (training: new year + prior year) ---
export PRIOR_YEAR=2023
export TARGET_YEAR=2024
export YEAR_MIN=2016
export YEAR_MAX=2024
export CCMMF_TARGET_YEAR=$TARGET_YEAR

# --- LandIQ ---
# Before gap-fill: harmonized v4.1. After gap-fill: point both at the product.
export CCMMF_LANDIQ_GAPFILL_PRODUCT="$CCMMF_ROOT/LandIQ-harmonized-v4.1.2"
export CCMMF_LANDIQ_V4="${CCMMF_LANDIQ_V4:-$CCMMF_ROOT/LandIQ-harmonized-v4.1}"
# After a successful gap-fill run, switch to:
#   export CCMMF_LANDIQ_V4="$CCMMF_LANDIQ_GAPFILL_PRODUCT"
#
# BU SCC: if shared LandIQ-harmonized-v4.1 / v4.1.2 are not writable, use e.g.:
#   export CCMMF_LANDIQ_V4="$CCMMF_ROOT/management/LandIQ-harmonized-v4.1-with-${TARGET_YEAR}"
#   export CCMMF_LANDIQ_GAPFILL_PRODUCT="$CCMMF_ROOT/management/LandIQ-harmonized-v4.1.2-with-${TARGET_YEAR}"

# --- Package roots (scripts inside PEcAn inst/ccmmf or a synced management tree) ---
export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export MSLSP_EXTRACT_ROOT="${MSLSP_EXTRACT_ROOT:-$CCMMF_CODE/mslsp-extract}"
export NDTI_EXTRACT_ROOT="${NDTI_EXTRACT_ROOT:-$CCMMF_CODE/ndti-extract}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/_lib}"

# --- Phenology / match / events ---
export CCMMF_MATCHED_DIR="$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2"
export GAPFILL_MODEL_DIR="$CCMMF_MANAGEMENT/phenology/gapfill_models"

# --- HLS / MSLSP NetCDF (from HLS_Phenology workflow) ---
export HLS_IMAGERY_LAYOUT=phenology
export HLS_IMAGERY_ROOT="$CCMMF_ROOT/data_phen/HLS_data_sort/HLS30"
export mslsp_new_base="$CCMMF_ROOT/data_phen/output"
export mslsp_legacy_dir="${mslsp_legacy_dir:-$CCMMF_ROOT/HLS_data}"

# Flat layout only if needed (pre-2020 / alternate archive):
# export HLS_IMAGERY_LAYOUT=flat
# export HLSL_BASE=/path/to/State_of_California_HLSL
# export HLSS_BASE=/path/to/State_of_California_HLSS

# --- Parcel-tile map (build once after geometry exists) ---
export PARCEL_MAP="$CCMMF_MANAGEMENT/hls_parcel_tile_map_v4.1.rds"
export NDTI_PARCEL_TILEMAP="$PARCEL_MAP"
export mslsp_parcel_tilemap="$PARCEL_MAP"
export HLS_PARCEL_TILEMAP="$PARCEL_MAP"

# --- Transition matrices (full-gap CLASS fill, e.g. historical 2017) ---
# Copy Ananya county CSVs + state matrix onto your machine, then:
# export COUNTY_TRANSITION_MATRICES_DIR="$CCMMF_ROOT/data_raw/county_crop_matrices"
# export EXTERNAL_TRANSITION_MATRIX_CSV="$CCMMF_ROOT/data_raw/transition_matrix.csv"

# --- CDL ---
export CDL_DIR="$CCMMF_ROOT/CDL_data"

# --- Threads (optional) ---
export NDTI_TERRA_THREADS="${NDTI_TERRA_THREADS:-4}"

# --- Sanity ---
echo "[ccmmf_env] CCMMF_ROOT=$CCMMF_ROOT"
echo "[ccmmf_env] CCMMF_CODE=$CCMMF_CODE"
echo "[ccmmf_env] CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_V4"
echo "[ccmmf_env] TARGET_YEAR=$TARGET_YEAR PRIOR_YEAR=$PRIOR_YEAR"
