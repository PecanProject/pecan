#!/usr/bin/env bash
# CCMMF env defaults. Override any var before sourcing.
#
#   source "$CCMMF_CODE/documentation/setup_env.sh"
#   source ./setup_env.sh

# Workspace roots
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"          # data
export CCMMF_CODE="${CCMMF_CODE:-$HOME/src/pecan/modules/data.remote/inst/ccmmf}"  # pipeline code

# Inventory year pair
export PRIOR_YEAR="${PRIOR_YEAR:-2023}"
export TARGET_YEAR="${TARGET_YEAR:-2024}"
export CCMMF_TARGET_YEAR=$TARGET_YEAR

# Data products ($CCMMF_ROOT)
export MANAGEMENT="${MANAGEMENT:-$CCMMF_ROOT/management}"           # outputs / lookups
export LANDIQ_ROOT="${LANDIQ_ROOT:-$CCMMF_ROOT/LandIQ}"
export LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$LANDIQ_ROOT/harmonized}"  # gap-fill input
export LANDIQ_GAPFILLED="${LANDIQ_GAPFILLED:-$LANDIQ_ROOT/gapfilled}"     # inventory product

# Remote-sensing inputs
export HLS_IMAGERY_ROOT="$CCMMF_ROOT/data_phen/HLS_data_sort/HLS30"  # HLS GeoTIFF
export MSLSP_NETCDF_ROOT="$CCMMF_ROOT/data_phen/output"             # MSLSP NetCDF
export CDL_DIR="$CCMMF_ROOT/CDL_data"                               # CDL GeoTIFF

# Lookups ($MANAGEMENT)
export HLS_PARCEL_TILEMAP="$MANAGEMENT/hls_parcel_tile_map_v4.1.csv"
export MATCHED_DIR="$MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2"

# Code components ($CCMMF_CODE)
export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$CCMMF_CODE/phenology}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export EVENTS_ROOT="${EVENTS_ROOT:-$CCMMF_CODE/events}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/R}"
export COUNTY_TRANSITION_MATRICES_DIR="${COUNTY_TRANSITION_MATRICES_DIR:-$LANDIQ_GAPFILL_ROOT/data/county_transition_matrices}"

echo "[setup_env] ROOT=$CCMMF_ROOT CODE=$CCMMF_CODE YEARS=$PRIOR_YEAR/$TARGET_YEAR"
