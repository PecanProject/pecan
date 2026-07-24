#!/usr/bin/env bash
# CCMMF training environment (in your PEcAn clone)
#
#   source "$CCMMF_CODE/documentation/setup_env.sh"
# or, from this directory:
#   source ./setup_env.sh

# --- Defaults: Session 0 training (2023/2024). Override for future year pairs. ---
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
export CCMMF_CODE="${CCMMF_CODE:-$HOME/src/pecan/modules/data.remote/inst/ccmmf}"

export PRIOR_YEAR="${PRIOR_YEAR:-2023}"
export TARGET_YEAR="${TARGET_YEAR:-2024}"
export CCMMF_TARGET_YEAR=$TARGET_YEAR
export LANDIQ_GAPFILL_BOUND_MIN="${LANDIQ_GAPFILL_BOUND_MIN:-2016}"
export LANDIQ_GAPFILL_BOUND_MAX="${LANDIQ_GAPFILL_BOUND_MAX:-$TARGET_YEAR}"

# --- Derived from the roots above (override only if needed) ---
export CCMMF_MANAGEMENT="${CCMMF_MANAGEMENT:-$CCMMF_ROOT/management}"
export CCMMF_LANDIQ_V4="${CCMMF_LANDIQ_V4:-$CCMMF_ROOT/LandIQ-harmonized-v4.1}"
export CCMMF_LANDIQ_GAPFILL_PRODUCT="${CCMMF_LANDIQ_GAPFILL_PRODUCT:-$CCMMF_ROOT/LandIQ-harmonized-v4.1.2}"

export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$CCMMF_CODE/phenology}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export EVENTS_ROOT="${EVENTS_ROOT:-$CCMMF_CODE/events}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/R}"

export HLS_IMAGERY_LAYOUT=phenology
export HLS_IMAGERY_ROOT="$CCMMF_ROOT/data_phen/HLS_data_sort/HLS30"
export mslsp_new_base="$CCMMF_ROOT/data_phen/output"
export CDL_DIR="$CCMMF_ROOT/CDL_data"

export PARCEL_MAP="$CCMMF_MANAGEMENT/hls_parcel_tile_map_v4.1.rds"
export NDTI_PARCEL_TILEMAP="$PARCEL_MAP"
export mslsp_parcel_tilemap="$PARCEL_MAP"
export HLS_PARCEL_TILEMAP="$PARCEL_MAP"

export CCMMF_MATCHED_DIR="$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2"

echo "[setup_env] ROOT=$CCMMF_ROOT CODE=$CCMMF_CODE YEARS=$PRIOR_YEAR/$TARGET_YEAR"
