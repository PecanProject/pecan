#!/usr/bin/env bash
# Fit phenology date models, then apply overlays for the given years.
# Does not overwrite canonical assigned_year=Y.parquet.
#
# Usage:
#   ./run_phenology_date_gapfill.sh [YEAR ...]
#   ./run_phenology_date_gapfill.sh 2023 2024
# Default years if none given: 2016-2023
#
# Requires: source your documentation/setup_env.sh first (CCMMF_MANAGEMENT, CCMMF_MATCHED_DIR, ...).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
YEARS=("$@")
if [[ ${#YEARS[@]} -eq 0 ]]; then
  YEARS=(2016 2017 2018 2019 2020 2021 2022 2023)
fi
: "${CCMMF_MANAGEMENT:?Set CCMMF_MANAGEMENT (source documentation/setup_env.sh)}"
export CCMMF_MATCHED_DIR="${CCMMF_MATCHED_DIR:-$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2}"
export GAPFILL_MODEL_DIR="${GAPFILL_MODEL_DIR:-$CCMMF_MANAGEMENT/phenology/gapfill_models}"
mkdir -p "$GAPFILL_MODEL_DIR" "$CCMMF_MATCHED_DIR/gapfill_dates"

echo "=== phenology date gapfill years=${YEARS[*]} $(date) ==="
Rscript "$ROOT/fit_phenology_gapfill_models.R"
Rscript "$ROOT/apply_phenology_gapfill.R" "${YEARS[@]}"
echo "=== phenology date gapfill done $(date) ==="
