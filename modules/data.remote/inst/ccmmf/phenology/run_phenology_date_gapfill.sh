#!/usr/bin/env bash
# Fit phenology date models, then apply overlays for the given years.
# Does not overwrite canonical assigned_year=Y.parquet.
#
# Usage:
#   ./run_phenology_date_gapfill.sh [YEAR ...]
#   ./run_phenology_date_gapfill.sh 2023 2024
# Default years if none given: 2016-2023
#
# Requires: source your documentation/setup_env.sh first (PRODUCTS_INVENTORY, MATCHED_DIR, ...).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
YEARS=("$@")
if [[ ${#YEARS[@]} -eq 0 ]]; then
  YEARS=(2016 2017 2018 2019 2020 2021 2022 2023)
fi
: "${PRODUCTS_INVENTORY:?Set PRODUCTS_INVENTORY (source documentation/setup_env.sh)}"
export MATCHED_DIR="${MATCHED_DIR:-$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2}"
export GAPFILL_MODEL_DIR="${GAPFILL_MODEL_DIR:-$PRODUCTS_INVENTORY/phenology/gapfill_models}"
mkdir -p "$GAPFILL_MODEL_DIR" "$MATCHED_DIR/gapfill_dates"

echo "=== phenology date gapfill years=${YEARS[*]} $(date) ==="
Rscript "$ROOT/fit_phenology_gapfill_models.R"
Rscript "$ROOT/apply_phenology_gapfill.R" "${YEARS[@]}"
echo "=== phenology date gapfill done $(date) ==="
