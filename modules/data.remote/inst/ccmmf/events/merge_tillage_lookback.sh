#!/usr/bin/env bash
# Fold tillage lookback amend parquets into canonical yearly products.
#
# Usage:
#   ./merge_tillage_lookback.sh [year ...]
#   Default years (if none given): 2016-2022
#
# Requires: source setup_env.sh (EVENTS_ROOT / MANAGEMENT as needed).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
echo "=== tillage lookback merge $(date) ==="
Rscript "$ROOT/merge_tillage_lookback.R" "$@"
echo "=== tillage lookback merge done $(date) ==="
