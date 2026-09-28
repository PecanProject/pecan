#!/usr/bin/env bash
# Generate event files for one opt-in type (PRIOR+TARGET, or one year).
#
# Usage:
#   ./make_events_statewide.sh PRIOR_YEAR TARGET_YEAR phenology|planting|harvest|tillage
#   ./make_events_statewide.sh YEAR phenology|planting|harvest|tillage
#   EVENT_TYPE=harvest ./make_events_statewide.sh 2023 2024
#
# MATCHED_DIR is the input overlay (demo: .../tile=$DEMO_TILE). Event files
# write to EVENT_OUTPUT_DIR ($PRODUCTS_INVENTORY/event_files). Submit with
# $CCMMF_SUBMIT (Session 0) or run here.
#
# Requires: source your documentation/setup_env.sh first (PRODUCTS_INVENTORY, LANDIQ_GAPFILLED, ...).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
VALID="phenology|planting|harvest|tillage"
TYPE="${EVENT_TYPE:-}"
YEARS=()
for arg in "$@"; do
  if [[ "$arg" =~ ^($VALID)$ ]]; then
    TYPE="$arg"
  else
    YEARS+=("$arg")
  fi
done
if [[ -z "$TYPE" || ${#YEARS[@]} -lt 1 || ${#YEARS[@]} -gt 2 ]]; then
  echo "Usage: $0 PRIOR_YEAR TARGET_YEAR phenology|planting|harvest|tillage" >&2
  echo "   or: $0 YEAR phenology|planting|harvest|tillage" >&2
  echo "  event_type is required (no default)." >&2
  exit 1
fi
echo "=== EVENTS years=${YEARS[*]} type=${TYPE} $(date) ==="
Rscript "$ROOT/make_events_statewide.R" "${YEARS[@]}" "$TYPE"
echo "=== EVENTS years=${YEARS[*]} type=${TYPE} done $(date) ==="
