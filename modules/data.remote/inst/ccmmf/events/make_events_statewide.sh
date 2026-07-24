#!/usr/bin/env bash
# Generate statewide event files for one year (phenology + planting + harvest by default).
#
# Usage:
#   ./make_events_statewide.sh YEAR [phenology|planting|harvest|tillage]
#   EVENT_TYPE=harvest ./make_events_statewide.sh 2024
#
# Requires: source your setup_env.sh first (CCMMF_MANAGEMENT, CCMMF_LANDIQ_V4, ...).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
YEAR="${1:-${YEAR:-}}"
TYPE="${2:-${EVENT_TYPE:-}}"
if [[ -z "$YEAR" ]]; then
  echo "Usage: $0 YEAR [phenology|planting|harvest|tillage]" >&2
  exit 1
fi
echo "=== EVENTS year=${YEAR}${TYPE:+ type=$TYPE} $(date) ==="
if [[ -n "$TYPE" ]]; then
  Rscript "$ROOT/make_events_statewide.R" "$YEAR" "$TYPE"
else
  Rscript "$ROOT/make_events_statewide.R" "$YEAR"
fi
echo "=== EVENTS year=${YEAR} done $(date) ==="
