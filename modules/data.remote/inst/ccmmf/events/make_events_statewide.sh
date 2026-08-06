#!/usr/bin/env bash
# Generate statewide event files for one year and one event type (required).
#
# Usage:
#   ./make_events_statewide.sh YEAR phenology|planting|harvest|tillage
#   EVENT_TYPE=harvest ./make_events_statewide.sh 2024
#
# Requires: source your ccmmf_env.sh first (CCMMF_MANAGEMENT, CCMMF_LANDIQ_V4, ...).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
YEAR="${1:-${YEAR:-}}"
TYPE="${2:-${EVENT_TYPE:-}}"
if [[ -z "$YEAR" || -z "$TYPE" ]]; then
  echo "Usage: $0 YEAR phenology|planting|harvest|tillage" >&2
  echo "  (or set YEAR and EVENT_TYPE in the environment)" >&2
  exit 1
fi
echo "=== EVENTS year=${YEAR} type=${TYPE} $(date) ==="
Rscript "$ROOT/make_events_statewide.R" "$YEAR" "$TYPE"
echo "=== EVENTS year=${YEAR} type=${TYPE} done $(date) ==="
