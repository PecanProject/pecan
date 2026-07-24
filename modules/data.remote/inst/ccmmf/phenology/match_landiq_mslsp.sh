#!/usr/bin/env bash
# Match LandIQ seasons to MSLSP cycles for one year.
#
# Usage:
#   ./match_landiq_mslsp.sh YEAR
#   YEAR=2024 ./match_landiq_mslsp.sh
#
# Requires: source your setup_env.sh first.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$ROOT}"
YEAR="${1:-${YEAR:-}}"
if [[ -z "$YEAR" ]]; then
  echo "Usage: $0 YEAR" >&2
  exit 1
fi
export YEAR
echo "=== MATCH year=${YEAR} $(date) ==="
Rscript -e "YEAR <- as.integer(Sys.getenv('YEAR')); stopifnot(!is.na(YEAR)); source(file.path('$ROOT', 'match', 'match_landiq_mslsp.R'))"
echo "=== MATCH year=${YEAR} done $(date) ==="
