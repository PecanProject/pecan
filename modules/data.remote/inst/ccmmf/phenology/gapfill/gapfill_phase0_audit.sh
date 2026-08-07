#!/usr/bin/env bash
# Phase-0 audit of matched LandIQ-MSLSP assignments.
#
# Usage: ./gapfill_phase0_audit.sh
# Requires: source your documentation/setup_env.sh first.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
: "${CCMMF_MANAGEMENT:?Set CCMMF_MANAGEMENT (source documentation/setup_env.sh)}"
echo "=== gapfill phase0 audit $(date) ==="
Rscript "$ROOT/gapfill_phase0_audit.R"
echo "=== gapfill phase0 audit done $(date) ==="
