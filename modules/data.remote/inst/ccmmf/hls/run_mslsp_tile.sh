#!/bin/bash
# One-MGRS-tile MSLSP phenology (QA mask, topo, EVI2 spline).
# Hours, not minutes -- submit with $CCMMF_SUBMIT (Session 0), do not use a login node.
#
#   source "$CCMMF_CODE/documentation/setup_env.sh"
#   export DEMO_TILE=10TEK
#   "$CCMMF_SUBMIT" -n mslsp-tile -c 4 -m 16G -t 24:00:00 -- "$0" "$DEMO_TILE"
#   # or: bash "$0" "$DEMO_TILE"
#
# Knobs (submitting shell; the job inherits them):
#   DEMO_TILE / first arg     -- MGRS id (required)
#   HLS_MSLSP_NCORES          -- default: CCMMF_JOB_CPUS / SLURM_CPUS_PER_TASK / NSLOTS / 8
#   HLS_MSLSP_NUM_CHUNKS      -- default 196
#   PRIOR_YEAR TARGET_YEAR    -- phenology years (img years = those +/- 185 days)
#   MSLSP_ALGO_ROOT HLS_IMAGERY_ROOT MSLSP_NETCDF_ROOT

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_SCRIPT="$SCRIPT_DIR/run_mslsp_tile.R"

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: missing $R_SCRIPT" >&2
  exit 1
fi

command -v Rscript >/dev/null 2>&1 || {
  echo "ERROR: Rscript not on PATH; activate your R/conda env before submit" >&2
  exit 1
}

TILE="${1:-${DEMO_TILE:-}}"
if [[ -z "$TILE" ]]; then
  echo "Usage: $0 TILE   (or export DEMO_TILE)" >&2
  exit 1
fi

: "${CCMMF_JOB_CPUS:=${SLURM_CPUS_PER_TASK:-${NSLOTS:-${PBS_NCPUS:-}}}}"
export HLS_MSLSP_NCORES="${HLS_MSLSP_NCORES:-${CCMMF_JOB_CPUS:-8}}"

echo "mslsp-tile: job=${SLURM_JOB_ID:-${JOB_ID:-${PBS_JOBID:-local}}} ncore=$HLS_MSLSP_NCORES tile=$TILE"
Rscript "$R_SCRIPT" "$TILE"
echo "mslsp-tile: done tile=$TILE"
