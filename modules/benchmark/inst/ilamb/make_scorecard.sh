#!/bin/bash
#
# Generate the ILAMB HTML scorecard for a PEcAn benchmarking window.
#
# Runs ilamb-run against a window's model root (PEcAn, the individual CMIP6
# and TRENDY models, and the ensemble means) to produce the interactive HTML
# scorecard ILAMB writes to the build directory. This is the public-facing
# comparison view; it uses the clean model roots (no individual PEcAn members),
# so the scorecard lists PEcAn alongside the models rather than 100 member rows.
#
# Prerequisites:
#   - ILAMB installed and on PATH (ilamb-run).
#   - ILAMB_ROOT set, with the benchmark datasets under $ILAMB_ROOT/DATA.
#   - The window model root already built (see build_window_ensembles.py),
#     e.g. ilamb_models_2012_2014 / ilamb_models_2015_2023.
#
# Usage:
#   ./make_scorecard.sh <window> [model_root] [build_dir]
#
#   window      Required. A label for the run, e.g. 2012_2014 or 2015_2023.
#   model_root  Optional. Directory of model entities to score.
#               Default: ilamb_models_<window>.
#   build_dir   Optional. Output directory for the HTML scorecard.
#               Default: ilamb_scorecard_<window>.
#
# Example:
#   export ILAMB_ROOT=/path/to/ILAMB_ROOT
#   ./make_scorecard.sh 2012_2014
#   ./make_scorecard.sh 2015_2023
#
# The generated build directory (index.html plus assets) is a self-contained
# static site that can be served directly, for example by copying it under a
# web server's document root.

set -euo pipefail

WINDOW="${1:?usage: make_scorecard.sh <window> [model_root] [build_dir]}"
MODEL_ROOT="${2:-ilamb_models_${WINDOW}}"
BUILD_DIR="${3:-ilamb_scorecard_${WINDOW}}"
CONFIG="$(dirname "$0")/pecan_ilamb.cfg"

if [ -z "${ILAMB_ROOT:-}" ]; then
  echo "ERROR: ILAMB_ROOT is not set." >&2
  exit 1
fi
if [ ! -d "$MODEL_ROOT" ]; then
  echo "ERROR: model root '$MODEL_ROOT' not found. Build it first." >&2
  exit 1
fi

echo "Generating scorecard for window '$WINDOW'"
echo "  model root: $MODEL_ROOT"
echo "  build dir:  $BUILD_DIR"

ilamb-run --config "$CONFIG" \
  --model_root "$MODEL_ROOT" \
  --build_dir "$BUILD_DIR" \
  --regions global

echo "Done. Open $BUILD_DIR/index.html, or serve $BUILD_DIR as a static site."
