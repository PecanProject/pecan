#!/usr/bin/env bash
#
# runs the fertilization statewide pipeline. FERT_PROJECT picks a profile
# from config.yml (default, medium, all).

set -euo pipefail

# run from PEcAn repo root regardless of caller cwd
cd "$(dirname "$0")/../.."

FERT_PROJECT="${FERT_PROJECT:-default}"
export FERT_PROJECT

DIR="workflows/fertilization-statewide"

Rscript "$DIR/01-build-parcel-design.R"
Rscript "$DIR/02-sample-n-rates.R"
Rscript "$DIR/03-write-parquet.R"
