#!/usr/bin/env bash
#
# runs the ncc (compost) statewide pipeline. NCC_PROJECT picks a profile
# from config.yml (default, small, medium, all).

set -euo pipefail

# run from the PEcAn repo root regardless of caller cwd.
cd "$(dirname "$0")/../.."

NCC_PROJECT="${NCC_PROJECT:-default}"
export NCC_PROJECT

DIR="workflows/ncc-statewide"

Rscript "$DIR/01-build-parcel-design.R"
Rscript "$DIR/02-sample-ncc-events.R"
Rscript "$DIR/03-write-parquet.R"
