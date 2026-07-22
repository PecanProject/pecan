#!/usr/bin/env bash
# Copy HLS tile ancillary rasters from modislc to neochatt MSLP ancillary dirs.
#
# Source (modislc):
#   water:  /projectnb/modislc/data/water/hls_tiles/
#   dem:    /projectnb/modislc/data/dem/usgs_ned/hls_tiles/{dem,slope,aspect}/
#
# Destination (skanee):
#   /projectnb/dietzelab/skanee/MSLP/ancillary/{water,dem,slope,aspect}/

set -euo pipefail

SRC_WATER="/projectnb/modislc/data/water/hls_tiles"
SRC_DEM_ROOT="/projectnb/modislc/data/dem/usgs_ned/hls_tiles"
DST_ROOT="/projectnb/dietzelab/skanee/MSLP/ancillary"

mkdir -p "$DST_ROOT"/{water,dem,slope,aspect}

copy_layer() {
  local src="$1"
  local dst="$2"
  local label="$3"

  if [[ ! -d "$src" ]]; then
    echo "ERROR: source not found: $src" >&2
    exit 1
  fi
  if [[ ! -d "$dst" ]]; then
    echo "ERROR: destination not found: $dst" >&2
    exit 1
  fi
  if [[ ! -w "$dst" ]]; then
    echo "ERROR: no write permission on $dst" >&2
    exit 1
  fi

  local n_src n_dst
  n_src=$(find "$src" -maxdepth 1 -name '*.tif' | wc -l)
  n_dst=$(find "$dst" -maxdepth 1 -name '*.tif' 2>/dev/null | wc -l)

  echo "==> $label: $n_src source files, $n_dst already in destination"
  rsync -a --info=progress2 "$src/" "$dst/"
  n_dst=$(find "$dst" -maxdepth 1 -name '*.tif' | wc -l)
  echo "    done: $n_dst files in $dst"
}

echo "Copying HLS ancillary tiles from modislc to skanee..."
echo "Destination root: $DST_ROOT"
echo

copy_layer "$SRC_WATER"              "$DST_ROOT/water"  "water"
copy_layer "$SRC_DEM_ROOT/dem"       "$DST_ROOT/dem"    "dem"
copy_layer "$SRC_DEM_ROOT/slope"     "$DST_ROOT/slope"  "slope"
copy_layer "$SRC_DEM_ROOT/aspect"    "$DST_ROOT/aspect" "aspect"

echo
echo "All copies complete."
