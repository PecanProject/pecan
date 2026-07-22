#!/usr/bin/env bash
# Move HLS tile ancillary rasters from skanee to neochatt MSLP ancillary dirs.
#
# Run as neochatt after skanee has granted write on the source directories:
#   chmod o+w /projectnb/dietzelab/skanee/MSLP/ancillary/{water,dem,slope,aspect}
#
# Source (skanee):
#   /projectnb/dietzelab/skanee/MSLP/ancillary/{water,dem,slope,aspect}/
#
# Destination (neochatt):
#   /projectnb/dietzelab/neochatt/MSLP/ancillary/{water,dem,slope,aspect}/

set -euo pipefail

SRC_ROOT="/projectnb/dietzelab/skanee/MSLP/ancillary"
DST_ROOT="/projectnb/dietzelab/neochatt/MSLP/ancillary"

move_layer() {
  local layer="$1"
  local src="$SRC_ROOT/$layer"
  local dst="$DST_ROOT/$layer"

  if [[ ! -d "$src" ]]; then
    echo "ERROR: source not found: $src" >&2
    exit 1
  fi
  if [[ ! -d "$dst" ]]; then
    echo "ERROR: destination not found: $dst" >&2
    exit 1
  fi
  if [[ ! -r "$src" ]]; then
    echo "ERROR: no read permission on $src" >&2
    exit 1
  fi
  if [[ ! -w "$dst" ]]; then
    echo "ERROR: no write permission on $dst" >&2
    exit 1
  fi
  if [[ ! -w "$src" ]]; then
    echo "ERROR: no write permission on $src (needed to remove files after move)" >&2
    echo "       Ask skanee to run: chmod o+w $src" >&2
    exit 1
  fi

  local n_src n_dst
  n_src=$(find "$src" -maxdepth 1 -name '*.tif' | wc -l)
  n_dst=$(find "$dst" -maxdepth 1 -name '*.tif' 2>/dev/null | wc -l)

  echo "==> $layer: moving $n_src files ($n_dst already in destination)"
  rsync -a --remove-source-files --info=progress2 "$src/" "$dst/"
  n_dst=$(find "$dst" -maxdepth 1 -name '*.tif' | wc -l)
  n_src=$(find "$src" -maxdepth 1 -name '*.tif' 2>/dev/null | wc -l)
  echo "    done: $n_dst in destination, $n_src remaining in source"
}

echo "Moving HLS ancillary tiles from skanee to neochatt..."
echo "Source:      $SRC_ROOT"
echo "Destination: $DST_ROOT"
echo

for layer in water dem slope aspect; do
  move_layer "$layer"
done

echo
echo "All moves complete."
