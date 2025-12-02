#!/bin/bash

# vars & window to keep small
VARS="GPP,AGB"
YEARS="2019 2020"


# input data location
# these will need to be changed to wherever the model ouptuts are
OUTPUTS=~/ccmmf/modelout/ccmmf_phase_2b_mixed_pfts_20250701/out/
# where to write fixtures inside the repo
# relative to pecan repository base dir
FIXTURES=base/utils/tests/testthat/data/ensemble_fixtures/

mkdir -p "$FIXTURES"

for d in ENS-00001-e968e9c8f8574cb2 \
         ENS-00001-ebb783e86d2ac6fb \
         ENS-00002-e968e9c8f8574cb2 \
         ENS-00002-ebb783e86d2ac6fb; do
  mkdir -p "$FIXTURES/$d"
  for y in $YEARS; do
    in="$OUTPUTS/$d/$y.nc"  # Define the input file path
    out="$FIXTURES/$d/$y.nc"
    # subset variables and time range (keeps lat=1, lon=1)
    ncks -O -4 -v $VARS,lat,lon,time,time_bounds "$in" "$out"
    # add metadata to file explaining where this came from
    ncatted -O -a history,global,o,c,"Created by make_ensemble_fixtures.sh script on $(date)" "$out"
    # compress by keeping only 3 significant digits
    ncks -O --ppc default=3 "$out" "$out"
  done
done
ls -larth */ | grep nc