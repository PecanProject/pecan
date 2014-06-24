#!/bin/bash

# goto home
cd $(dirname $0)/../..
set -x

# command to connect to database
if [ "`uname -s`" != "Darwin" ]; then
  export POSTGRES="sudo -u postgres"
fi
export CMD="${POSTGRES} psql -U bety"

# Fully qualified hostname
FQDN=$( hostname -f )

# load latest dump of the database
curl -o betydump.gz https://ebi-forecast.igb.illinois.edu/pecan/dump/betydump.psql.gz

${POSTGRES} dropdb bety
${POSTGRES} createdb -O bety bety

gunzip -c betydump.gz | ${CMD} bety
rm betydump.gz

# remove old runs
sudo rm -rf output
mkdir output
chmod 777 output

# add sites
if [ -e sites/addsites.sh ]; then
  (cd sites && ./addsites.sh)
fi

# add models
$(dirname $0)/addmodels.sh
